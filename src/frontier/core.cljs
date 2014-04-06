(ns frontier.core
  (:require
   [sablono.core :as sab :include-macros true]
   [reactor.core :as rct]
   [cljs.core.async :as async
    :refer [chan put! map< close!]])
  (:require-macros
   [cljs.core.async.macros :as m :refer [go-loop]]))

(enable-console-print!)

(defn- dev-null [in]
  (go-loop [v (<! in)]
           (if (nil? v) :closed (recur (<! in)))))

(defprotocol iPluginInit
  (-initialize [_ effect-chan event-chan]))

(defprotocol iPluginStop
  (-stop [_]))

(defprotocol iTransform
  (-transform [_ msg state]))

(defprotocol iEffect
  (-effect [_ msg state event-chan effect-chan]))

(defprotocol iInputFilter
  (-filter-input [_ msg state]))

(defprotocol iDerive
  (-derive [_ state]))

(defprotocol iRenderable
  (-render [_ state]))

(defn add-effects [state & args]
  (update-in state [:__effects]
             (fn [effects]
               (concat effects args))))

(defn compose [& components]
  (let [initializers    (filter #(satisfies? iPluginInit %) components)
        stoppers        (filter #(satisfies? iPluginStop %) components)
        transforms      (filter #(satisfies? iTransform %) components)
        effects         (filter #(satisfies? iEffect %) components)
        input-filters   (filter #(satisfies? iInputFilter %) components)
        derivatives     (filter #(satisfies? iDerive %) components)
        ifilter (apply comp (mapv
                             (fn [pl]
                               (let [func (partial -filter-input pl)]
                                 (fn [[msg state]]
                                   [(func msg state) state])))
                             (reverse input-filters)))
        itrans (apply comp (mapv
                            (fn [pl]
                              (let [func (partial -transform pl)]
                                (fn [[msg state]]
                                  [msg (func msg state)])))
                            (reverse transforms)))
        ideriv (apply comp (mapv
                            (fn [pl]
                              (partial -derive pl))
                            (reverse derivatives)))        
        ieffects (fn [msg state event-chan effect-chan]
                   (doseq [pl (reverse effects)]
                     (-effect pl msg state event-chan effect-chan)))]
    (reify
      ICloneable
      (-clone [o] o)      
      iPluginInit
      (-initialize [_ state event-chan]
        (doseq [pl initializers]
          (-initialize pl state event-chan)))
      iPluginStop
      (-stop [_]
        (doseq [pl stoppers]
          (-stop pl)))      
      iTransform
      (-transform [_ msg state]
        (last (itrans [msg state])))
      iEffect
      (-effect [_ msg state event-chan effect-chan]
        (ieffects msg state event-chan effect-chan))
      iInputFilter
      (-filter-input [_ msg state]
        (first (ifilter [msg state])))
      iDerive
      (-derive [_ state]
        (ideriv state)))))

(defn make-renderable [component render-function]
  (specify component
    iRenderable
    (-render [_ state]
      (render-function state))))

(defn trans-helper* [comp* effect-handler state msg]
  (if-let [new-state (-transform comp* msg state)]
    (do
      (effect-handler (:__effects new-state))
      (-> new-state
          (dissoc :__effects)))
    state))

(defprotocol IMessageTransform
  (message-transform [runnable msg state]))

(defprotocol IStateProcess
  (state-process [runnable state]))

(defrecord RunnableSystem [component initial-state state-atom event-chan effect-chan
                           running state-callback]
  IStateProcess
  (state-process [r state] state)
  IMessageTransform
  (message-transform [r state msg]
    (trans-helper* (:component r)
                   #(doseq [ef %]
                      (put! (:effect-chan r) ef))
                   state msg)))

(defrecord DevRunnableSystem [component initial-state state-atom event-chan effect-chan
                              running state-callback]
  IStateProcess
  (state-process [r state]
    (reduce (partial trans-helper* (:component r) identity)
                                  (:intial-state r)
                                  state))
  IMessageTransform
  (message-transform [r state msg]
    (trans-helper* (:component r)
                   #(doseq [ef %]
                      (put! (:effect-chan r) ef))
                   (state-process r state) 
                   msg)
    (conj state msg)))

(defn make-runnable [component initial-state]
  (map->RunnableSystem {:component component
                        :initial-state initial-state}))

(defn coerce-to-history-based-runnable [runnable]
  (map->DevRunnableSystem runnable))

(defn initialize [{:keys [component effect-chan event-chan] :as r}]
  (-initialize component effect-chan event-chan)
  r)

(defn listen-for-effects [{:keys [component state-atom event-chan effect-chan] :as r}]
  (let [effect-chan (or effect-chan (chan))
        event-chan (or event-chan (chan))]
    (dev-null
     (map< (fn [msg]
             (-effect component msg
                      (state-process r @state-atom)
                      event-chan
                      effect-chan) true)
           effect-chan))
    (assoc r :effect-chan effect-chan)))

(defn listen-for-messages [{:keys [component state-atom event-chan effect-chan] :as r}]
  (let [event-chan (or event-chan (chan))
        effect-chan (or effect-chan (chan))]
    (dev-null
     (map< (fn [msg]
           (let [new-msg (-filter-input component msg @state-atom)]
             (swap! state-atom (partial message-transform r) new-msg)))
           event-chan))
    (assoc r
      :event-chan event-chan)))

(defn install-initial-state [{:keys [initial-state state-atom] :as r}]
  (if (nil? state-atom)
    (assoc r :state-atom (atom initial-state))
    (if (nil? @state-atom)
      (do (reset! state-atom initial-state) r) 
      r)))

(defn hook-up-state-callback [{:keys [state-callback state-atom component event-chan] :as r}]
  (when state-callback
    (add-watch state-atom
               :state-callback (fn [_ _ o n]
                                 (state-callback
                                  { :state (-derive component
                                                    (state-process r n))
                                    :event-chan event-chan }))))
  r)

(defn runner-start [runnable]
  (-> runnable
      install-initial-state
      listen-for-effects
      listen-for-messages 
      hook-up-state-callback 
      initialize
      (assoc :running true)))

(defn runner-stop [runnable]
  (when (:state-atom runnable)
    (remove-watch (:state-atom runnable) :state-callback))
  (when (:event-chan runnable)
    (close! (:event-chan runnable)))
  (when (:effect-chan runnable)
    (close! (:effect-chan runnable)))
  (-> runnable
      (assoc :event-chan nil)
      (assoc :effect-chan nil)
      (assoc :running nil)))

(defn run [initial-state component state-callback]
  (-> (make-runnable component initial-state)
      (assoc :state-callback state-callback)
      runner-start))

(defn run-with-atom [state-atom initial-state component state-callback]
  (-> (make-runnable component initial-state)
      (assoc :state-atom state-atom)
      (assoc :state-callback state-callback)
      runner-start))

(defn devrunner-start [runnable]
  (-> runnable
      coerce-to-history-based-runnable
      listen-for-effects
      listen-for-messages
      hook-up-state-callback
      initialize
      (assoc :running true)))

(defn devrunner [initial-state component state-callback]
  (-> (make-runnable component initial-state)
      (assoc :state-atom (atom []))
      (assoc :state-callback state-callback)
      devrunner-start))

(defn devrunner-with-atom [state-atom initial-state component state-callback]
  (-> (make-runnable component initial-state)
      (assoc :state-atom state-atom)
      (assoc :state-callback state-callback)
      devrunner-start))

(defn run-with-initial-inputs [initial-state
                               comp*
                               state-callback
                               initial-inputs]
  (let [system (run initial-state comp* state-callback)]
    (doseq [msg initial-inputs]
      (swap! (:state system) (partial message-transform system) msg))
    system))


