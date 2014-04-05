(ns frontier.core
  (:require
   [cljs.core.async :as async
    :refer [chan put! map<]])
  (:require-macros
   [cljs.core.async.macros :as m :refer [go-loop]]))

(enable-console-print!)

(defn- dev-null [in]
  (go-loop [v (<! in)]
           (if (nil? v) :closed (recur (<! in)))))

(defprotocol iPluginInit
  (-initialize [_ state event-chan]))

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

(defn run [initial-state
           comp*
           state-callback]
  (let [state (atom initial-state)
        event-chan (chan)
        effect-chan (chan)
        transformer (partial trans-helper* comp* #(doseq [ef %]
                                                   (put! effect-chan ef)))]
    
    (add-watch state :renderer (fn [_ _ o n]
                                 
                                 (state-callback { :state (-derive comp* n)
                                                  :event-chan event-chan } )

))
    
    (-initialize comp* initial-state event-chan)

    (dev-null
     (map< (fn [msg]
             (-effect comp* msg @state event-chan effect-chan) true)
           effect-chan))
    
    (dev-null
     (map< (fn [msg]
             (let [new-msg (-filter-input comp* msg @state)]
               (swap! state transformer new-msg)))
           event-chan))
    
    { :state state
     :event-chan event-chan
     :effect-chan effect-chan
     :initial-state initial-state     
     :component comp* }))

(defn run-with-initial-inputs [initial-state
                               comp*
                               state-callback
                               initial-inputs]
  (let [system (run initial-state comp* state-callback)
        trans (partial trans-helper* comp* identity)]
    (doseq [msg initial-inputs]
      (swap! (:state system) trans msg))
    system))

(defn devrunner [initial-state comp* state-callback]
  (let [state (atom [])
        event-chan (chan)
        effect-chan (chan)
        transformer (partial trans-helper* comp* #(doseq [ef %]
                                                    (put! effect-chan ef)))]
    
    (add-watch state :renderer (fn [_ _ o n]
                                 (let [prev-state (reduce (partial trans-helper* comp* identity)
                                                          initial-state
                                                          (butlast n))
                                       cur-state  (transformer prev-state (last n))]
                                   (state-callback { :state (-derive comp* cur-state)
                                                     :event-chan event-chan }))))
    
    (-initialize comp* initial-state event-chan)
    
    (dev-null
     (map< (fn [msg]
             (-effect comp* msg @state event-chan effect-chan) true)
           effect-chan))
    
    (dev-null
     (map< (fn [msg]
             (println msg)
             (println state)
             (let [new-msg (-filter-input comp* msg @state)]
               (swap! state conj new-msg)))
           event-chan))
    
    { :state state
      :event-chan event-chan
      :effect-chan effect-chan
      :initial-state initial-state
      :component comp* }))

