(ns frontier.cards
  (:require
   [cljs.core.async :as async
    :refer [put!]]
   [reactor.core :refer [render-to raw]]
   [sablono.core :as sab :include-macros true]
   [frontier.util.edn-renderer :refer [html-edn]]
   [frontier.core :refer [iInputFilter
                          iPluginInit
                          iTransform
                          iEffect
                          iDerive
                          iRenderable
                          iPluginStop
                          -stop
                          -render
                          -derive
                          trans-helper*
                          runner-stop
                          run
                          run-with-atom
                          devrunner
                          devrunner-with-atom                          
                          add-effects
                          compose]]
   [jayq.util :refer [log]]))

(defn can-go-forward? [{:keys [pointer]} history]
  (< pointer (count history)))

(defn can-go-back? [{:keys [pointer]}] (pos? pointer))

(defn under-control? [system history]
  (not= (:pointer system ) (count history)))

(defn current-state*
  ([history pointer {:keys [component initial-state]}]
     (-derive component
              (if (zero? pointer)
                initial-state
                (reduce (partial trans-helper* component identity)
                        initial-state
                        (subvec history 0 pointer)))))
  ([history system]
     (current-state* history (count history) system)))

(defmulti hist-trans first)

(defmethod hist-trans :default [_ system _] system)

(defmethod hist-trans :history.goto [[_ p] sys _]
  (assoc sys :pointer p))

(defmethod hist-trans :history.collect [[_ data] sys _]
  (assoc sys :pointer (count (:new-history data))))

(defmethod hist-trans :history.back [_ sys _]
  (if (can-go-back? sys)
    (update-in sys [:pointer] dec)
    sys))

(defmethod hist-trans :history.forward [_ sys {:keys [history]}]
  (if (can-go-forward? sys history)
    (update-in sys [:pointer] inc)
    sys))

(defmethod hist-trans :history.keep [_ {:keys [pointer] :as sys} {:keys [history]}]
  (add-effects sys [:set-state (subvec history 0 pointer)]))

(defmethod hist-trans :history.cancel [_ sys {:keys [history]}]
  (assoc sys :pointer (count history)))

;; derivatives

(defn under-control [system history]
  (assoc system :under-control
         (under-control? system history)))

(defn render-state [system history comp*]
  (assoc system :render-stater
         (if (under-control?  system history)
           (current-state* history (:pointer system) comp*)
           (current-state* history comp*))))

(defn can-go-forward [state history]
  (assoc state :can-go-forward
         (can-go-forward? state history)))

(defn can-go-back [state]
  (assoc state :can-go-back
         (can-go-back? state)))

(defn add-msg [state history]
  (assoc state :msg (get history (dec (:pointer state)))))

(defn messages [state history]
  (assoc state :messages
         (take 20 (reverse (map-indexed (fn [i x] [(inc i) x]) history)))))

(declare render-history-controls)

(defrecord HistoryManager [managed-system]
  iPluginInit
  (-initialize [_ state event-chan]
    (add-watch (:state-atom managed-system) :managed-system-change
               (fn [_ _ o n]
                 (if (or (zero? (count o))
                         (not= (count o) (count n)))
                   (put! event-chan [:history.collect {:new-history n}])))))
  iPluginStop
  (-stop [_]
    (if (:state-atom managed-system)
      (remove-watch (:state-atom managed-system)
                    :managed-system-change))
    (-stop managed-system))
  iInputFilter
  (-filter-input [_ msg state] msg)  
  iTransform
  (-transform [o msg system]
    (let [current-system (assoc
                             (select-keys managed-system [:component :initial-state])
                           :history @(:state-atom managed-system))]
      (hist-trans msg system current-system)))
  iEffect
  (-effect [o [msg data] system event-chan effect-chan]
    (if (= :set-state msg)
      (reset! (:state-atom managed-system) data)))
  iDerive
  (-derive [o system]
    (let [history @(:state-atom managed-system)]
      (-> system
          (under-control history) 
          (can-go-forward history) 
          can-go-back
          (add-msg history)
          (messages history)
          (render-state history managed-system))))
  iRenderable
  (-render [_ {:keys [state event-chan] :as hist-state}]
    (let [derived-state (:render-stater state)]
      [:div
       (render-history-controls state event-chan)
       (-render (:component managed-system)
                { :state derived-state
                  :event-chan (:event-chan managed-system) })
       (html-edn derived-state)])))

(defn managed-system [initial-state sys-comp render-callback initial-inputs]
  (let [sys (devrunner initial-state sys-comp nil)
        history-manager (HistoryManager. sys)
        history (run {}
                     history-manager
                     (fn [{:keys [state event-chan]}]
                       (render-callback
                        (-render history-manager { :state state
                                                  :event-chan event-chan }))))]
    (when (and (zero? (count @(:state-atom sys)))
               initial-inputs)
      (doseq [msg initial-inputs]
        (swap! (:state-atom sys) conj msg)))
    sys))

(defn managed-system-with-atoms [state-atom
                                 history-manager-state-atom
                                 initial-state sys-comp render-callback initial-inputs]
  (let [sys (devrunner-with-atom state-atom initial-state sys-comp nil)
        history-manager (HistoryManager. sys)
        render-fn (fn [{:keys [state event-chan]}]
                    (render-callback
                     (-render history-manager { :state state
                                                :event-chan event-chan })))
        history (run-with-atom
                 history-manager-state-atom
                 {}
                 history-manager
                 render-fn)]
    (if (and (zero? (count @(:state-atom sys)))
             initial-inputs)
      (doseq [msg initial-inputs]
        (swap! (:state-atom sys) conj msg))
      (put! (:event-chan history) [:history.render-no-op]))
    { :system-manager history
      :system sys }))

(defn render-history-controls [{:keys [under-control can-go-back can-go-forward msg messages] :as sys} hist-chan]
  (sab/html
   [:div.navbar.navbar-default
    [:div.nav.navbar-nav.btn-group
     (if can-go-back
       [:a.btn.btn-default.navbar-btn
        {:className ""
         :href "#"
         :onClick (fn [x]
                    (.preventDefault x)
                    (put! hist-chan [:history.back]))}
        [:span.glyphicon.glyphicon-step-backward]]
       [:a.btn.btn-default.navbar-btn.disabled [:span.glyphicon.glyphicon-step-backward]])
     (if under-control
       [:a.btn.btn-default.navbar-btn
        {:className ""
         :onClick (fn [x]
                    (.preventDefault x)
                    (put! hist-chan [:history.cancel]))}
        [:span.glyphicon.glyphicon-stop]]
       [:a.btn.btn-default.navbar-btn.disabled [:span.glyphicon.glyphicon-stop]]) 
     (if under-control
       [:a.btn.btn-default.navbar-btn
        {:className ""
         :onClick (fn [x]
                     (.preventDefault x)
                     (put! hist-chan [:history.keep]))}
        [:span.glyphicon.glyphicon-download-alt]]
       [:a.btn.btn-default.navbar-btn.disabled
        [:span.glyphicon.glyphicon-download-alt]])
     (if (and under-control can-go-forward)
       [:a.btn.btn-default.navbar-btn
        {:className "right"
         :onClick (fn [x]
                    (.preventDefault x)
                    (put! hist-chan [:history.forward]))}
        [:span.glyphicon.glyphicon-step-forward]]
       [:a.btn.btn-default.navbar-btn.disabled
        [:span.glyphicon.glyphicon-step-forward]])]
    [:ul.nav.navbar-nav
     [:li.dropdown
      [:a.dropdown-toggle {:data-toggle "dropdown"} "Input history " [:b.caret]]
      [:ul.dropdown-menu
       (map
        (fn [[i m]]
          [:li
           [:a
            {:href "#"
             :onClick
             (fn [x]
               (.preventDefault x)
               (put! hist-chan [:history.goto i]))}
            (str i " " (prn-str m))]])
        messages)
       ]]]
    [:p.navbar-text (:pointer sys) " " (prn-str msg)]
    ]
   ))

(defn render-input-message-links [msgs event-chan & {:keys [disabled]}]
  [:ul
   (map (fn [x] [:li
                (if disabled
                  (prn-str x)
                  [:a
                   { :onClick (fn [] (put! event-chan x)) }
                   (prn-str x)])])
        msgs)])

(defn input-controls-renderer [input-messages]
  (fn [{:keys [event-chan]} & {:keys [disabled]}]
    (render-input-message-links
     input-messages
     event-chan)))

(defn managed-system-card [initial-state component-fn initial-inputs]
  (fn [{:keys [node data]}]
    (if-let [s (get-in @data [:system :running])]
      (runner-stop (:system @data)))
    (if-let [s (get-in @data [:system-manager :running])]
      (runner-stop (:system-manager @data)))
    (let [new-ms (managed-system-with-atoms
                  (or (get-in @data [:system :state-atom]) (atom []))
                  (or (get-in @data [:system-manager :state-atom]) (atom {}))
                  initial-state
                  (component-fn) 
                  (fn [react-dom]
                    (when react-dom
                      (render-to (sab/html react-dom) node identity)))
                  initial-inputs)]
      (reset! data new-ms))))

