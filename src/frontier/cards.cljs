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
                          -derive
                          trans-helper*
                          run
                          devrunner
                          add-effects
                          compose]]
   [jayq.util :refer [log]]))

(defn can-go-forward? [{:keys [history pointer]}]
  (< pointer (count history)))

(defn can-go-back? [{:keys [pointer]}] (pos? pointer))

(defn current-state [{:keys [history pointer]}]
  (get history pointer))

(defn current-state* [history pointer {:keys [component initial-state]}]
  (-derive component
           (if (zero? pointer)
             initial-state
             (reduce (partial trans-helper* component identity)
                     initial-state
                     (subvec history 0 pointer)))))

(defmulti hist-trans first)

(defmethod hist-trans :default [_ system _] system)

(defmethod hist-trans :goto [[_ p] {:keys [history pointer] :as sys} comp*]
  (-> sys
      (assoc :pointer p)
      (assoc :render-state (current-state* history p comp*))))

(defmethod hist-trans :collect [[_ data] system comp*]
  (-> system
      (update-in [:pointer] (fn [p] (count (:new-history data))))
      (assoc-in [:history] (:new-history data))))

(defmethod hist-trans :back [_ {:keys [history pointer] :as sys} comp*]
  (if (can-go-back? sys)
    (-> sys
        (update-in [:pointer] dec)
        (assoc-in [:render-state] (current-state* history (dec pointer) comp*)))
    sys))

(defmethod hist-trans :forward [_ {:keys [history pointer] :as sys} comp*]
  (if (can-go-forward? sys)
    (-> sys
        (update-in [:pointer] inc)
        (assoc-in [:render-state] (current-state* history (inc pointer) comp*)))
    sys))

(defmethod hist-trans :keep [_ {:keys [history pointer] :as sys} _]
  (-> sys
      (add-effects [:set-state (subvec history 0 pointer)])
      (assoc :pointer (count history))
      (dissoc :render-state)))

(defmethod hist-trans :cancel [_ {:keys [history pointer] :as sys} _]
  (-> sys
      (assoc :pointer (count history))
      (dissoc :render-state)))

(defn under-control [system]
  (if (:render-state system)
    (assoc system :under-control true)
    system))

(defn can-go-forward [state]
  (if (can-go-forward? state)
    (assoc state :can-go-forward true)
    state))

(defn can-go-back [state]
  (if (can-go-back? state)
    (assoc state :can-go-back true)
    state))

(defn add-msg [state]
  (assoc state :msg (get (:history state) (dec (:pointer state)))))

(defn messages [state]
  (assoc state :messages
         (take 20 (reverse (map-indexed (fn [i x] [(inc i) x]) (:history state))))))

(defrecord HistoryManager [managed-system]
  iTransform
  (-transform [o msg system]
    (hist-trans msg system (select-keys managed-system [:component :initial-state])))
  iEffect
  (-effect [o [msg data] system event-chan effect-chan]
    (if (= :set-state msg)
      (reset! (:state managed-system) data)))
  iDerive
  (-derive [o system]
    (-> system
        under-control
        can-go-forward
        can-go-back
        add-msg
        messages)))

(defrecord SystemSetter []
  iTransform
  (-transform [o [msg data] system]
    (if (= msg :__system.set-state) data system)))

(defn managed-system [initial-state comp state-callback initial-inputs]
  (let [managed-state (atom {})
        watch (add-watch managed-state :renderer
                         (fn [_ _ _ cs]
                           (state-callback cs)))
        sys-comp (compose
                  (SystemSetter.)
                  comp)
        sys (devrunner
             initial-state
             sys-comp
             (fn [{:keys [state event-chan]}]
               (swap! managed-state
                      assoc
                      :sys-state state
                      :sys-chan event-chan)))
        history (run {}
                     (compose
                      (HistoryManager. sys))
                     (fn [{:keys [state event-chan]}]
                       (swap! managed-state
                              assoc
                              :hist-state state
                              :hist-chan event-chan)))]
    (add-watch (:state sys) :history-collect
               (fn [_ _ _ n]
                 (println "collecting stuff")
                 (put! (:event-chan history)
                       [:collect { :new-history n } ])))
    (when initial-inputs
      (doseq [msg initial-inputs]
        (swap! (:state sys) conj msg)))
    sys))

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
                    (put! hist-chan [:back]))}
        [:span.glyphicon.glyphicon-step-backward]]
       [:a.btn.btn-default.navbar-btn.disabled [:span.glyphicon.glyphicon-step-backward]])
     (if under-control
       [:a.btn.btn-default.navbar-btn
        {:className ""
         :onClick (fn [x]
                    (.preventDefault x)
                    (put! hist-chan [:cancel]))}
        [:span.glyphicon.glyphicon-stop]]
       [:a.btn.btn-default.navbar-btn.disabled [:span.glyphicon.glyphicon-stop]]) 
     (if under-control
       [:a.btn.btn-default.navbar-btn
        {:className ""
         :onClick (fn [x]
                     (.preventDefault x)
                     (put! hist-chan [:keep]))}
        [:span.glyphicon.glyphicon-download-alt]]
       [:a.btn.btn-default.navbar-btn.disabled
        [:span.glyphicon.glyphicon-download-alt]])
     (if (and under-control can-go-forward)
       [:a.btn.btn-default.navbar-btn
        {:className "right"
         :onClick (fn [x]
                    (.preventDefault x)
                    (put! hist-chan [:forward]))}
        [:span.glyphicon.glyphicon-step-forward]]
       [:a.btn.btn-default.navbar-btn.disabled
        [:span.glyphicon.glyphicon-step-forward]]
       )
     ]
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
               (put! hist-chan [:goto i]))}
            (str i " " (prn-str m))]])
        messages)
       ]]]
    [:p.navbar-text (:pointer sys) " " (prn-str msg)]
    ]
   ))

(defn managed-renderer [target-node render-func]
  (fn [{:keys [sys-state sys-chan hist-state hist-chan]}]
    (let [state (or (:render-state hist-state) sys-state)]
      (render-to (sab/html
                  [:div
                   (render-history-controls hist-state hist-chan)
                   (render-func { :state state
                                 :event-chan sys-chan }
                                :disabled (:render-state hist-state))
                   (html-edn
                    
                    (merge state { :fun  5 })
                    #_(dissoc state :__msg))])
                   target-node
                   identity))))

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
     event-chan
     :disabled disabled)))

(defn managed-system-card [initial-state component render-func initial-inputs]
  (fn [{:keys [node data]}]
    (managed-system initial-state
                    component
                    (managed-renderer node render-func)
                    initial-inputs)))
