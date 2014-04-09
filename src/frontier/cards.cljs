(ns frontier.cards
  (:require
   [cljs.core.async :as async
    :refer [put!]]
   [clojure.string :as string]
   [reactor.core :refer [render-to raw]]
   [sablono.core :as sab :include-macros true]
   [devcards.util.edn-renderer :refer [html-edn]]
   [devcards.system :refer [IMountable]]
   [frontier.core :refer [iInputFilter
                          iPluginInit
                          iTransform
                          iEffect
                          iDerive
                          iRenderable
                          iPluginStop
                          HistoryKeeper
                          -stop
                          -render
                          -derive
                          -effect
                          -transform
                          -initialize
                          -filter-input
                          trans-helper*
                          runner-stop
                          run-with-atom
                          add-effects
                          compose
                          move-effects-to-top]]
   [jayq.util :refer [log]]))

(defn can-go-forward? [{:keys [pointer]} history]
  (< pointer (count history)))

(defn can-go-back? [{:keys [pointer]}] (pos? pointer))

(defn under-control? [system history]
  (not= (:pointer system ) (count history)))

(defn current-state*
  ([{:keys [initial-state history] :as virt-state } pointer component]
     (-derive component
                      (if (zero? pointer)
                        {:__history-keeper {:state initial-state
                                     :initial-state initial-state
                                     :history [] }}
                        (reduce (partial trans-helper* component identity)
                                initial-state
                                (subvec history
                                        0 pointer)))))
  ([{:keys [history] :as virt-state} component]
     (current-state* virt-state (count history) component)))

(defmulti hist-trans first)

(defmethod hist-trans :default [_ system _] system)

(defmethod hist-trans :history.goto [[_ p] sys _]
  (assoc-in sys :pointer p))

(defmethod hist-trans :history.back [_ sys _]
  (if (can-go-back? sys)
    (update-in sys [:pointer] dec)
    sys))

(defmethod hist-trans :history.forward [_ sys {:keys [history]}]
  (if (can-go-forward? sys history)
    (update-in sys [:pointer] inc)
    sys))

(defmethod hist-trans :history.keep [_ {:keys [pointer] :as sys} {:keys [history]}]
  (add-effects sys [:history.set-state (subvec history 0 pointer)]))

(defmethod hist-trans :history.cancel [_ sys {:keys [history]}]
  (assoc sys :pointer (count history)))

;; derivatives

(defn under-control [system history]
  (assoc system :under-control
         (under-control? system history)))

(defn render-state [hist-state virt-state comp*]
  (assoc hist-state :render-stater
         (if (under-control? hist-state (:history virt-state))
           (current-state* virt-state (:pointer hist-state) comp*)
           (current-state* virt-state comp*))))

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

(defn history-message? [msg]
  (-> (first msg)
      name
      (string/split #"\.")
      first
      (= "history")))

(defrecord HistoryManager [virtual-system]
  iPluginInit
  (-initialize [_ state event-chan]
    (-initialize virtual-system state event-chan))
  iPluginStop
  (-stop [_]
    (-stop virtual-system))
  iInputFilter
  (-filter-input [_ msg state]
    (-filter-input virtual-system msg state))
  iTransform
  (-transform [o msg state]
    (if (history-message? msg)
      (move-effects-to-top
       [:__history-manager]
       (update-in state [:__history-manager]
                  (fn [hist-state]
                    (hist-trans msg hist-state (:__history-keeper state)))))
      (let [new-state (-transform virtual-system msg state)]
        (assoc-in
         new-state
         [:__history-manager :pointer]
         (count (get-in new-state [:__history-keeper :history]))))))
  iEffect
  (-effect [o msg state event-chan effect-chan]
    (if (= (first msg) :history.set-state)
      (put! event-chan [:__history-keeper.set-history (second msg)])
      (-effect virtual-system msg state event-chan effect-chan)))
  
  iDerive
  (-derive [o state]
    (let [history (get-in state [:__history-keeper :history])]
      (update-in state
                 [:__history-manager]
                 (fn [hist-state]
                   (-> hist-state
                       (under-control history) 
                       (can-go-forward history) 
                       can-go-back
                       (add-msg history)
                       (messages history)
                       (render-state (:__history-keeper state) virtual-system))))))
  iRenderable
  (-render [_ {:keys [state event-chan] :as hist-state}]
    (let [derived-state (get-in state [:__history-manager :render-stater])]
      [:div
       (render-history-controls (:__history-manager state) event-chan)
       (-render virtual-system
                { :state derived-state
                  :event-chan event-chan })
       (html-edn (get-in derived-state [:__history-keeper :state]))])))

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

(defn system-card [initial-state component initial-inputs]
  (reify
    IMountable
    (mount [_ {:keys [node data]}]
      (let [sys (run-with-atom
                 (or (:state-atom @data) (atom {})) 
                 {}
                 component
                 (fn [state]
                   (when-let [react-dom (-render component state)]
                     (render-to (sab/html react-dom) node identity))))]
        (if (and (= {} @(:state-atom sys))
                 initial-inputs)
          (doseq [msg initial-inputs]
            (put! (:event-chan sys) msg))
          (put! (:event-chan sys) [:history.render-no-op]))
        (reset! data sys)))
    (unmount [_ {:keys [node data]}]
      (when (:running @data)
        (reset! data (runner-stop @data)))
      (.unmountComponentAtNode js/React node))))

(defn managed-history-card [initial-state component initial-inputs]
  (system-card {}
               (HistoryManager.
                (HistoryKeeper.
                 component
                 {}))
               [[:inc] [:inc]]))
