(ns examples.core
  (:require
   [devcards.core :refer-macros [defcard hidecard]]
   [devcards.system :refer [IMountable]]
   [devcards.cards 
    :refer [react-card sab-card test-card edn-card]]
   [crate.core :as c]
   [sablono.core :as sab :include-macros true]
   [frontier.core :refer [compose make-renderable
                          iTransform iRenderable -transform -render -derive
                          -filter-input
                          -initialize
                          -stop
                          -effect
                          transform-with-effects
                          HistoryKeeper
                          Namespacer                          
                          make-runnable
                          runner-start
                          runner-stop]]
   [frontier.adaptors :refer [om-adaptor]]
   [frontier.example.components :refer [ExampleTodos ExampleCounter]]
   [frontier.cards :as cards :refer [system-card
                                     HistoryManager
                                     history-manager
                                     input-controls-renderer
                                     managed-history-card]]
   [reactor.core :refer [render-to] :as rct]
   [devcards.util.edn-renderer :refer [html-edn]]   
   [cljs.core.async :refer [chan close! <!]]
   [om.core :as om :include-macros true]
   [examples.sparse])
  (:require-macros
   [cljs.core.async.macros :refer [go-loop]]
   [devcards.cards :refer [is are= are-not=]]
   [devcards.macros :refer [defonce]]))

(devcards.core/start-devcard-ui!)

#_(devcards.core/start-single-card-ui!)

(devcards.core/start-file-reloader!)

(defn todo-counter-app []
  (make-renderable
       (compose
        (ExampleCounter.)
        (ExampleTodos.))
       (fn [{:keys [state event-chan] :as rstate}]
         [:div
          ((input-controls-renderer [[:inc]
                                     [:dec]
                                     [:deccerert]
                                     [:create-todo {:content "do something"}]])
           rstate)
          #_(html-edn state)])))



(defcard managed-ex
  (managed-history-card { :strange {:money { } }}
                        (Namespacer. [:strange :money]
                                     (todo-counter-app))
                        #_[]
                        #_[[:inc] [:inc]]
                        [[[:strange :money :inc] nil]
                         [[:strange :money :inc] nil]]))

#_(defcard new-history-keeper
  (system-card {}
               (HistoryManager.
                (Namespacer. :__history-keeper
                             (HistoryKeeper.
                              (Namespacer. :state (todo-counter-app))
                              {})))
               [[[:__history-keeper :state :inc]]
                [[:__history-keeper :state :inc]]]))

;; working with om
(defn widget [data owner]
  (reify
    om/IRender
    (render [this]
      (print data)
      (sab/html [:h1 "thi heh? " (:text data)]))))

(defn omcard [om-comp initial-state]
  (reify IMountable
    (mount [_ {:keys [node data]}]
      (print "looking at data")
      (print data)
      (when (or (nil? @data)
                (= {} @data))
        (reset! data initial-state))
      (om/root om-comp data {:target node}))
    (unmount [_ {:keys [node]}]
      (.unmountComponentAtNode js/React node))))

(defcard omcard-frontier-ex2
    (omcard (om-adaptor (history-manager {:hello 5} (todo-counter-app))) {:hello 5}))

(defcard omcard-frontier-ex
    (omcard (om-adaptor (todo-counter-app)) {:hello 5}))

(defcard omcard-ex
    (omcard widget {}))

#_(defn om-comper [cursor owner]
  (print cursor)
  (reify
    om/IRender
    (render [this]
      (sab/html [:div
                 (om/build
                  #_widget
                  (om-frontier-comp (todo-counter-app) { :hello 5 })
                  (:fun cursor))]))))


#_(let [el (.getElementById js/document "main-area")]
  (om/root
   #_om-comper
   (om-frontier-comp (todo-counter-app) { :hello 5 })
   (atom { :fun {:times {}}})
   {:target el}))


;; trying react
(def RunnableComponent
  (let [runnable (fn [this]
                   (rct/get-prop-val this "runnable"))]
    (.createClass
     js/React
     (js-obj
      "componentWillMount"
      (fn []
        (this-as this
                 (print "calling component will MOUNT")
                 (let [running (-> (runnable this)
                                   (assoc :state-callback
                                     (fn [rstate]
                                       (.setState this
                                                  (js-obj "runnable-state"
                                                          rstate))))
                                   runner-start)]
                   ;; set starting state
                   (.setState this
                              (js-obj "running-runnable"
                                      running
                                      "runnable-state"
                                      { :event-chan (:event-chan running)
                                        :state @(:state-atom running)})))))
      "componentWillUnmount"
      (fn []
        (this-as this
                 (print "calling component will UNNNMOUNT")
                 (when-let [running (aget (.-state this) "running-runnable")]
                   (runner-stop running))))
      "render"
      (fn []
        (this-as this
                 (.log js/console this)
                 (sab/html (-render (:component (runnable this))
                                      (aget (.-state this) "runnable-state")))))))))

#_(defonce state-atom (atom nil))

#_(defonce running-component (RunnableComponent. (js-obj "runnable"
                                                       (assoc (make-runnable (todo-counter-app) { :hello 1 })
                                                         :state-atom 
                                                         state-atom))))

#_(render-to
 #_running-component
 (sab/html [:h1 "hi"]) 
 #_(RunnableComponent. (js-obj "runnable"
                             (assoc (make-runnable (todo-counter-app) { :hello 1 })
                               :state-atom 
                               state-atom)))
 (.getElementById js/document "main-area") identity)


(defcard fortunate
         (fn [{:keys [node]}]
           (.html (js/$ node) (c/html [:h3 "Hello 49ers are you there"]))))

(defcard edn-card-ex 
  (edn-card { :count 134

             :stuff #{
                      {:content "do something" :id 5284589 }
                      {:content "do something" :id 6449664 }
                      {:content "do something" :id 4739498 }
                      {:content "do something" :id 4486982 }}
             :todos
             
             [
              {:content "do omething" :id 5284589 }
              {:content "do something" :id 6449664 }
              {:content "do something" :id 4739498 }
              {:content "do something" :id 4486982 }
              {:content "do something" :id 9475352 }
              {:content "do something" :id 2498755 }
              {:content "do something" :id 47767 }
              {:content "do something" :id 3415408 }
              {:content "do something" :id 9156329 }
              ] :double 4}))

#_(devcards.core/render-single-card
   [:examples.core :edn-card-ex]
   (.getElementById js/document "main-area"))


#_(defrecord NsTester []
  iInputFilter
  (-filter-input [_ [msg-name data] state]
    (if (= :doit msg-name)
      [:darn-it data]
      [msg-name data]))
  iTransform
  (-transform [o msg state]
    (if (= (first msg) :hello)
      (-> state
          (assoc :count (+ (:count state) (:add (last msg))))
          (add-effects [:wowzers {}]))
      state))
  iEffect
  (-effect [o msg state event-chan effect-chan]
    (put! event-chan [:msg-c  msg] )
    (put! effect-chan [:state-c state]))
  iDerive
  (-derive [o state]
    (assoc state
      :double (* 2 (:count state)))))

#_(def o (Namespacer.
        :myname
        (make-renderable
         (compose
          (NsTester.)
          )
         (fn [{:keys [state event-chan]}]
           (put! event-chan [:hello {:dd :top}])
           [:div state]))
        ))

#_(print (-filter-input o [[:myname :doit] {:fiver 1}] {}))


#_(print "" (-transform o [[:myname :hello] {:add 4}] {}))

#_(print (-derive o {:myname {:count 5}}))

#_(go
 (let [ev (chan)]
   (print (-render o
            {:state {:myname {:counterific 2}}
             :event-chan ev}))
   (print "ev-render " (<! ev))) )

#_(let [c (chan)
      scoped-chan (ns-scoped-channel :myname c)]
  (put! scoped-chan [:hello {:add 40}])
  (go
   (print (<! c)))  
  )

#_(go
 (let [ev (chan)
       ef (chan)]
   (-effect o
            [[:mynamer :creep :effect-name] {:data 1}]
            {:myname {:counterific 2}}
            ev ef)
   (print "ev" (<! ev))
   (print "ef" (<! ef))) )

(comment

(defcard fortunater
  (fn [{:keys [node data position]}]
    (.html (js/$ node) (c/html [:h3 "Hello 49ers if you like stuff like that yeah "]))))

(defcard react-card-ex
  (react-card (sab/html [:div [:h3 "another test is here can you beleive"]])))

(defcard sab-card-ex
  (sab-card [:div [:h4 "another test is here and is working"]]))

(defn my-func [r]
  {:johhny 36 :marco 95 :fun "never enough" :r r})

(defcard test-card-ex
  (test-card
   (is 5)
   (are= 5 4)
   (are-not= 5 4)
   (are= 5 5)
   (are-not= 5 5)
   (are-not= 5 5)
   (are-not= 5 6))))

(defn log [d]
  (.log js/console d))

(log "funneer for me you know it is")

