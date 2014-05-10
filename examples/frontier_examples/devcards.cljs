(ns frontier-examples.devcards
  (:require
   [devcards.core :refer [react-card sab-card test-card edn-card]]
   [devcards.system :refer [IMountable]]
   [devcards.util.edn-renderer :refer [html-edn]]
   
   [frontier.core :refer [compose make-renderable
                          ITransform IRender -transform -render -derive
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
   [frontier.cards :as cards :refer [system-card
                                     HistoryManager
                                     history-manager
                                     input-controls-renderer
                                     managed-history-card]]

   [frontier-examples.components :refer [ExampleTodos ExampleCounter]]

   [crate.core :as c]
   [sablono.core :as sab :include-macros true]
   [om.core :as om :include-macros true]

   [cljs.core.async :refer [chan close! <!]])
  (:require-macros
   [cljs.core.async.macros :refer [go-loop]]
   [devcards.core :refer [defcard is are= are-not=]]))

(devcards.core/start-devcard-ui!)

(devcards.core/start-figwheel-reloader!)

(defn todo-counter-app []
  (make-renderable
       (compose
        (ExampleCounter.)
        (ExampleTodos.))
       (input-controls-renderer [[:inc]
                                 [:dec]
                                 [:deccerter]
                                 [:create-todo {:content "do something more or less big"}]
                                 [:create-todo {:content "do something else"}]])))

(defcard managed-ex
  (managed-history-card { }
                        (todo-counter-app)
                        [[:inc] [:inc]]))

;; working with om
(defn widget [data owner]
  (reify
    om/IRender
    (render [this]
      #_(print data)
      (sab/html [:h1 "thi heh? " (:text data)]))))

(defn omcard [om-comp initial-state]
  (reify IMountable
    (mount [_ {:keys [node data]}]
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
#_(def RunnableComponent
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

(println "Aloiver")
