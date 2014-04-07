(ns examples.core
  (:require
   [devcards.core :refer-macros [defcard]]
   [devcards.cards 
    :refer [react-card sab-card test-card edn-card]]
   [crate.core :as c]
   [sablono.core :as sab :include-macros true]
   [frontier.core :refer [compose make-renderable
                          iTransform iRenderable -transform -render
                          make-runnable
                          runner-start
                          runner-stop]]
   [frontier.example.components :refer [ExampleTodos ExampleCounter]]
   [frontier.cards :as cards :refer [managed-system-card
                                     input-controls-renderer
                                     managed-system]]
   [reactor.core :refer [render-to] :as rct]
   [frontier.util.edn-renderer :refer [html-edn]]   
   [cljs.core.async :refer [chan]]
   )
  (:require-macros
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
                                 [:decce]
                                 [:create-todo {:content "do something"}]])
       rstate)
      #_(html-edn state)])))

(defcard managed-ex
  (managed-system-card {}
                       (todo-counter-app) 
                       [[:inc] [:inc] ]))

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
