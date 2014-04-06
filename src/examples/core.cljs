(ns examples.core
  (:require
   [devcards.core :refer-macros [defcard]]
   [devcards.cards 
    :refer [react-card sab-card test-card edn-card]]
   [crate.core :as c]
   [sablono.core :as sab :include-macros true]
   [frontier.core :refer [compose make-renderable
                          iTransform iRenderable -transform -render]]
   [frontier.example.components :refer [ExampleTodos ExampleCounter]]
   [frontier.cards :as cards :refer [managed-system-card
                                     input-controls-renderer
                                     managed-system]]
   [reactor.core :refer [render-to]]
   [cljs.core.async :refer [chan]]
   )
  (:require-macros
   [devcards.cards :refer [is are= are-not=]]
   [devcards.macros :refer [defonce]]))

(devcards.core/start-file-reloader!)

(defn todo-counter-app [] (make-renderable
                           (compose
                            (ExampleCounter.)
                            (ExampleTodos.))
                           (input-controls-renderer [[:inc]
                                                     [:dec]
                                                     [:decccddd]
                                                     [:create-todo {:content "do something"}]])))


(defcard managed-ex
  (managed-system-card {}
                       todo-counter-app
                       [[:inc] [:inc]]))

#_(defcard edn-card-ex 
  (edn-card { :count 2

             :stuff #{               {:content "do omething" :id 5284589 }
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

(comment

(defcard fortunate
         (fn [{:keys [node]}]
           (.html (js/$ node) (c/html [:h3 "Hello 49ers if you like it that way "]))))

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
