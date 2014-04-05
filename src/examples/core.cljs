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
   [frontier.cards :as cards :refer [managed-system-card input-controls-renderer]]
   [cljs.core.async :refer [chan]]
   )
  (:require-macros
   [devcards.cards :refer [is are= are-not=]]
   [devcards.macros :refer [defonce]]))

(devcards.core/start-file-reloader!)

(def todo-counter-app (make-renderable
                         (compose
                          (ExampleCounter.)
                          (ExampleTodos.))
                         (input-controls-renderer [[:inc]
                                                   [:dec]
                                                   [:create-todo {:content "do something"}]])))

(defcard managed-ex
  (managed-system-card {}
                       todo-counter-app
                       (input-controls-renderer [[:inc]
                                                 [:dec]
                                                 [:create-todo {:content "do something else"}]
                                                 ])
                       [[:inc] [:inc]]))

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

(defcard edn-card-ex 
  (edn-card (my-func 6)))

(defcard test-card-ex
  (test-card
   (is 5)
   (are= 5 4)
   (are-not= 5 4)
   (are= 5 5)
   (are-not= 5 5)
   (are-not= 5 5)
   (are-not= 5 5)))

(defn log [d]
  (.log js/console d))

(log "funneer for me you know it is")
