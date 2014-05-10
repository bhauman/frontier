(ns frontier-examples.devcards
  (:require
   [devcards.core :refer [om-card]]
   [frontier.core :refer [compose make-renderable]]
   [frontier.adaptors :refer [om-adaptor]]
   [frontier.cards :as cards :refer [system-card
                                     history-manager
                                     input-controls-renderer
                                     managed-history-card]]
   [frontier-examples.components :refer [ExampleTodos ExampleCounter]])
  (:require-macros
   [devcards.core :refer [defcard]]))

(devcards.core/start-devcard-ui!)

(devcards.core/start-figwheel-reloader!)

(defn todo-counter-app []
  (make-renderable
       (compose
        (ExampleCounter.)
        (ExampleTodos.))
       (input-controls-renderer [[:inc]
                                 [:dec]
                                 [:create-todo {:content "buy milk"}]
                                 [:create-todo {:content "buy bread"}]
                                 [:create-todo {:content "buy an F1 racer"}]])))

(defcard managed-ex
  (managed-history-card { }
                        (todo-counter-app)
                        [[:inc] [:inc]]))

(defcard omcard-frontier-ex2
    (om-card (om-adaptor (history-manager {:hello 5} (todo-counter-app))) {:hello 5}))

(defcard omcard-frontier-ex
    (om-card (om-adaptor (todo-counter-app)) {:hello 5}))
