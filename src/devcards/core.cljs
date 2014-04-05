(ns devcards.core
  (:require
   [frontier.core :refer [run]]
   [devcards.system :refer [devcard-start
                            devcard-comp
                            devcard-renderer
                            register-listeners]]
   [devserver.reloader :refer [watch-and-reload]]   
   [cljs.core.async :refer [put!]])
  (:require-macros
   [devcards.macros :refer [defonce]]))

(defonce devcard-system
      (run devcard-start
           devcard-comp
           devcard-renderer))

(defonce listeners (register-listeners "#devcards" (:event-chan devcard-system)))

(defn register-card [path tags func]
  (put! (:event-chan devcard-system)
        [:register-card {:path path :tags tags :func func}]))


(defn start-file-reloader! []
  (defonce reloading-socket
    (do
      (.on (js/$ "body") "devserverJsReload"
           (fn [e]
             (.log js/console "reload callback happening")
             (put! (:event-chan devcard-system) [:jsreload])))
      (watch-and-reload))))


