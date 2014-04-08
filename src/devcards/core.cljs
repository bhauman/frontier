(ns devcards.core
  (:require
   [frontier.core :refer [run
                          make-runnable
                          runner-start]]
   [devcards.system :refer [devcard-system-start
                            devcard-renderer
                            register-listeners
                            unmount-card-nodes
                            mount-card-nodes
                            unique-card-id
                            throttle-function]]
   [devserver.reloader :refer [watch-and-reload]]   
   [cljs.core.async :refer [put! chan]])
  (:require-macros
   [devcards.macros :refer [defonce]]))

;; oh well
(defonce devcard-event-chan (chan))

(defn start-devcard-ui! []
  (defonce devcard-system
    (let [ds (devcard-system-start devcard-event-chan
                                   (throttle-function devcard-renderer 50))]
      (register-listeners "#devcards" devcard-event-chan)
      ds)))

(defn start-single-card-ui! []
  (defonce devcard-system
    (devcard-system-start devcard-event-chan
                          (throttle-function
                           (fn [{:keys [state event-chan]}]
                            (unmount-card-nodes state)
                            (mount-card-nodes state))
                           50))))

(defn start-file-reloader! []
  (defonce reloading-socket
    (do
      (.on (js/$ "body") "devserverJsReload"
           (fn [e]
             #_(.log js/console "reload callback happening")
             (put! devcard-event-chan [:jsreload])))
      (watch-and-reload))))

(defn register-card [path tags func]
  (put! devcard-event-chan
        [:register-card {:path path :tags tags :func func}]))

(defn render-single-card [card-path node]
  (let [id (unique-card-id card-path)]
    (when-not (.getElementById js/document id)
      (.html (js/$ node) (str "<div class='devcard-rendered-card' id='" id "'></div>")))))
