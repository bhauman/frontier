(ns devserver.reloader
  (:require
   [goog.net.jsloader :as loader]
   [clojure.string :as string]
   [clojure.set :refer [union]]
   [cljs.reader :refer [read-string]]))

(defn log [d]
  (.log js/console (clj->js d)))

(def namespaces-to-watch (atom #{}))

(def namespaces-to-ignore (atom #{}))

(defn ^:export watch-namespaces [& namespaces]
  (swap! namespaces-to-watch union (set (map name namespaces))))

(defn ^:export reset-watched-namespaces []
  (reset! namespaces-to-watch #{}))

(log @namespaces-to-watch)

(defn path-to-ns [url]
  (let [no-ext (string/replace url #"\.js$" "")
        parts (string/split no-ext "/")]
    (string/join "." (subvec parts 4))))

(defn should-reload? [url]
  (let [ns-str (path-to-ns url)
        provided? (.isProvided_ js/goog ns-str)]
    (if (pos? (count @namespaces-to-watch))
     (and provided?
          (@namespaces-to-watch ns-str))
     provided?)))

(defn js-reload [url]
  (when (should-reload? url)
    (.log js/console "reloading javascript file: " url)
    (let [deferred (loader/load url)]
      (.addCallback deferred
                    (fn [] (.trigger (js/$ "body") "devserverJsReload"))))))

(defn watch-and-reload []
  (set! js/COMPILED true)
  (.log js/console "opening cljs reload socket")
  (let [socket (js/WebSocket. "ws:localhost:8080/ws")]
    (set! (.-onmessage socket) (fn [msg-str]
                                 #_(.log js/console msg-str)
                                 #_(.log js/console (read-string (.-data msg-str)))
                                 (let [msg (read-string (.-data msg-str))]
                                     (js-reload (:file msg)))))))
