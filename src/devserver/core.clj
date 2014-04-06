(ns devserver.core
  (:require
   [compojure.route :refer [files not-found] :as route]
   [compojure.handler :refer [site api]] ; form, query params decode; cookie; session, etc
   [compojure.core :refer [defroutes GET POST DELETE ANY context routes]]
   [org.httpkit.server :refer [run-server with-channel on-close on-receive send! open?]]
   #_[clojure-watch.core :refer [start-watch]]
   [watchtower.core :refer [watcher rate ignore-dotfiles file-filter extensions on-change]]
   [clojure.core.async :refer [go-loop <!! chan put! sliding-buffer timeout map< mult tap close!
                               ]]
   [clojure.string :as string]
   [digest :as digest]
   [clojure.java.io :refer [as-file]]))

(def file-md5-cache (atom {}))

(defn file-contents-changed? [filename]
  (let [check-sum (digest/md5 (as-file filename))]
    (when (not= (@file-md5-cache filename) check-sum)
      (swap! file-md5-cache assoc filename check-sum)
      true)))

(def logger-chan (chan))
(defn log [& args] (put! logger-chan args))

(defn prnt []
  (go-loop []
           (when-let [m (<! logger-chan)]
             (println (prn-str m))
             (recur))))

;; yes i'm doing this :)
(defonce file-change-atom (atom (list)))

(defn setup-file-change-sender [wschannel]
  (let []
    (add-watch file-change-atom :message-watch
               (fn [_ _ o n]
                 (let [msg (first n)]
                   (log "sending message")
                   (log msg)
                   (when msg
                     (<!! (timeout 500))
                     (when (and (file-contents-changed? (:local-path msg))
                                (open? wschannel))
                       (send! wschannel (prn-str msg))))
                   )))
    (go-loop []
             (<! (timeout 5000))
             (when (open? wschannel)
               (send! wschannel (prn-str {:msg-name :ping}))
               (recur)
               ))))

(defn reload-handler [request]
  (with-channel request channel
    (setup-file-change-sender channel)
    (on-close channel (fn [status]
                        (log "channel closed: " status)))))

(defroutes new-routes
  (GET "/new-route-test" [] (fn [request] {:status 200
                                          :headers {"Content-Type" "text/html"}
                                          :body "Hello World"})))

(defn server [& {:keys [ring-handler]}]
  (run-server
   (routes (GET "/ws" [] reload-handler)
           (if ring-handler ring-handler (fn [r])))
   {:port 8080}))

#_(defn -main [& args]
  (run-server (site #'all-routes) {:port 8080}))

(defn server-relative-path [path idx]
  (str "/"
       (string/join "/"
                    (subvec
                     (string/split path #"\/")
                     idx))))

(defn append-msg [q msg]
  (conj (take 30 q) msg))

(defn send-changed-file [filename]
  (log filename)
  (log "putting file on channel")
  (swap! file-change-atom append-msg
         {:msg-name :file-changed
          :type :javascript
          :local-path filename
          :file (server-relative-path filename 3)}))

(defn send-changed-files [files]
  (when (> 10 (count files))
    (mapv send-changed-file (mapv #(.getPath %) files))))

(defn starts-with? [s prefix]
  (when s (zero? (.indexOf s prefix))))

(defn require-prefixes [& prefixes]
  (fn [file]
    (reduce (fn [a b] (or a b))
            (map #(starts-with? (.getPath file) %) prefixes))))

(defn ignore-prefix [prefix]
  (fn [file]
    (not ((require-prefixes prefix) file))))

(defn file-watcher [] (watcher ["./resources/public/js/compiled"]
                           (rate 500) ;; poll every 500ms
                           (file-filter ignore-dotfiles) ;; add filter
                           (file-filter (extensions :js)) ;; filter by extensions
                           (file-filter (ignore-prefix "./resources/public/js/compiled/out/goog/"))
                           (file-filter (ignore-prefix "./resources/public/js/compiled/out/clojure/"))
                           (file-filter (ignore-prefix "./resources/public/js/compiled/out/cljs/"))
                           #_(file-filter (require-prefixes "./resources/public/js/compiled/out/devcards/"
                                                          "./resources/public/js/compiled/out/frontier/"
                                                          "./resources/public/js/compiled/out/examples/"
                                                        ))

                           #_(file-filter file-contents-changed)
                           (on-change send-changed-files)))

(defn start-server [ring-handler]
  { :http-server (server :ring-handler ring-handler)
    :file-change-watcher (file-watcher)})

(defn start-static-server []
  (start-server (route/resources "/")))

(defn stop-server [{:keys [http-server file-change-watcher] :as server-data}]
  (http-server)
  (future-cancel file-change-watcher))
