(ns devserver.core
  (:require
   [compojure.route :refer [files not-found] :as route]
   [compojure.handler :refer [site api]] ; form, query params decode; cookie; session, etc
   [compojure.core :refer [defroutes GET POST DELETE ANY context routes]]
   [org.httpkit.server :refer [run-server with-channel on-close on-receive send! open?]]
   #_[clojure-watch.core :refer [start-watch]]
   [watchtower.core :as wt :refer [watcher compile-watcher watcher* rate ignore-dotfiles file-filter extensions on-change]]
   [clojure.core.async :refer [go-loop <!! chan put! sliding-buffer timeout map< mult tap close!
                               ]]
   [clojure.string :as string]
   [digest :as digest]
   [clojure.java.io :refer [as-file]]
   [fs.core :as fs]))

(defn file-contents-changed? [{:keys [file-md5-cache] :as st} filename]
  (let [check-sum (digest/md5 (as-file filename))]
    (when (not= (@file-md5-cache
                 filename) check-sum)
      (swap! file-md5-cache
             assoc filename
             check-sum)
      true)))

(defn log [{:keys [logger-chan]} & args]
  (put! logger-chan args))

(defn print-logs [{:keys [logger-chan]}]
  (go-loop []
           (when-let [m (<! logger-chan)]
             (println (prn-str m))
             (recur))))

(defn setup-file-change-sender [{:keys [file-change-atom compile-wait-time] :as server-state}
                                wschannel]
  (add-watch file-change-atom
             :message-watch
             (fn [_ _ o n]
               (let [msg (first n)]
                 (log server-state "sending message")
                 (log server-state msg)
                 (when msg
                   (<!! (timeout @compile-wait-time))
                   (when (and (file-contents-changed? server-state (:local-path msg))
                              (open? wschannel))
                     (send! wschannel (prn-str msg)))))))
  
  ;; Keep alive!!
  (go-loop []
           (<! (timeout 5000))
           (when (open? wschannel)
             (send! wschannel (prn-str {:msg-name :ping}))
             (recur))))

(defn reload-handler [server-state]
  (fn [request]
    (with-channel request channel
      (setup-file-change-sender server-state channel)
      (on-close channel (fn [status]
                          (log server-state "channel closed: " status))))))

(defroutes new-routes
  (GET "/new-route-test" [] (fn [request] {:status 200
                                          :headers {"Content-Type" "text/html"}
                                          :body "Hello World"})))

(defn server [{:keys [ring-handler] :as server-state}]
  (run-server
   (if ring-handler
     (routes (GET "/ws" [] (reload-handler server-state)) ring-handler)
     (routes (GET "/ws" [] (reload-handler server-state))))
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

(defn send-changed-file [{:keys [file-change-atom] :as st} filename]
  (log st filename)
  (log st "putting file on channel")
  (swap! file-change-atom
         append-msg
         {:msg-name :file-changed
          :type :javascript
          :local-path filename
          ;; this assumes /resources/public
          :file (server-relative-path filename 3)})) 

(defn send-changed-files [server-state files]
  (mapv (partial send-changed-file server-state)
        (mapv #(.getPath %) files)))

(defn starts-with? [s prefix]
  (when s (zero? (.indexOf s prefix))))

(defn require-prefixes [& prefixes]
  (fn [file]
    (reduce (fn [a b] (or a b))
            (map #(starts-with? (.getPath file) %) prefixes))))

(defn ignore-prefix [prefix]
  (fn [file]
    (not ((require-prefixes prefix) file))))

(defn compile-js-filewatcher [{:keys [js-dirs] :as state}]
  (compile-watcher (-> js-dirs
                       (watcher*)
                       (file-filter ignore-dotfiles) ;; add filter
                       (file-filter (extensions :js)) ;; filter by
                       ;; this is too specific
                       (file-filter (ignore-prefix "./resources/public/js/compiled/out/goog/"))
                       (file-filter (ignore-prefix "./resources/public/js/compiled/out/clojure/"))
                       (file-filter (ignore-prefix "./resources/public/js/compiled/out/cljs/"))
                       (on-change (partial send-changed-files state)))))

(defn check-for-changes [{:keys [last-pass js-dirs] :as state}]
  (binding [wt/*last-pass* last-pass]
    (let [{:keys [updated? changed]} (compile-js-filewatcher state)]
      (when-let [changes (updated?)]
        (println (prn-str changes))
        (changed changes)))))

#_(check-for-changes ["./resources/public/js/compiled"] last-pass)

(defn file-watcher [state] (watcher ["./.cljsbuild-mtimes"]
                               (rate 500) ;; poll every 500ms
                               (on-change (fn [_] (check-for-changes state)))))

(defn create-initial-state [{:keys [js-dirs ring-handler]}]
  { :js-dirs js-dirs
    :ring-handler ring-handler 
    :last-pass (atom (System/currentTimeMillis))
    :file-md5-cache (atom {})
    :compile-wait-time (atom 10)
    :file-change-atom (atom (list))
    :logger-chan (chan (sliding-buffer 100))})

(defn start-server [{:keys [js-dirs ring-handler] :as opts}]
  (let [state (create-initial-state opts)]
    (merge { :http-server (server state)
             :file-change-watcher (file-watcher state)}
           state)))

(defn start-static-server [{:keys [js-dirs] :as opts}]
  (start-server (merge opts {:ring-handler (route/resources "/")})))

(defn stop-server [{:keys [http-server file-change-watcher] :as server-data}]
  (http-server)
  (future-cancel file-change-watcher))
