(defproject frontier "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/clojurescript "0.0-2202"]
                 [org.clojure/core.async "0.1.278.0-76b25b-alpha"]
                 [prismatic/schema "0.2.0"]                 
                 [sablono "0.1.5"]
                 [crate "0.2.4"]
                 [jayq "2.4.0"]
                 [org.clojars.franks42/cljs-uuid-utils "0.1.3"]                 
                 [om "0.5.3"]
                 #_[com.cemerick/double-check "0.5.7-SNAPSHOT"]
                 [cljschangeserver "0.0.1"]
                 
                 ;; devserver
                 [fs "1.1.2"]
                 [ring "1.2.1"] 
                 [http-kit "2.1.16"]
                 [compojure "1.1.6"]
                 [watchtower "0.1.1"]
                 [digest "1.4.3"]]

  :plugins [[lein-cljsbuild "1.0.3"] [devserver "0.1.0-SNAPSHOT"]]

  :source-paths ["src" "src/reactor" "src/frontier" "src/devcards" ]

  :cljsbuild {
              :builds [{:id "examples"
                        :source-paths ["src/examples" "src/reactor" "src/frontier" "src/devcards" "src/devserver"]
                        :compiler {:output-to "resources/public/js/compiled/examples.js"
                                   :output-dir "resources/public/js/compiled/out"
                                   :externs ["resources/public/js/externs/jquery-1.9.js"]
                                   :optimizations :none
                                   :source-map true}}]}
  :main devserver.core)

