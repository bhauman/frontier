(defproject frontier "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/clojurescript "0.0-2202"]
                 [org.clojure/core.async "0.1.278.0-76b25b-alpha"]
                 [sablono "0.2.16"]] ;; this is a dep for now

  :source-paths ["src"]

  :profiles {
             :dev {
                   :dependencies [[figwheel "0.1.3-SNAPSHOT"]
                                  [devcards "0.1.0-SNAPSHOT"]]
                   :plugins [[lein-cljsbuild "1.0.3"]
                             [lein-figwheel "0.1.3-SNAPSHOT"]]}}
  
  :cljsbuild {
              :builds [{:id "examples"
                        :source-paths ["src" "examples/frontier_examples" #_"checkouts/devcards/src"]
                        :compiler {:output-to "resources/public/js/compiled/examples.js"
                                   :output-dir "resources/public/js/compiled/out"
                                   :externs ["resources/public/js/externs/jquery-1.9.js"]
                                   :optimizations :none
                                   :source-map true}}]})

