(ns devcards.cards
  (:require
   [reactor.core :refer [render-to]]
   [sablono.core :as sab :include-macros true]
   [frontier.util.edn-renderer :refer [html-edn]]))

(defn react-card [react-component]
  (fn [{:keys [node]}]
    (render-to react-component node identity)))

(defn sab-card [sab-template]
  (react-card (sab/html sab-template)))

(defn edn-card [clj-data]
  (sab-card (html-edn clj-data)))

(defmulti render-test :type)

(defn test-wrapper [test bd]
  [:li.list-group-item
   {:className (if (:passed test) "list-group-item-success" "list-group-item-danger")}
   (if (:passed test)
     [:span.glyphicon.glyphicon-ok]
     [:span.glyphicon.glyphicon-remove])
   [:span.test-body
    bd]])

(defmethod render-test :is [test]
  (test-wrapper test
                (list [:span.operator "is"]
                      [:span.exp (prn-str (:body test))])))

(defmethod render-test :are= [test]
  (test-wrapper test
                (list [:span.operator "="]
                      [:span.exp (prn-str (:exp1 test))]
                      [:span.exp (prn-str (:exp2 test))])))

(defmethod render-test :are-not= [test]
  (test-wrapper test
                (list [:span.operator "!="]
                      [:span.exp (prn-str (:exp1 test))]
                      [:span.exp (prn-str (:exp2 test))])))

(defn test-card [& assertions]
  (sab-card
   [:ul.list-group.test-group
    (map render-test assertions)]))
