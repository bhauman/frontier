(ns devcards.cards
  (:require
   [reactor.core :refer [render-to]]
   [sablono.core :as sab :include-macros true]
   [devcards.system :refer [IMountable]]
   [devcards.core :include-macros true :as dcc]
   [devcards.util.edn-renderer :refer [html-edn]]
   [frontier.core :as fr]
   [frontier.cards :as fc]
   [cljs.core.async :as async])
)

(defn react-card [react-component]
  (reify IMountable
    (mount [_ {:keys [node]}]
      (render-to react-component
                 node identity))
    (unmount [_ {:keys [node]}]
      (.unmountComponentAtNode js/React node))))

(defn react-runner-card [react-component-fn]
  (reify IMountable
    (mount [_ {:keys [node data]}]
      (add-watch data :react-runner
                 (fn [_ _ _ n]
                   (render-to (react-component-fn data)
                              node identity)
                   ))
      (render-to (react-component-fn data)
                 node identity))
    (unmount [_ {:keys [node data]}]
      (remove-watch data :react-runner)
      (.unmountComponentAtNode js/React node))))

(defn sab-card [sab-template]
  (react-card (sab/html sab-template)))

(defn edn-card [clj-data]
  (sab-card
   [:div.devcards-pad-card
    (html-edn clj-data)]))

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

;; slider card

(defrecord SliderCard [f arg-seqs]
  fr/iTransform
  (-transform [o [msg-name data] state]
    (condp = msg-name
      :set-index-for-key (assoc-in state [:keyed-vals (:k data)] (:index data))
      state))
  fr/iDerive
  (-derive [o state]
    (let [sorted-keyed-values (sort-by first (into [] (:keyed-vals state)))
          slider-inputs (map (fn [[k i] seq*]
                              {:k k
                               :index i
                               :v (nth seq* i)
                               :max (dec (count seq*))})
                              sorted-keyed-values
                              arg-seqs)]
      (assoc state
        :slider-inputs slider-inputs
        :result
        (apply f (map :v slider-inputs))))))

(defn slider-input-control [{:keys [k v index seq*] :as ic} event-chan]
  [:div.slider-control
    [:input {:type "range"
             :onChange (fn [e]
                         (async/put! event-chan
                               [:set-index-for-key
                                {:k k,
                                 :index (.parseInt
                                         js/window
                                         (.-value (.-target e)))}]))
             :defaultValue index
             :min 0
             :max (:max ic)}]
   [:div (prn-str v)]])

(defn make-slider-renderer [value-render-func]
  (fn [{:keys [state event-chan] :as rstate}]
    [:div
     [:div.col-md-4
      [:h4 "args"]
      (map
       (fn [slider-in]
         (slider-input-control slider-in event-chan))
       (:slider-inputs state))]
     [:div.col-md-8
      [:h4 "result"]
      [:div (value-render-func (:result state))]]
     [:div.clearfix]]))


(defn slider-card [f arg-seqs & {:keys [value-render-func]}]
  (fc/system-card { :keyed-vals (into {}
                                   (mapv vector (range (count arg-seqs))
                                         (repeat 0)))}
               (fr/make-renderable
                (fr/compose (SliderCard. (fn [& args] {:r (apply + args)})
                                         arg-seqs))
                (make-slider-renderer (or value-render-func html-edn)))
               []))

;; heckler card

(defn heckle-values [generator]
  (mapv
   (fn [args]
     {:args args })
   (generator)))

(defn heckle-error? [res-val]
  (get (meta res-val) :heckle-error))

(defn heckle-derive [data f test-func]
  (map
   (fn [args]
     (let [res (try (apply f (:args args))
                    (catch :default e (with-meta {} {:heckle-error e})))
           passed (if (heckle-error? res) true
                      (test-func res))]
       { :args (:args args)
         :class (if (heckle-error? res)
                  "danger "
                  (if (not passed)
                    "warning "
                    (if (:only-errors data)
                      "hidden" "")))
         :res-val res
         :passed passed
         :error (when (heckle-error? res)
                 (:heckle-error (meta res))) }))
   (:gen-arg-list data)))

(defn heckle-renderer [f data generator value-render-func test-func]
  (let [derived-data (heckle-derive @data f test-func)]
    [:div.heckler-card
     [:div
      {:style (js-obj "paddingLeft" "14px")}
      [:a.btn.btn-danger.navbar-btn
       {:type "button"
        :onClick (fn [] (swap! data assoc-in [:gen-arg-list]
                              (heckle-values generator)))}
       "Re-heckle!"]
      [:a.btn.btn-default.navbar-btn
       {:style (js-obj "marginLeft" "14px")
        :className (if (:only-errors @data) "active" "")
        :onClick (fn [] (swap! data update-in [:only-errors] (fn [x] (not x)))) }
       "Toggle Errors"]
      [:span {:style (js-obj "paddingLeft" "14px")}
       (if (pos? (count (filter :error derived-data)))
         [:span.label.label-danger
          (count (filter :error derived-data))
          " Errors"]
         [:span.label.label-success "No errors"])]
      [:span {:style (js-obj "paddingLeft" "14px")}
       (let [failed-tests (filter (fn [x] (= false (:passed x))) derived-data)]
         (if (pos? (count failed-tests))
           [:span.label.label-warning
            (count failed-tests)
            " Tests Failed"]
           [:span]))]]
     [:table.table.table-striped.table-hover 
      [:tr
       [:th.devcards-first-cell "Called"]
       [:th "Result"]]
      (map
       (fn [{:keys [args error res-val] :as res}]
         (let []
           [:tr
            {:className (str (:class res))}
            [:td.devcards-first-cell
             [:span.text-muted "(f "]
             (interpose [:span.text-muted ", "]
                        (map
                         (fn [a] [:span (prn-str a)])
                         args)) [:span.text-muted " )"]]
            [:td
             (if error
               (.-message error)
               (value-render-func res-val))
             ]]
           ))
       derived-data
       )]]
    ))

(defn heckler-card [f generator & {:keys [test-func
                                          value-render-func]}]
  (let [system-func (fn [data]
                      (when (or (nil? @data)
                                (zero? (count @data)))
                        (swap! data assoc-in [:gen-arg-list]
                               (heckle-values generator)))
                      (sab/html
                       (heckle-renderer f data generator
                                        (or value-render-func html-edn)
                                        (or test-func (fn [x] true)))))]
    (react-runner-card system-func)))

;; reduce-card

(defn reduce-card->tests [f curr value-expected-vec]
  (if (empty? value-expected-vec)
    []
    (let [val* (first value-expected-vec)
          res-val  (f curr (first val*))]
      (cons
       { :type :are=
         :exp1 res-val #_(list 'f curr (first val*))
         :exp2 (second val*)
         :passed (= (f curr (first val*)) (second val*))}
       (reduce-card->tests f res-val (rest value-expected-vec))))))

(defn reduce-card [f init value-expected-vec]
  (let [red-tests (reduce-card->tests f init (partition 2 value-expected-vec))]
    (apply test-card red-tests)))

(defn reduce-fr-card [fr-comp initial-state & msg-expects-vec]
  (reduce-card (fn [state msg]
                 (fr/-transform (SliderCard. 1 1) msg state))
               initial-state
               msg-expects-vec))

