(ns examples.sparse
  (:require
   [devcards.core :include-macros true :as dcc]
   [devcards.util.edn-renderer :refer [html-edn]]
   [devcards.cards :as devc]
   [sablono.core :include-macros true :as sab]
   [frontier.core :as fr]
   [frontier.cards :as fc]
   [cemerick.double-check.generators :as gen]   
   [cljs.core.async :refer [put! sliding-buffer chan] :as async]))


(dcc/defcard reduce-slider-trans
  (devc/reduce-fr-card
   (devc/SliderCard. 1 1)
   {}
   [:set-index-for-key {:k 0 :index 5}] {:keyed-vals {0 5}}
   [:set-index-for-key {:k 0 :index 3}] {:keyed-vals {0 3}}
   [:set-index-for-key {:k 1 :index 3}] {:keyed-vals {0 3 1 3}}))

(dcc/defcard reduce-card-ex
  (devc/reduce-card + 1 [1 2
                    1 3
                    1 4
                    1 5]))

(dcc/defcard slider-card-dev
  (let [slider-args [(gen/sample gen/string 200)
                     (gen/sample gen/string 200)
                     (gen/sample gen/string 200)]]
    (devc/slider-card (fn [& args] {:r (apply + args)})
                 slider-args)))

(defn to-heckle-f [a b]
  (if (or (= b 0) (= b 112))
    (throw (js/Error. "Crappers Error Thrown"))
    (+ a b)))


(dcc/defcard heckler-ex
  (devc/heckler-card to-heckle-f #(gen/sample (gen/tuple (gen/tuple gen/int) gen/int) 100)
                :test-func (fn [x] (< (count x) 8))))






