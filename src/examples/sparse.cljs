(ns examples.sparse
  (:require
   [devcards.core :include-macros true :as dcc]
   [devcards.util.edn-renderer :refer [html-edn]]
   [devcards.cards :as devc]
   [sablono.core :include-macros true :as sab]
   [frontier.core :as fr]
   [frontier.cards :as fc]
   #_[cemerick.double-check.generators :as gen]   
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


#_(dcc/defcard slider-card-dev
  (devc/slider-card (fn [& args] (apply + args))
                    [(gen/sample gen/string 200)
                     (gen/sample gen/string 200)
                     (gen/sample gen/string 200)]))


(defn to-heckle-f [a b]
  (if (or (= b 333) (= b 3333))
    (throw (js/Error. "Crappers Error Thrown"))
    (+ a b)))


#_(dcc/defcard heckler-ex
  (devc/heckler-card to-heckle-f #(gen/sample (gen/tuple (gen/tuple gen/int)
                                                         gen/int) 100)
                :test-func (fn [x] (< (count x) 100))))

(defn css-transform [{:keys [rx ry rz tx ty tz]}]
  { "-webkit-transform" (str "rotateX(" (or rx 0) "deg) "
                             "rotateY(" (or ry 0) "deg) "
                             "rotateZ(" (or rz 0) "deg) "
                             "translateX(" (or tx 0) "px) "
                             "translateY(" (or ty 0) "px) "
                             "translateZ(" (or tz 0) "px) "
                             )})



(defn square [{:keys [x y z tx ty tz color]}]
  [:div { :style (js-obj "width" "30px"
                         "height" "30px"
                         "backgroundColor" color
                         "position" "absolute"
                         "top" "0px"
                         "left" "0px"
                         "-webkit-transform" (str "rotateX(" x "deg) "
                                                  "rotateY(" y "deg) "
                                                  "rotateZ(" z "deg) "
                                                  "translateX(" (or tx 0) "px) "
                                                  "translateY(" (or ty 0) "px) "
                                                  "translateZ(" (or tz 0) "px) "
                                                  )
                         
                         )}
   ])

(defn front-side [d]
  (assoc d
    :tz -15)
  )

(defn left-side [d]
  (assoc d
    :y (+ 90 (:y d))
    :tz -15)
  )

(defn cube [{:keys [color css]}]
  [:div.cube {:style (clj->js
                      (merge css
                             {:-webkit-transform-style "preserve-3d"}))}
   [:div.side {:style (clj->js
                       (merge {:backgroundColor "blue"
                               :width "100px"
                               :height "100px"
                               :top "0px"
                               :postition "absolute" }
                              (css-transform
                               { :ry 0
                                 :tz 50})))}]
   [:div.side {:style (clj->js
                       (merge {:backgroundColor "red"
                               :width "100px"
                               :height "100px"
                               :position "absolute"
                               :top "0px"}
                              (css-transform
                               { :ry 180
                                :tz 50})))}]
   [:div.side {:style (clj->js
                       (merge {:backgroundColor "green"
                               :width "100px"
                               :height "100px"
                               :position "absolute"
                               :top "0px"}
                              (css-transform
                               {  :ry 90
                                  :tz 50 })))}]
   
   [:div.side {:style (clj->js
                       (merge {:backgroundColor "green"
                               :width "100px"
                               :height "100px"
                               :position "absolute"
                               :top "0px"}
                              (css-transform
                               {  :ry 270
                                  :tz 50})))}]   
   ])

(dcc/defcard threed-fun
  (devc/slider-card
   (fn [x y z]
     {:x x :y y :z z})
   [(range 360)
    (range 360)
    (range 360)]
   :value-render-func
   (fn [{:keys [x y z] :as d}]
     [:div
      {:style (js-obj "position"
                      "relative"
                      "width" "100px"
                      "height" "100px")}
      (cube {:css (merge
                   { :width "100%"
                    :height "100%"}
                   (css-transform {:rx x
                                   :ry y
                                   :rz z}))})
      #_(square (assoc (left-side d)
                  :color "red"))
      #_(square (assoc (front-side d)
                  :color "blue"))
      
      #_(html-edn d)]
     )))

