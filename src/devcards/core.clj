(ns devcards.core
  (:require
   [cljs.compiler :refer (munge)])
  (:refer-clojure :exclude (munge defonce)))


(defmacro defcard
  [vname expr]
  (let [ns (-> &env :ns :name name munge)]
    `(do
       (def ~vname (fn [] ~expr))
       (devcards.core/register-card  [~(keyword ns) ~(keyword vname)] [] (fn [] ~expr)))))

