(ns devcards.cards)

(defmacro is [body]
  `{ :type :is  :body (quote ~body) :passed ~body})

(defmacro are= [exp1 exp2]
  `{ :type :are= :exp1 (quote ~exp1) :exp2 (quote ~exp2) :passed (= ~exp1 ~exp2)})

(defmacro are-not= [exp1 exp2]
  `{ :type :are-not=  :exp1 (quote ~exp1) :exp2 (quote ~exp2) :passed (not= ~exp1 ~exp2)})

