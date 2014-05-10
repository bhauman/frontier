(ns frontier-examples.components
  (:require
   [frontier.core :refer [IInputFilter
                          IInit
                          ITransform
                          IEffect
                          IDerive
                          add-effects]]
   [cljs.core.async :as async :refer [put!]]   
   [jayq.util :refer [log]]))

(defmulti count-trans identity)

(defmethod count-trans :default [_ system data] system)

(defmethod count-trans :inc [_ system data]
  (assoc system :count
         (inc (:count system))))

(defmethod count-trans :dec [_ system data]
  (assoc system :count
         (dec (:count system))))

(defrecord ExampleCounter []
  IInputFilter
  (-filter-input [_ [name data] system]
    [name data])
  IInit
  (-initialize [o system event-chan]
    #_(log "initializing counter"))
  ITransform
  (-transform [o msg system]
    (count-trans (first msg) system (last msg)))
  IDerive
  (-derive [o system]
    (if (:count system)
      (assoc system
        :double (* 2 (:count system))
        :triple (* 3 (:count system)))
      system))
  IEffect
  (-effect [o msg system event-chan effect-chan]
    #_(log "affecting")
    )
  )


(defmulti todo-trans identity)

(defmethod todo-trans :default [_ system data] system)

(defmethod todo-trans :create-todo [_ system data]
  (-> system
      (add-effects [:store-changes data])
      (update-in [:todos]
                 (fn [todos]
                   (assoc (or todos {})
                     (:id data) data)))))

(defmethod todo-trans :delete-todo [_ system data]
  (update-in system
             [:todos]
             (fn [todos]
               (filter #(not= (:id %) (:id data))
                       todos))))

(defmulti todo-eff identity)

(defmethod todo-eff :default [_ system data event-chan])

(defmethod todo-eff :store-changes [_ system data event-chan]
  (put! event-chan [:inc {}])
  #_(log "storing changes"))

(defrecord ExampleTodos []
  IInputFilter
  (-filter-input [_ [name data] system]
    (if (= name :create-todo)
      [name (assoc data :id (rand-int 10000000))]
      [name data]))
  IInit
  (-initialize [o system event-chan]
    #_(log "initializing todos"))
  ITransform
  (-transform [o msg system]
    (todo-trans (first msg) system (last msg)))
  IEffect
  (-effect [o [name data] system event-chan effect-chan]
    (todo-eff name system data event-chan)
    #_(log "affecting 2")
    ))
