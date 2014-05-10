(ns frontier.adaptors
  (:require
   [frontier.core :refer [compose make-renderable
                          -transform -render -derive
                          -filter-input
                          -initialize
                          -stop
                          -effect
                          transform-with-effects
                          HistoryKeeper
                          Namespacer                          
                          make-runnable
                          runner-start
                          runner-stop]]
   [sablono.core :as sab :include-macros true]   
   [cljs.core.async :refer [chan close! <!]]
   [om.core :as om :include-macros true])
  (:require-macros
      [cljs.core.async.macros :refer [go-loop]]))

;; would really like to try different composition strategies using om
;; 1. having one event-channel for all (pass it down a side channel)
;; 2. independant event-channels
;; 3. using the cursor path to determine the sent message path
;; 4. mimic the om api frontier/build, frontier/root etc

;; an local react state based om-component or should we pass
;; { :cursor <> :local-state <> } for transformation

;; it would be interesting to have a history manager om component wrap
;; another om-component as an example of nested composition

;; there strange relationship between display hierarchy and control
;; heirarchy hence pedestals notion of app-state and display state
;; which may be the way to go, something like frontier/controller 'vs
;; frontier/build

;; don't forget about mirroring frontier control structures on the
;; server with an immutable db of messages ala lambda-architecture

(defn scoped-cursor-state [cursor]
  (get-in @(om/-state cursor) (om/-path cursor)))

;; we can parameterize the how and which state to work on cursor vs.
;; local state (local state could come in and leave as meta data?)

(defn om-adaptor [fr-comp]
  "this doesn't work on the react local state right now"
  (fn [cursor owner]
    (reify
      om/IInitState
      (init-state [_]
        { :event-chan (chan)
          :effect-chan (chan) })
      om/IWillMount
      (will-mount [_]
        #_(print "MOUNT called")        
        (let [event-chan  (om/get-state owner :event-chan)
              effect-chan (om/get-state owner :effect-chan)]
          (go-loop []
                   (when-let [msg (<! event-chan)]
                     (let [cursor-state (scoped-cursor-state cursor)
                           filtered-msg (-filter-input fr-comp msg cursor-state)
                           new-state (transform-with-effects fr-comp effect-chan cursor-state
                                                             filtered-msg)]
                       (om/update! cursor new-state))
                     (recur)))
          ;; will do effects later
          (go-loop []
                   (when-let [msg (<! effect-chan)]
                     (let [cursor-state (scoped-cursor-state cursor)]
                       (-effect fr-comp
                                msg
                                cursor-state
                                event-chan
                                effect-chan))
                     (recur)))
          (-initialize fr-comp
                       (scoped-cursor-state cursor)
                       event-chan)))
      om/IWillUnmount
      (will-unmount [_]
        #_(print "unmount called")
        (-stop fr-comp)        
        (close! (om/get-state owner :event-chan))
        (close! (om/get-state owner :effect-chan)))
      om/IRenderState
      (render-state [_ {:keys [event-chan]}]
        #_(print "rendering" (-derive fr-comp (scoped-cursor-state cursor)))
        ;; does is speed rendering to use the cursor?
        (sab/html
         (-render fr-comp { :state (-derive fr-comp cursor) 
                            :event-chan event-chan
                            :om-cursor cursor }))))))

