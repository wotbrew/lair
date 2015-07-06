(ns lair.event
  (:require [manifold.bus :as bus]
            [manifold.stream :as s]
            [lair.global :as global]))
(defonce streams (atom []))
(defn stream-ctor
  []
  (let [s (s/stream)]
    (swap! streams conj s)
    s))

(def bus (bus/event-bus stream-ctor))
(doseq [s @streams]
  (s/close! s))
(reset! streams [])

(def created (bus/subscribe bus :created))
(def put (bus/subscribe bus :put))

;; INPUT
(def input (bus/subscribe bus :input))
(def mouse-pos
  (->> input
      (s/transform (comp
                     (map :mouse-point)
                     (dedupe)))))

(def mouse-world-point
  (->> mouse-pos
       (s/transform (comp
                      (map global/world-point)
                      (dedupe)))))


(defn publish!
  [event-or-events]
  (when event-or-events
    (if (sequential? event-or-events)
      (doseq [e event-or-events]
        (bus/publish! bus (:type e) e))
      (bus/publish! bus (:type event-or-events) event-or-events))))
