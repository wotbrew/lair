(ns lair.anim
  (:require [lair.global :as global]
            [lair.gfx :as gfx]
            [lair.point :as point]))

(def counter (atom 0))
(def state (atom {}))

(defn add!
  [animation]
  (when animation
    (let [id (swap! counter inc)]
      (swap! state assoc id animation)
      id)))

(defn remove!
  [id]
  (swap! state dissoc id))

(defprotocol IAnim
  (animate! [this delta]))

(def ^:dynamic *jiggle-speed* 50)
(def ^:dynamic *jiggle-distance* 16)

(defn jiggle
  [offset origin direction delta]
  (let [[x y] (point/mult offset (* *jiggle-speed* delta))]
    (if (< (Math/abs x) *jiggle-distance*)
      nil)))

(defrecord AttackJiggle [e direction]
  IAnim
  (animate! [this delta]
    (swap! gfx/offsets update e
           (fnil point/add point/unit)
           (point/mult direction (* *jiggle-speed* delta)))
    nil))

(defn attack-jiggle
  [e target]
  (let [a (global/point-of e)
        b (global/point-of target)]
    (when (and a b)
      (->AttackJiggle e (point/direction a b)))))

(defn animate-all!
  [delta]
  (reduce-kv (fn [_ k v]
               (animate! v delta))
             nil
             @state))
