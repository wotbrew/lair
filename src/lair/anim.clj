(ns lair.anim
  (:require [lair.global :as global]
            [lair.gfx :as gfx]
            [lair.point :as point]
            [clojure.core.async :refer [<! >! go chan] :as async]))

(def jiggle-amount 8)
(def jiggle-ms 33)

(defn jiggle
  [a b]
  (let [pa (global/point-of a)
        pb (when pa (global/point-of b))
        direction (when pb (point/direction pa pb))
        origin (when direction (point/direction pb pa))
        offset (point/mult direction jiggle-amount)]
    (async/go-loop
      [[x y :as o] offset]
      (if (and (<= -1 x 1) (<= -1 y 1))
        (do
          (swap! gfx/offsets dissoc a))
        (do
          (swap! gfx/offsets assoc a o)
          (<! (async/timeout jiggle-ms))
          (recur (point/add o origin)))))))