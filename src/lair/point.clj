(ns lair.point
  (:require [lair.util :as util]
            [clj-tuple :as tuple])
  (:import [java.util PriorityQueue HashSet]))

(def point tuple/vector)
(def unit (point 0 0))


(defn add
  ([[x y] n-or-pt]
   (if (vector? n-or-pt)
     (let [[x2 y2] n-or-pt]
       (add x y x2 y2))
     (add x y n-or-pt n-or-pt)))
  ([[x y] x2 y2]
   (add x y x2 y2))
  ([x y x2 y2]
   (point (+ x x2) (+ y y2))))

(defn mult
  ([[x y] n-or-pt]
   (if (vector? n-or-pt)
     (let [[x2 y2] n-or-pt]
       (mult x y x2 y2))
     (mult x y n-or-pt n-or-pt)))
  ([[x y] x2 y2]
   (mult x y x2 y2))
  ([x y x2 y2]
   (point (* x x2) (* y y2))))


(def north (vector 0 -1))
(def south (vector 0 1))
(def north-west (vector -1 -1))
(def west (vector -1 0))
(def south-west (vector -1 1))
(def north-east (vector 1 -1))
(def east (vector 1 0))
(def south-east (vector 1 1))

(def directions
  [north
   north-east
   east
   south-east
   south
   south-west
   west
   north-west])

(def cardinal-directions
  [north
   east
   south
   west])

(defn adjacent
  ([[x y]]
   (adjacent x y))
  ([x y]
   (map #(add % x y) directions)))

(defn adjacent?
  ([[x y] [x2 y2]]
   (adjacent? x y x2 y2))
  ([x y x2 y2]
   (and  (not (and (= x x2) (= y y2)))
         (<= (Math/abs (- x x2)) 1)
         (<= (Math/abs (- y y2)) 1))))

(defn diagonal?
  [[x y] [x2 y2]]
  (let [a (zero? (- x x2))
        b (zero? (- y y2))]
    (not (util/xor a b))))

(defn manhattan
  ([[x y] [x2 y2]]
    (manhattan x y x2 y2))
  ([x y x2 y2]
   (+ (Math/abs (int (- x x2)))
      (Math/abs (int (- y y2))))))

(defn direction
  ([[x y] [x2 y2]]
   (direction x y x2 y2))
  ([x y x2 y2]
   (point (compare x2 x)
          (compare y2 y))))

;;path finding
(deftype A*Node [pt ^int g ^int h parent]
  Comparable
  (compareTo [this x]
    (compare (+ g h)
             (let [^A*Node x x]
               (+ (.g x) (.h x))))))

(defn a*-g
  [a b]
  (if (diagonal? a (.pt ^A*Node b))
    1.4
    1))

(def a*-h manhattan)
(def ^:dynamic *max-path-iter* 1000)
(defn a*
  ([pred [x y] [x2 y2]]
    (a* pred x y x2 y2))
  ([pred x y x2 y2]
   (let [open-q (PriorityQueue.)
         closed-s (HashSet.)
         goal (point x2 y2)
         current-v (volatile! nil)
         f (comp (filter pred)
                 (filter #(not (.contains closed-s %)))
                 (map #(A*Node. % (a*-g % @current-v) (a*-h % goal) @current-v)))
         reducing (completing #(.add open-q %2))]
     (.add open-q (A*Node. (point x y) 0 0 nil))
     (loop [i (int 0)]
       (when (< i *max-path-iter*)
         (when-let [^A*Node current (.poll open-q)]
           (if (= (.pt current) goal)
             (into (list)
                   (comp (take-while some?)
                         (map #(.pt ^A*Node %)))
                   (iterate #(.parent ^A*Node %) current))
             (do
               (.add closed-s (.pt current))
               (vreset! current-v current)
               (let [adj (adjacent (.pt current))]
                 (transduce f reducing nil adj)
                 (recur (inc i)))))))))))
