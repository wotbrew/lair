(ns lair.game.pos
  (:require [lair.game.attr :as attr]
            [lair.rect :as rect]
            [lair.point :as point]
            [lair.util :as util]))

;;layers
(def floor-layer 100000)
(def item-layer 200000)
(def wall-layer 300000)
(def decor-layer 400000)
(def selection-layer 500000)
(def object-layer 600000)
(def flag-layer 700000)

(defn index
  [m e map pt layer index]
  (-> m
      (assoc-in [::pos e] (hash-map :map map :pt pt :layer layer :index index))
      (assoc-in [::mpli map pt layer index] e)
      (update-in [::mpl map pt layer] util/set-conj e)
      (update-in [::mp map pt] util/set-conj e)
      (util/sorted-assoc-in [::sort map (vector layer pt index)] e)))

(defn unindex
  [m e map pt layer index]
  (-> m
      (util/dissoc-in [::pos e])
      (util/dissoc-in [::mpli map pt layer index])
      (util/disjoc-in [::mpl map pt layer] e)
      (util/disjoc-in [::mp map pt] e)
      (util/dissoc-in [::sort map (vector layer pt index)])))

(defn unput
  [m e]
  (if-let [{:keys [map pt layer index]} (-> m ::pos (get e))]
    (-> (unindex m e map pt layer index))
    m))

(defn put
  ([m e pt map layer]
    (put m e pt map layer 0))
  ([m e pt map layer index]
   (let [m (unput m e)]
     (loop [m m e e pt pt map map layer layer index index]
       (if (-> m ::mpli (get map) (get pt) (get layer) (get index))
         (recur m e pt map layer (inc index))
         (lair.game.pos/index m e map pt layer index))))))

(defn sort-run!
  "Runs a function `f` efficiently over the sorted index
   in order passing pt, layer, entity as arguments"
  [m map f]
  (reduce-kv
    (fn [_ x v]
      (f (nth x 1) (nth x 0) v))
    nil
    (-> m ::sort (get map))))

(defn of
  [m e]
  (-> m ::pos (get e)))

(defn pt
  [m e]
  (:pt (of m e)))

(defn map-of
  [m e]
  (:map (of m e)))

(defn at
  ([m map pt layer index]
    (-> m ::mpli (get map) (get pt) (get layer) (get index)))
  ([m map pt layer]
    (-> m ::mpl (get map) (get pt) (get layer)))
  ([m map pt]
    (-> m ::mp (get map) (get pt)))
  ([m map]
    (-> m ::mp (get map) vals)))

(defn in
  ([m map points]
   (mapcat #(at m map %) points))
  ([m map layer points]
   (mapcat #(at m map % layer) points)))

(defmulti distance (fn [m e thing] (class thing)))

(defmethod distance :default
  [m e other]
  (when-let [b (pt m other)]
    (distance m e b)))

(defmethod distance clojure.lang.IPersistentVector
  [m e pt]
  (when-let [a (lair.game.pos/pt m e)]
    (point/manhattan a pt)))

(defn adjacent
  [m e]
  (when-let [pt (lair.game.pos/pt m e)]
    (point/adjacent pt)))
