(ns lair.game
  (:require [lair.game.attr :as attr]
            [lair.game.pos :as pos]
            [lair.point :as point]
            [lair.shape :as shape]
            [lair.util :as util]))

;; QUERY

(defn by-type
  [m type]
  (attr/with m :type type))

(defn by-type-of
  [m attrs]
  (by-type m (:type attrs)))

(defn default-layer
  [m e]
  (or (:layer (pos/of m e))
      (:default-layer (attr/all m e))))

(defn entity-isa?
  [m e type]
  (isa? (attr/find m e :type) type))

;; EVENTS

(defn event
  [m event-map]
  (update m :events util/vec-conj event-map))

;; CREATE

(defn create
  [m attrs]
  (let [id (:id m 0)
        attrs (assoc attrs :id id)
        m (-> (attr/merge m id attrs)
              (update :id (fnil inc 0))
              (event {:type :created
                      :entity id
                      :attrs attrs}))]
    (vector m id)))

(defn delete
  [m e]
  (-> m
      (pos/unput e)
      (attr/clear e)
      (event {:type :deleted
              :entity e})))

(defn clear
  [m ents]
  (reduce delete m ents))

;; TIME

(defn turns?
  [m]
  (= (:time-mode m) :turns))

(defn real?
  [m]
  (not (turns? m)))

;; CREATURES

(defn natural-maximum-of
  [m e resource]
  10.0)

(defn amount-of
  [m e resource]
  (or (:current (attr/find m e resource))
      (natural-maximum-of m e resource)))

(defn maximum-of
  [m e resource]
  (or (:maximum (attr/find m e resource))
      (natural-maximum-of m e resource)))

(defn refill
  ([m e resource]
    (refill m e resource 1))
  ([m e resource mult]
   (attr/update m e resource assoc :current (* mult (maximum-of m e resource)))))

(defmulti expend (fn [m e resource amount] resource))

(defn expend*
  [m e resource amount]
  (attr/update m e resource assoc :current (max 0 (- (amount-of m e resource) amount))))

(defmethod expend :default
  [m e resource amount]
  (expend* m e resource amount))

(defmethod expend :ap
  [m e resource amount]
  (if (turns? m)
    (expend* m e resource amount)
    m))

(defn can-afford?
  [m e resource cost]
  (<= cost (amount-of m e resource)))

(defn creatures
  [m]
  (attr/with m :type :creature))

(defn refresh-creatures
  [m]
  (reduce #(refill %1 %2 :ap) m (creatures m)))

;; PLAYERS

(defn player?
  [m e]
  (= (attr/find m e :faction)
     :player))

(defn players
  [m]
  (attr/with m :faction :player))

(defn playern
  [m n]
  (nth (seq (players m)) n nil))

(defn refresh-players
  [m]
  (reduce #(refill %1 %2 :ap) m (players m)))

;; SELECTION

(defn unselect
  [m e]
  (attr/remove m e :selected?))

(defn can-select?
  [m e]
  (let [atts (attr/all m e)]
    (and (not (:selected? atts))
         (player? m e))))

(defn select
  [m e]
  (if (can-select? m e)
    (attr/add m e :selected? true)
    m))

(defn selected
  [m]
  (attr/with m :selected? true))

(defn selected?
  [m e]
  (attr/find m e :selected?))

(defn unselect-all
  [m]
  (reduce unselect m (selected m)))

(defn select-only
  [m e]
  (-> (unselect-all m)
      (select e)))

(defn select-many
  [m coll]
  (reduce select m coll))

;; SOLIDITY

(defn solid?
  [m e]
  (attr/find m e :solid?))

(defn solid-at?
  [m pt map]
  (some #(solid? m %) (pos/at m map pt)))

;; MOVEMENT

(defn put
  ([m e pt]
    (put m e pt (:map (pos/of m e))))
  ([m e pt map]
    (put m e pt map (default-layer m e)))
  ([m e pt map layer]
   (if (solid-at? m pt map)
     m
     (-> (pos/put m e pt map layer)
         (event {:type :put
                 :entity e
                 :previous (pos/of m e)
                 :pt pt
                 :map map
                 :layer layer})))))

(defn put-create
  ([m attrs pt map]
   (let [[m e] (create m attrs)]
     (put m e pt map (default-layer m e))))
  ([m attrs pt map layer]
   (let [[m e] (create m attrs)]
     (put m e pt map layer))))

(defn put-create-many
  ([m attrs map points]
   (reduce #(put-create %1 attrs %2 map) m points))
  ([m attrs map layer points]
   (reduce #(put-create %1 attrs %2 map layer) m points)))

(defn can-step-ignoring-cost?
  [m e pt]
  (when-let [p (pos/pt m e)]
    (point/adjacent p pt)))

(defn can-step?
  [m e pt]
  (and
    (can-step-ignoring-cost? m e pt)
    (can-afford? m e :ap 1)))

(defn step
  [m e pt]
  (if (can-step? m e pt)
    (-> (put m e pt)
        (expend e :ap 1))
    m))

;; VIS

(defn explore
  [m map points]
  (update-in m [::explored map] (fnil into #{}) points))

(defn explored
  [m map]
  (-> m ::explored (get map) (or #{})))

(defn look
  [m e points]
  (if-let [map (:map (pos/of m e))]
    (cond-> m
            (player? m e) (explore map points)
            :then (attr/add e :visible points))
    m))

(defn visible?
  [m e pt]
  (contains? (attr/find m e :visible) pt))

(defn visible-to-players?
  [m pt]
  (some #(visible? m % pt) (players m)))

(defn opaque?
  [m e]
  (attr/find m e :opaque?))

(defn opaque-at?
  [m pt map]
  (some #(opaque? m %) (pos/at m map pt)))

(defn los?
  [m e pt]
  (when-let [pos (pos/of m e)]
    (let [map (:map pos)
          origin (:pt pos)]
      (every? #(not (opaque-at? m % map))
              (butlast (shape/line origin pt))))))

(defn fov
  [m e]
  (if-let [[x y] (:pt (pos/of m e))]
    (->>
     (shape/filled-circle x y 5)
     (into #{} (filter #(los? m e %))))
    #{}))

;; SWITCH TIME

(defn into-turns
  [m]
  (assoc m :time-mode :turns))

(defn into-real
  [m]
  (let [m (assoc m :time-mode :real)]
    (-> m
        refresh-creatures)))
