(ns lair.game
  (:require [lair.game.attr :as attr]
            [lair.game.pos :as pos]
            [lair.point :as point]
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

;; SELECTION

(defn unselect
  [m e]
  (attr/remove m e :selected?))

(defn can-select?
  [m e]
  (let [atts (attr/all m e)]
    (and (not (:selected? atts))
         (isa? (:type atts) :creature))))

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

(defn step
  [m e pt]
  (if-let [p (pos/pt m e)]
    (if (point/adjacent? p pt)
      (put m e pt)
      m)
    m))
