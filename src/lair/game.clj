(ns lair.game
  (:require [lair.game.attr :as attr]
            [lair.game.pos :as pos]))

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

;; CREATE

(defn create
  [m attrs]
  (let [id (:id m 0)
        m (-> (attr/merge m id attrs {:id id})
              (update :id (fnil inc 0)))]
    (vector m id)))

(defn delete
  [m e]
  (-> m
      (pos/unput e)
      (attr/clear e)))

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

;; MOVEMENT

(defn put
  ([m e pt]
    (put m e pt (:map (pos/of m e))))
  ([m e pt map]
    (put m e pt map (default-layer m e)))
  ([m e pt map layer]
    (pos/put m e pt map layer))
  ([m e pt map layer index]
    (pos/put m e pt map layer index)))

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
