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

;; TIME

(defn turns?
  [m]
  (= (:time-mode m) :turns))

(defn real?
  [m]
  (not (turns? m)))

(defn into-turns
  [m]
  (assoc m :time-mode :turns))

(defn into-real
  [m]
  (assoc m :time-mode :real))

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

;; PLAYERS

(defn player?
  [m e]
  (attr/find m e :player?))

(defn players
  [m]
  (attr/with m :player? true))

(defn playern
  [m n]
  (nth (seq (players m)) n nil))

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
