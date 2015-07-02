(ns lair.game
  (:require [lair.game.attr :as attr]))

(defn create
  [m attrs]
  (let [id (:id m 0)
        m (-> (attr/merge m id attrs {:id id})
              (update :id (fnil inc 0)))]
    (vector m id)))

(defn creature
  [m]
  (create m {:type :creature
             :sprite :goblin-slave}))

(defn creature?
  [m e]
  (= (attr/find m e :type) :creature))

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
