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

;; SELECTION

(defn unselect
  [m e]
  (attr/remove m e :selected?))

(defn select
  [m e]
  (attr/add m e :selected? true))

(defn selected
  [m]
  (attr/with m :selected? true))
