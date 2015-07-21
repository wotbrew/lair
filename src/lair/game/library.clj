(ns lair.game.library
  "Base entity maps required for the game to function
   or to use as a basis for more complex entities"
  (:require [lair.game.pos :as pos]))

(def yellow-flag
  {:sprite        :yellow-flag
   :type          :yellow-flag
   :default-layer pos/flag-layer})

(def creature
  {:sprite :goblin-slave
   :type :creature
   :solid? true
   :player? true
   :default-layer pos/object-layer})

(def floor
  {:sprite :castle/floor
   :type :floor
   :default-layer pos/floor-layer})

(def wall
  {:sprite :castle/wall1
   :type :wall
   :solid? true
   :default-layer pos/wall-layer})
