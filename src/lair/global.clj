(ns lair.global
  (:require [lair.gdx :as gdx]
            [lair.game :as game]
            [lair.game.pos :as pos]
            [lair.gdx.cam :as cam]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.tools.logging :refer [error warn info]]))


;; STATE

(defonce batch (delay @(gdx/go (gdx/batch))))
(defonce font (delay @(gdx/go (gdx/flipped-font))))
(defonce game-camera (delay @(gdx/go (gdx/flipped-camera 800 600))))
(defonce ui-camera (delay @(gdx/go (gdx/flipped-camera 800 600))))

(def input (atom {}))
(def input-modifier (atom false))

(def settings (atom (edn/read-string (slurp (io/resource "settings.edn")))))

(def game (agent (let [[m id] (game/creature {})
                       [m id2] (game/creature m)]
                   (-> m
                       (pos/put id [0 0] :foo pos/object-layer)
                       (pos/put id2 [4 4] :foo pos/object-layer)))))

(defn send-game
  [f & args]
  (apply send game f args)
  nil)

;; API - BASICS

(defn ensure-cell-size
  []
  (if-let [cs (:cell-size @settings)]
    cs
    (do (error "No cell size could be found!")
        (throw (Exception. "No cell-size found!")))))

(defn cell-width
  []
  (first (ensure-cell-size)))

(defn cell-height
  []
  (second (ensure-cell-size)))

;; API - CAM

(defn move-cam!
  "Moves the game camera to the x, y pixel co-ordinates"
  ([[x y]]
   (move-cam! x y))
  ([x y]
   (cam/move! @game-camera x y)))

(defn move-cam-to-origin!
  "Moves the game camera to its origin position
  (top left)"
  []
  (cam/move-to-default! @game-camera))

(defn shift-cam!
  "Moves the camera to by x, y pixels"
  ([[x y]]
   (shift-cam! x y))
  ([x y]
   (cam/shift! @game-camera x y)))

(defn look-at-pixel-point!
  "Points the camera at the given pixel"
  ([[x y]]
   (look-at-pixel-point! x y))
  ([x y]
   (move-cam! x (+ y (cell-width)))))

(defn look-at-map-point!
  "Points the camera at the given
   map co-ordinates"
  ([[x y]]
   (look-at-map-point! x y))
  ([x y]
   (look-at-pixel-point! (* x (cell-width)) (* y (cell-height)))))

(defn look-at-entity!
  "Points the camera at the given
   map co-ordinates"
  [e]
  (let [m @game]
    (if-let [{:keys [pt]} (pos/of m e)]
      (look-at-map-point! pt)
      (warn "Tried to point camera at entity that did not have a position"
            "entity:" e))))

(defn current-cam-speed
  []
  (let [settings @settings]
    (* (if @input-modifier (:cam-modifier-speed settings) 1)
       (:cam-speed settings)
       (:cam-constant-factor settings)
       (gdx/delta))))

;; API - SELECTION

(defn select!
  [e]
  (send-game game/select e))

(defn unselect!
  [e]
  (send-game game/unselect e))

(defn selected
  []
  (game/selected @game))

;; INPUT - MOUSE

(defn mouse-screen-pixel
  []
  (:mouse-point @input))

(defn mouse-world-pixel
  []
  (let [[x y] (mouse-screen-pixel)]
    (cam/unproject @game-camera x y)))

(defn mouse-world
  []
  (let [[x y] (mouse-world-pixel)
        [cw ch] (ensure-cell-size)]
    (vector (int (/ x cw)) (int (/ y ch)))))

;; INPUT - HANDLERS

(def commands (edn/read-string (slurp (io/resource "commands.edn"))))

(defmulti handle! identity)

(defmethod handle! :default
  [x]
  (warn "Unknown command:" x))

(defmethod handle! :modifier-down
  [_]
  (reset! input-modifier true))

(defmethod handle! :modifier-up
  [_]
  (reset! input-modifier false))

(defmethod handle! :cam-up
  [_]
  (shift-cam! 0 (* -1 (current-cam-speed))))

(defmethod handle! :cam-down
  [_]
  (shift-cam! 0 (current-cam-speed)))

(defmethod handle! :cam-left
  [_]
  (shift-cam! (* -1 (current-cam-speed)) 0))

(defmethod handle! :cam-right
  [_]
  (shift-cam! (current-cam-speed) 0))


(defn handle-input!
  [input]
  (doseq [k (:pressed input)
          c (-> commands :down k)]
    (handle! c))
  (doseq [k (:hit input)
          c (-> commands :hit k)]
    (handle! c)))