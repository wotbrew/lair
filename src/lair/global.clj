(ns lair.global
  (:require [lair.gdx :as gdx]
            [lair.game :as game]
            [lair.game.pos :as pos]
            [lair.game.attr :as attr]
            [lair.gdx.cam :as cam]
            [lair.rect :as rect]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.tools.logging :refer [error warn info]]))


;; STATE

(defonce batch (delay @(gdx/go (gdx/batch))))
(defonce font (delay @(gdx/go (gdx/flipped-font))))
(def game-camera (delay @(gdx/go (gdx/flipped-camera 800 600))))
(def ui-camera (delay @(gdx/go (gdx/flipped-camera 800 600))))

(def input (atom {}))
(def input-modifier (atom false))

(def settings (atom (edn/read-string (slurp (io/resource "settings.edn")))))

(def game (agent (let [[m id] (game/creature {})
                       [m id2] (game/creature m)]
                   (-> m
                       (pos/put id [0 0] :foo pos/object-layer)
                       (pos/put id2 [6 4] :foo pos/object-layer)
                       (attr/add 0 :sprite :goblin-slave)))))

(def unit-rect (vector 0 0 0 0))
(def lasso (atom unit-rect))

(defn lassoing?
  []
  (let [[_ _ w h] @lasso]
    (and (pos? w)
         (pos? h))))

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

(defn unproject
  [pt-or-rect]
  (let [[x y] pt-or-rect
        [xc yc] (cam/unproject @game-camera x y)]
    (assoc pt-or-rect 0 xc 1 yc)))

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

(defn select-only!
  [e]
  (send-game game/select-only e))

;; API - Questions

(defn creature?
  [e]
  (game/creature? @game e))

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

(defn at-mouse
  ([]
   (pos/at @game :foo (mouse-world)))
  ([layer]
   (pos/at @game :foo (mouse-world) layer))
  ([layer index]
   (pos/at @game :foo (mouse-world) layer index)))

(defn creature-at-mouse
  []
  (->> (at-mouse pos/object-layer)
       (filter creature?)
       first))

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

(defmethod handle! :select
  [_]
  (when-let [e (creature-at-mouse)]
    (if @input-modifier
      (select! e)
      (select-only! e))))

(defmethod handle! :lasso
  [_]
  (let [[mx my] (mouse-screen-pixel)]
    (if (lassoing?)
      (swap! lasso
             #(let [[x y _ _ ox oy] %
                    w (Math/abs (int (- mx ox)))
                    h (Math/abs (int (- my oy)))
                    x (min ox mx)
                    y (min oy my)]
                (vector x y w h ox oy)))
      (reset! lasso (vector mx my 1 1 mx my)))))

(defmethod handle! :release-lasso
  [_]
  (let [[_ _ w h :as r] (unproject @lasso)
        xs (when (< 10 w) (< 10 h)
             (pos/in @game :foo pos/object-layer (rect/scale r (/ 1 32))))]
    (when xs
      (send-game #(-> (game/unselect-all %) (game/select-many xs))))
    (reset! lasso unit-rect)))

(defn handle-input!
  [input]
  (doseq [k (:pressed input)
          c (-> commands :down k)]
    (handle! c))
  (doseq [k (:hit input)
          c (-> commands :hit k)]
    (handle! c)))
