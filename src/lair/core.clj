(ns lair.core
  (:require [clj-tuple :as tuple]
            [lair.gdx :as gdx]
            [lair.gfx :as gfx]
            [lair.game :as game]
            [clojure.tools.logging :refer [error info warn]]
            [lair.game.pos :as pos]
            [lair.game.attr :as attr]
            [lair.gdx.cam :as cam]))

;; GO FASTER STRIPES

(alter-var-root #'clojure.core/vector (constantly tuple/vector))
(alter-var-root #'clojure.core/hash-map (constantly tuple/hash-map))

;; STATE

(defonce batch (delay @(gdx/go (gdx/batch))))
(defonce font (delay @(gdx/go (gdx/flipped-font))))
(defonce game-camera (delay @(gdx/go (gdx/flipped-camera 800 600))))
(defonce ui-camera (delay @(gdx/go (gdx/flipped-camera 800 600))))

(def game (agent (let [[m id] (game/creature nil)]
                   (-> m
                       (pos/put id [0 0] :foo pos/object-layer)))))


;; API

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
   (let [c @game-camera]
     (move-cam! x (+ y 32)))))

(defn look-at-map-point!
  "Points the camera at the given
   map co-ordinates"
  ([[x y]]
   (look-at-map-point! x y))
  ([x y]
   (look-at-pixel-point! (* x 32) (* y 32))))

(defn look-at-entity!
  "Points the camera at the given
   map co-ordinates"
  [e]
  (let [m @game]
    (if-let [{:keys [pt]} (pos/of m e)]
      (look-at-map-point! pt)
      (warn "Tried to point camera at entity that did not have a position"
            "entity:" e))))

(defn frame!
  []
  (try
    (gdx/clear!)
    (let [batch @batch
          font @font
          game-camera @game-camera
          game @game]
      (cam/update! game-camera)
      (gdx/with-batch
        batch
        (gdx/with-camera
          batch
          game-camera
          (gfx/draw-map! batch game :foo))))
    (catch Throwable e
      (error e "An error occurred rendering frame")
      (throw e))))

(defn begin-loop!
  []
  (future
    @(gdx/go (frame!)) (recur)))

(defn -main
  [& args]
  (gdx/app gdx/default-configuration)
  (begin-loop!))