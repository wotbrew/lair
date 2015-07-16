(ns lair.event
  (:require [lair.global :as global]
            [lair.game :as game]
            [lair.game
             [pos :as pos]
             [attr :as attr]]
            [lair.rect :as rect]
            [lair.ai :as ai]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.tools.logging :refer [info warn trace]]))

(defmulti handle! (some-fn :type identity))

(defmethod handle! :default
  [m]
  (trace "Unknown event" m))

(defmethod handle! :created
  [m]
  (let [attrs (:attrs m)]
    (when (= (:type attrs) :creature)
      (ai/spawn (:entity m)))))

(def commands (edn/read-string (slurp (io/resource "commands.edn"))))

(defmethod handle! :modifier-down
  [_]
  (reset! global/input-modifier true))

(defmethod handle! :modifier-up
  [_]
  (reset! global/input-modifier false))

(defmethod handle! :cam-up
  [_]
  (global/shift-cam! 0 (* -1 (global/current-cam-speed))))

(defmethod handle! :cam-down
  [_]
  (global/shift-cam! 0 (global/current-cam-speed)))

(defmethod handle! :cam-left
  [_]
  (global/shift-cam! (* -1 (global/current-cam-speed)) 0))

(defmethod handle! :cam-right
  [_]
  (global/shift-cam! (global/current-cam-speed) 0))

(defmethod handle! :select
  [_]
  (when-not (global/lassoing? 10 10)
    (if-let [e (global/creature-at-mouse)]
      (if @global/input-modifier
        (global/select! e)
        (global/select-only! e))
      (if-let [s (seq (global/selected))]
        (doseq [e s]
          (ai/remember! e :move-to (global/mouse-world)) )))))

(defmethod handle! :lasso
  [_]
  (let [[mx my] (global/mouse-screen-pixel)]
    (if (global/lassoing?)
      (swap! global/lasso
             #(let [[_ _ _ _ ox oy] %
                    w (Math/abs (int (- mx ox)))
                    h (Math/abs (int (- my oy)))
                    x (min ox mx)
                    y (min oy my)]
                (vector x y w h ox oy)))
      (reset! global/lasso (vector mx my 1 1 mx my)))))

(defmethod handle! :release-lasso
  [_]
  (let [[_ _ w h :as r] (global/unproject @global/lasso)
        xs (when (< 10 w) (< 10 h)
             (pos/in @global/game :foo pos/object-layer (rect/scale r (/ 1 32))))]
    (when xs
      (global/send-game #(-> (game/unselect-all %) (game/select-many xs))))
    (reset! global/lasso global/unit-rect)))


(defn publish!
  [event-or-events]
  (when event-or-events
    (if (sequential? event-or-events)
      (doseq [e event-or-events]
        (publish! e))
      (handle! event-or-events))))

(defn publish-input!
  [input]
  (doseq [k (:pressed input)
          c (-> commands :down k)]
    (handle! c))
  (doseq [k (:hit input)
          c (-> commands :hit k)]
    (handle! c)))
