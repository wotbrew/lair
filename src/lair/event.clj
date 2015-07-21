(ns lair.event
  (:require [lair.global :as global]
            [lair.game :as game]
            [lair.game
             [pos :as pos]
             [attr :as attr]]
            [lair.ui :as ui]
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

(defmethod handle! :select-player
  [m]
  (if-let [index (:index m)]
    (when-let [player (global/playern index)]
      (handle! {:type   :select-player
                :entity player}))
    (when-let [e (:entity m)]
      (if @global/input-modifier
        (global/select! e)
        (global/select-only! e)))))

(defmethod handle! :move-selected-to
  [m]
  (when-let [pt (:pt m)]
    (doseq [e (global/selected)]
      (ai/remember! e :move-to pt))))

(defmethod handle! :move-selected-to-mouse
  [m]
  (handle! {:type :move-selected-to
            :pt (global/mouse-world)}))

(defmethod handle! :select-at-mouse
  [m]
  (if-let [e (global/creature-at-mouse)]
    (handle! {:type :select-player
              :entity e})
    (handle! :move-selected-to-mouse)))

(defmethod handle! :select-game
  [_]
  (when-not (global/lassoing? 10 10)
    (handle! :select-at-mouse)))

(defmethod handle! :select
  [_]
  (when-let [click-event (ui/click-event (deref @ui/ui))]
    (handle! click-event)))

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
