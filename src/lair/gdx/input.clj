(ns lair.gdx.input
  (:require [clojure.string :as str])
  (:import (com.badlogic.gdx Gdx Input$Buttons Input$Keys)))

(defn get-x
  []
  (.getX Gdx/input))

(defn get-y
  []
  (.getY Gdx/input))

(defn mouse-point
  "Find the current mouse point for the given IInput"
  []
  (vector (get-x) (get-y)))

(defn key-pressed?
  [key]
  (.isKeyPressed Gdx/input key))

(defn button-pressed?
  [button]
  (.isButtonPressed Gdx/input button))

(def key-up?
  (complement key-pressed?))

(def button-up?
  (complement button-pressed?))

(def key-nums
  (into {}
        (for [n (range 0 10)
              :let [i (+ 7 n)]]
          [(keyword (str "num" n)) i])))

(def key-chars
  (into {}
        (for [n (range 0 26)
              :let [i (+ 65 n)
                    ci (+ 29 n)]]
          [(keyword (str/lower-case (str (char i)))) ci])))

(def key-map
  (merge {:any    -1
          :lshift Input$Keys/SHIFT_LEFT
          :rshift Input$Keys/SHIFT_RIGHT
          :esc    Input$Keys/ESCAPE
          :space  Input$Keys/SPACE
          :f1     Input$Keys/F1
          :f2     Input$Keys/F2
          :f3     Input$Keys/F3
          :f4     Input$Keys/F4
          :f5     Input$Keys/F5
          :f6     Input$Keys/F6
          :f7     Input$Keys/F7
          :f8     Input$Keys/F8
          :f9     Input$Keys/F9
          :f10    Input$Keys/F10
          :f11    Input$Keys/F11
          :f12    Input$Keys/F12}
         key-nums
         key-chars))

(def button-map
  {:left  Input$Buttons/LEFT
   :right Input$Buttons/RIGHT})

(defn keys-pressed
  []
  (for [[key real-key] key-map
        :when (key-pressed? real-key)]
    key))

(defn buttons-pressed
  []
  (for [[button real-button] button-map
        :when (button-pressed? real-button)]
    button))

(defn current
  []
  {:pressed     (into #{} cat (vector (buttons-pressed) (keys-pressed)))
   :mouse-point (mouse-point)})

(defn hit
  [prev-state]
  (for [p (:pressed prev-state)
        :when (or (when-let [k (get key-map p)] (key-up? k))
                  (when-let [b (get button-map p)] (button-up? b)))]
    p))

(defn next-input
  [prev-state new-state]
  (assoc new-state
    :hit (hit prev-state)))