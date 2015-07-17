(ns lair.ui
  (:require [lair.global :as global]
            [lair.gdx :as gdx]
            [lair.gfx :as gfx]))

(defprotocol IDraw
  (draw! [this batch x y]))

(defrecord Label [x y text-fn font]
  IDraw
  (draw! [this batch x y]
    (gdx/draw-text! batch font (text-fn) x y)))

(defn label*
  ([x y text]
   (label* x y text @global/font))
  ([x y text font]
   (->Label x y (if (fn? text) text (constantly text)) font)))

(defmacro label
  ([text]
   `(label 0 0 ~text))
  ([x y text]
   `(label ~x ~y ~text @global/font))
  ([x y text font]
   `(label* ~x ~y (fn [] (str ~text)) ~font)))

(defrecord Many [x y x-step y-step controls]
  IDraw
  (draw! [this batch x y]
    (loop [controls controls
           x (int (+ x (:x this)))
           y (int (+ y (:y this)))]
      (when-let [[c & rst] (seq controls)]
        (draw! c batch x y)
        (recur rst (+ x x-step) (+ y y-step))))))

(defn many
  ([controls]
   (many 0 controls))
  ([y-step controls]
   (many 0 0 0 y-step controls))
  ([x-step y-step controls]
   (many 0 0 x-step y-step controls))
  ([x y x-step y-step controls]
   (->Many x y x-step y-step controls)))

(defrecord Lasso [color]
  IDraw
  (draw! [this batch _ _]
    (gfx/draw-box! batch @global/lasso color)))

(defrecord Mouse []
  IDraw
  (draw! [this batch _ _]
    (let [[x y] (global/mouse-screen-pixel)]
      (gfx/draw-sprite! batch :mouse x y 32 32))))

(defrecord FillPanel [x y w h color]
  IDraw
  (draw! [this batch x2 y2]
    (gfx/draw-filled-rect! batch (+ x x2) (+ y y2) w h color)))

(defn fill
  [x y w h color]
  (->FillPanel x y w h color))

(defrecord TilePanel [x y w h sprite]
  IDraw
  (draw! [this batch x2 y2]
    (let [mx (int (+ x x2 w))
          my (int (+ y y2 h))]
      (loop [x (int (+ x x2))]
        (when (<= (+ x 32) mx)
          (loop [y (int (+ y y2))]
            (when (<= (+ y 32) my)
              (gfx/draw-sprite! batch sprite x y 32 32)
              (recur (+ y 32))))
          (recur (+ x 32)))))))

(defn tiled
  [x y w h sprite]
  (->TilePanel x y w h sprite))

(defn game-rect
  [width height]
  (vector (* 32 5) 0 (- width (* 32 10)) (- height (* 32 6))))

(defn create-main
  [width height]
  (let [[gx gy gw gh] (game-rect width height)]
    (many
     [(fill 0 0 gx height :black)
      (fill (+ gx gw) 0 gx height :black)
      (fill 0 gh width (- height gh) :black)
      (tiled (- gx 32) 0 32 height :blank)
      (tiled 0 gh width 32 :blank)
      (tiled (+ gx gw) 0 32 height :blank)
      (many gx 0 0 16
            [(label (gdx/fps))
             (label (global/mouse-world))])
      (->Lasso :green)
      (->Mouse)])))

(def ui (atom (delay @(gdx/go (create-main 1024 768)))))

(defn draw-debug!
  [batch x y]
  (let [font @global/font]
    (gdx/draw-text! batch font (str (gdx/fps)) x y)
    (gdx/draw-text! batch font (str (global/mouse-world)) x (+ y 16))))

(defn draw-lasso!
  [batch]
  (gfx/draw-box! batch @global/lasso :green))

(defn draw-ui!
  [batch game]
  (when-let [c (deref @ui)]
    (draw! c batch 0 0))
  #_(draw-lasso! batch)
  #_(draw-debug! batch 0 0))
