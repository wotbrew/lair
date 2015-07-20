(ns lair.ui
  (:require [lair.global :as global]
            [lair.gdx :as gdx]
            [lair.gfx :as gfx]
            [lair.game :as game]
            [lair.game.attr :as attr]))

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
  (vector (* 32 6) 0 (- width (* 32 12)) (- height (* 32 6))))

(defn left-rect
  [width height]
  (let [[gx gy gw gh] (game-rect width height)]
    (vector 0 0 (- gx 32) gh)))

(defn right-rect
  [width height]
  (let [[gx gy gw gh] (game-rect width height)]
    (vector (+ gx gw) 0 (- width gx gw 32) gh)))

(defn bottom-rect
  [width height]
  (let [[gx gy gw gh] (game-rect width height)]
    (vector gx (+ gy gh) gw (- height gh))))

(defn bottom-left-rect
  [width height]
  (let [[lx ly lw lh] (left-rect width height)]
    (vector lx (+ ly lh 32) lw (- height (+ ly lh 32)))))

(defn bottom-right-rect
  [width height]
  (let [[rx ry rw rh] (right-rect width height)]
    (vector rx (+ ry rh 32) rw (- height (+ ry rh 32)))))

(defn draw-player!
  [batch m e x y w h]
  (gfx/draw-box! batch x y w h
                 (if (game/selected? m e)
                   :green
                   :white))
  (gfx/draw-entity! batch m e (+ x 48) (+ y 48) 64 64))

(defrecord PlayerPanel [x y w h]
  IDraw
  (draw! [this batch x y]
    (let [g @global/game]
      (loop [i 0
             players (take 3 (attr/with g :type :creature))]
        (when-let [[p & tail] (seq players)]
          (draw-player! batch g p x (+ y i) w 192)
          (recur (+ i 192) tail))))))

(defn left-panel
  [width height]
  (let [[x y w h] (left-rect width height)]
    (many
     [(fill x y w h :black)
      (tiled (+ x w) y 32 h :blank)
      (->PlayerPanel x y w h)])))

(defn right-panel
  [width height]
  (let [[x y w h] (right-rect width height)]
    (many
     [(fill x y w h :black)
      (tiled (- x 32) y 32 h :blank)])))

(defn bottom-panel
  [width height]
  (let [[x y w h] (bottom-rect width height)]
    (many
     [(fill x y w h :black)
      (tiled x y w 32 :blank)])))

(defn bottom-left-panel
  [width height]
  (let [[x y w h] (bottom-left-rect width height)]
    (many
     [(fill x y w h :black)
      (tiled x (- y 32) (+ w 32) 32 :blank)
      (tiled (+ x w) y 32 (+ y h) :blank)])))

(defn bottom-right-panel
  [width height]
  (let [[x y w h] (bottom-right-rect width height)]
    (many
     [(fill x y w h :black)
      (tiled x (- y 32) w 32 :blank)
      (tiled (- x 32) y 32 (+ y h) :blank)])))


(defn create-main
  [width height]
  (let [[gx gy gw gh] (game-rect width height)]
    (many
     [(left-panel width height)
      (right-panel width height)
      (bottom-panel width height)
      (bottom-left-panel width height)
      (bottom-right-panel width height)
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
