(ns lair.ui
  (:require [lair.global :as global]
            [lair.gdx :as gdx]
            [lair.gfx :as gfx]
            [lair.rect :as rect]
            [lair.game :as game]
            [lair.game.attr :as attr]))

(defn mouse-in?
  [x y w h]
  (let [[x2 y2] (global/mouse-screen-pixel)]
    (rect/contains-point? x y w h x2 y2)))

(defprotocol IDraw
  (draw! [this batch x2 y2]))

(defprotocol IClickable
  (click-event [this x2 y2]))

(extend-type Object
  IDraw
  (draw! [this batch x y])
  IClickable
  (click-event [this x2 y2]))

(defrecord Label [x y text-fn font]
  IDraw
  (draw! [this batch x2 y2]
    (gdx/draw-text! batch font (text-fn) (+ x x2) (+ y y2))))

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
  IClickable
  (click-event [this x2 y2]
    (some #(click-event % (+ x x2) (+ y y2)) controls))
  IDraw
  (draw! [this batch x2 y2]
    (loop [controls controls
           x (int (+ x x2))
           y (int (+ y y2))]
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
    (vector (+ gx gw 32) 0 (- width gx gw 32) gh)))

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

(defrecord PlayerPanel [x y w h playern sub]
  IClickable
  (click-event [this x2 y2]
    (when (mouse-in? (+ x x2) (+ y y2) w h)
      {:type  :select-player
       :index playern}))
  IDraw
  (draw! [this batch x2 y2]
    (let [m @global/game
          e (game/playern m playern)
          x (+ x x2)
          y (+ y y2)]
      (when e
        (gfx/draw-box! batch x y (dec w) (dec h)
                       (if (game/selected? m e)
                         :green
                         :white))
        (gfx/draw-entity! batch m e (+ x 48) (+ y 48) 64 64)
        (draw! sub batch x y)))))

(defn player-resource-label
  ([resource playern]
    (player-resource-label 0 0 resource playern))
  ([x y resource playern]
   (->>
     (str (name resource) ": "
          (when-let [e (global/playern playern)]
            (int (game/amount-of @global/game e resource))))
     (label x y))))

(defn player-panel
  [x y w h playern]
  (map->PlayerPanel
    {:x       x
     :y       y
     :w       w
     :h       h
     :playern playern
     :sub     (many
                [(label 16 16 (str "Entity: " (lair.global/playern playern)))
                 (many 16 116 0 18
                       [(player-resource-label :hp playern)
                        (player-resource-label :ap playern)
                        (player-resource-label :sp playern)
                        (player-resource-label :exp playern)])
                 (many 72 116 0 18
                       [(player-resource-label :enc playern)
                        (player-resource-label :faith playern)
                        (player-resource-label :fatigue playern)
                        (player-resource-label :morale playern)])])}))

(defn players-panel
  [x y w h startn]
  (let [n (int (/ h 192))]
    (many
      (into []
            (for [n (range startn n)]
              (player-panel x (+ (* n 192) y) w 192 n))))))

(defn left-panel
  [width height]
  (let [[x y w h] (left-rect width height)]
    (many
     [(fill x y w h :black)
      (tiled (+ x w) y 32 h :blank)
      (players-panel x y w h 0)])))

(defn right-panel
  [width height]
  (let [[x y w h] (right-rect width height)]
    (many
     [(fill x y w h :black)
      (tiled (- x 32) y 32 h :blank)
      (players-panel x y w h 3)])))

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
      (tiled (- x 32) (- y 32) (+ w 32) 32 :blank)
      (tiled (- x 32) y 32 (+ y h) :blank)])))

(defrecord GamePanel [x y w h]
  IClickable
  (click-event [this x2 y2]
    (if (mouse-in? (+ x x2) (+ y y2) w h)
      :select-game
      nil)))

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
             (label (global/mouse-world))
             (label (:time-mode @global/game))])
      (->Lasso :green)
      (->Mouse)
      (->GamePanel gx gy gw gh)])))

(def ui (atom (delay {:control @(gdx/go (create-main 1024 768))
                      :screen :main})))

(defn current-screen
  []
  (:screen (deref @ui)))

(defn main-screen!
  [width height]
  (reset! ui (delay {:control @(gdx/go (create-main 1024 768))
                     :screen :main})))

(defn inventory-screen!
  [width height]
  (reset! ui (delay {:screen :inventory})))

(defn draw-ui!
  [batch game]
  (when-let [ui (deref @ui)]
    (when-let [c (:control ui)]
      (draw! c batch 0 0))))
