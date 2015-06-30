(ns lair.gfx
  (:require [lair.gdx :as gdx]
            [clojure.java.io :as io]
            [clojure.edn :as edn]
            [potemkin :as potemkin]
            [lair.game :as game]
            [lair.game.pos :as pos]
            [lair.game.attr :as attr]))

(defonce texture
  (potemkin/fast-memoize
    (fn [file]
      @(gdx/go (gdx/texture file)))))

(defonce region
  (potemkin/fast-memoize
    (fn [file x y w h]
      (if-let [t (texture file)]
        (doto (gdx/texture-region t x y w h)
          (.flip false true))
        (throw (Exception. (str "Not found: " file)))))))

(def sprite
  (edn/read-string (slurp (io/resource "sprites.edn"))))

(def sprite-region
  (memoize (fn [key]
             (when-let [[file [x y w h]] (sprite key)]
               (region (io/resource (str "tiles/" file ".png")) x y w h)))))

(def color
  (comp (fnil gdx/color gdx/white) (edn/read-string (slurp (io/resource "colors.edn")))))

(defn draw-sprite!
  ([batch sprite x y w h]
   (when-let [region (sprite-region sprite)]
     (gdx/draw-region! batch region x y w h)))
  ([batch sprite x y w h color]
    (gdx/with-color batch (lair.gfx/color color) (draw-sprite! batch sprite x y w h))))

(defmulti draw-entity!* (fn [batch m e x y w h] (attr/find m e :type)))

(defmethod draw-entity!* :default
  [batch m e x y w h]
  (draw-sprite! batch (attr/find m e :sprite) x y w h))

(defmethod draw-entity!* :creature
  [batch m e x y w h]
  (let [atts (attr/all m e)]
    (when (:selected? atts)
      (draw-sprite! batch :selected x y w h :green))
    (draw-sprite! batch (:sprite atts) x y w h)))

(defn draw-entity!
  ([batch m e rect]
    (let [[x y w h] (gdx/rect rect)]
      (draw-entity! batch m e x y w h)))
  ([batch m e x y w h]
    (draw-entity!* batch m e x y w h)))

(defn draw-map!
  [batch m map]
  (pos/sort-run! m map
    (fn [[x y] layer e]
      (draw-entity! batch m e (* x 32) (* y 32) 32 32))))

