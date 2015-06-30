(ns lair.gdx.pixmap
  (:import (com.badlogic.gdx.graphics Texture Pixmap$Format Pixmap Color)))

(def formats
  "A map of pixmap formats"
  {:alpha           Pixmap$Format/Alpha
   :intensity       Pixmap$Format/Intensity
   :luminance-alpha Pixmap$Format/LuminanceAlpha
   :rgb-565         Pixmap$Format/RGB565
   :rgb-888         Pixmap$Format/RGB888
   :rgba-4444       Pixmap$Format/RGBA4444
   :rgba-8888       Pixmap$Format/RGBA8888})

(defn pixmap
  "Creates a new pixmap with the given format.
   (formats are given by the `formats` map)

   pixmaps must be created on the render thread"
  ([x y format]
   (Pixmap. ^int x ^int y ^Pixmap$Format (formats format format)))
  ([x y]
   (pixmap x y :rgba-8888)))

(defn set-color!
  "Sets the color used by drawing operations on the pixmap"
  ([^Pixmap pixmap r g b a]
   (.setColor pixmap r g b a))
  ([^Pixmap pixmap color]
   (.setColor pixmap ^Color color)))

(defn fill
  "Fills the pixmap with solid color"
  ([^Pixmap pixmap]
   (.fill pixmap))
  ([^Pixmap pixmap color]
   (set-color! pixmap color)
   (fill pixmap)))

(defn pixel
  "Creates a single pixel pixmap of the given color"
  [color]
  (doto (pixmap 1 1)
    (fill color)))

(defn texture
  "Creates a texture from the pixmap.

  Must be called on the render thread"
  [^Pixmap pixmap]
  (Texture. pixmap))