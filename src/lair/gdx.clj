(ns lair.gdx
  "Contains LibGDX primitives we will use to build the game"
  (:require [lair.gdx.cam :as cam]
            [lair.gdx.pixmap :as pixmap])
  (:import (com.badlogic.gdx.files FileHandle)
           (java.io File)
           (java.net URL)
           (com.badlogic.gdx.graphics Texture GL20 Color OrthographicCamera)
           (com.badlogic.gdx.graphics.g2d BitmapFont TextureRegion SpriteBatch)
           (com.badlogic.gdx Gdx ApplicationListener)
           (java.util List Map)
           (clojure.lang IPersistentVector)
           (com.badlogic.gdx.backends.lwjgl LwjglApplicationConfiguration LwjglApplication)))

(defn fps
  "Get the current frames per second"
  []
  (. Gdx/graphics getFramesPerSecond))

(defn delta
  "Get the current delta time"
  []
  (. Gdx/graphics getDeltaTime))

(defprotocol IRectable
  (->rect [thing]
    "Generic constructor for rectangles
     takes vectors or maps of :x, :y, :width, :height"))

(extend-protocol IRectable
  IPersistentVector
  (->rect [thing] thing)
  Map
  (->rect [{:keys [x y width height]}]
    (vector x y width height)))

(defn rect
  "Generic constructor for rectangles
   takes vectors or maps of :x, :y, :width, :height"
  ([thing] (->rect thing))
  ([x y w h]
    (vector x y w h)))

(defprotocol IFileHandleable
  (->file-handle [this]
    "Generic constructor for file-handles
     takes a single argument, strings, URL's and File's are supported"))


(extend-protocol IFileHandleable
  FileHandle
  (->file-handle [this]
    this)
  File
  (->file-handle [this]
    (FileHandle. this))
  String
  (->file-handle [this]
    (FileHandle. this))
  URL
  (->file-handle [this]
    (FileHandle. (.getPath this))))

(defn texture
  "Loads a texture"
  [file]
  (Texture. ^FileHandle (->file-handle file)))

(defn font
  "Creates a BitmapFont"
  ([]
   (BitmapFont.))
  ([file]
   (BitmapFont. ^FileHandle (->file-handle file))))

(defn flipped-font
  "Creates a BitmapFont, flips the `y` axis"
  ([]
   (BitmapFont. true))
  ([file]
   (BitmapFont. ^FileHandle (->file-handle file) true)))

(defprotocol ITextureRegionable
  (->texture-region [this x y w h]
    "Derive a new texture region for the given rectangle"))

(extend-protocol ITextureRegionable
  Texture
  (->texture-region [this x y w h]
    (TextureRegion. this (int x) (int y) (int w) (int h)))
  TextureRegion
  (->texture-region [this x y w h]
    (TextureRegion. this (int x) (int y) (int w) (int h))))

(defn texture-region
  "Creates a texture region from the source
   (being either an existing texture region or a texture)"
  ([thing rect]
   (let [[x y w h] (->rect rect)]
     (texture-region thing x y w h)))
  ([thing x y w h]
   (->texture-region thing x y w h)))

(defn clear!
  "Clears the screen"
  []
  (.glClearColor Gdx/gl  0 0 0 0)
  (.glClear Gdx/gl GL20/GL_COLOR_BUFFER_BIT))

(defprotocol IColorable
  (->color [thing]
    "Generic constuctor for colors
     by default takes a vector or a map of :r, :g, :b, :a
     or a hex string"))

(extend-protocol IColorable
  Color
  (->color [this] this)
  List
  (->color [[r g b a]]
    (Color. (int r) (int g) (int b) (int a)))
  Map
  (->color [{:keys [r g b a]}]
    (Color. (int r) (int g) (int b) (int a)))
  String
  (->color [s]
    (Color/valueOf s)))

(defn color
  ([thing]
    (->color thing))
  ([r g b a]
    (Color. (int r) (int g) (int b) (int a))))

(def black (Color/BLACK))
(def blue (Color/BLUE))
(def clear (Color/CLEAR))
(def cyan (Color/CYAN))
(def dark-gray (Color/DARK_GRAY))
(def gray (Color/GRAY))
(def green (Color/GREEN))
(def light-gray (Color/LIGHT_GRAY))
(def magneta (Color/MAGENTA))
(def maroon (Color/MAROON))
(def navy (Color/NAVY))
(def olive (Color/OLIVE))
(def orange (Color/ORANGE))
(def pink (Color/PINK))
(def purple (Color/PURPLE))
(def red (Color/RED))
(def teal (Color/TEAL))
(def white (Color/WHITE))
(def yellow (Color/YELLOW))

(defn pixel
  "Creates a basic solid 1x1 texture for use in drawing primitive shapes
   if a color is not supplied, white is used.

   Must be called on the render thread."
  ([]
   (pixel white))
  ([color]
   (pixmap/texture (pixmap/pixel (->color color)))))

(defn batch
  "Creates a SpriteBatch"
  []
  (SpriteBatch.))

(defmacro with-batch
  "Executes the forms with between `(.begin batch)` and `(.end batch)`"
  [batch & forms]
  `(let [^SpriteBatch batch# ~batch]
     (try
       (.begin batch#)
       ~@forms
       (finally
         (.end batch#)))))

(defn set-font-color!
  "Sets the color used for the given font"
  ([^BitmapFont font r g b a]
   (.setColor font r g b a))
  ([^BitmapFont font ^Color color]
   (.setColor font color)))

(defn set-color!
  "Sets the color used by the current sprite batch"
  ([batch r g b a]
   (.setColor batch r g b a))
  ([batch ^Color color]
   (.setColor batch color)))

(defmacro with-font-color
  "Perform the body using the given font color"
  [font color & body]
  `(let [old# (.getColor font)]
     (set-font-color! font ~color)
     ~@body
     (set-font-color! font old#)))

(defmacro with-color
  "Perform any sprite batch operations in the body using the given color"
  [batch color & body]
  `(let [old# (.getColor batch)]
     (set-color! batch ~color)
     ~@body
     (set-color! batch old#)))

(defn camera
  "Creates a new orthographic camera"
  [width height]
  (OrthographicCamera. width height))

(defn flipped-camera
  "Creates a new orthographic camera with a flipped `y` axis.
   so that +y is down, not up"
  [width height]
  (doto (camera width height)
    (.setToOrtho true)))

(defmacro with-camera
  "Perform the body using the given camera"
  [batch camera & body]
  `(let [batch# ~batch
         proj# (.cpy (.getProjectionMatrix batch#))]
     (.setProjectionMatrix batch# (cam/combined ~camera))
     ~@body
     (.setProjectionMatrix batch# proj#)))

(defn draw-texture!
  ([batch texture rect]
    (draw-texture! batch texture (->rect rect)))
  ([batch texture x y w h]
   (.draw ^SpriteBatch batch ^Texture texture (float x) (float y) (float w) (float h))))

(defn draw-region!
  ([batch region rect]
    (draw-region! batch region (->rect rect)))
  ([batch region x y w h]
   (.draw ^SpriteBatch batch ^TextureRegion region (float x) (float y) (float w) (float h))))

(defn draw-text!
  ([batch font string x y]
   (.drawMultiLine font batch string x y))
  ([batch font string x y w]
   (.drawWrapped font batch string (float x) (float y) (float w))))


(def ^:dynamic *on-render-thread* false)

(defn gdx-dispatch!
  [f]
  (let [p (promise)
        f #(let [r (try (binding [*on-render-thread* true] (f)) (catch Throwable e e))]
            (p r)
            (when (fn? r)
              (gdx-dispatch! f)))]
    (if *on-render-thread*
      (f)
      (.postRunnable Gdx/app f))
    (delay (let [x (deref p)]
             (if (instance? Throwable x)
               (throw x)
               x)))))

(defmacro go
  [& forms]
  `(gdx-dispatch! (fn [] ~@forms)))

(def listener
  (proxy
    [ApplicationListener]
    []
    (pause [])
    (resume [])
    (dispose [])
    (create [])
    (render [])
    (resize [x y])))


(def default-configuration
  {:width 1024
   :height 768
   :fullscreen? false
   :title "Untitled"
   :vsync? false
   :max-fps 60
   :resizable? false})


(defn map->lwjgl-configuration
  [m]
  (let [cfg (LwjglApplicationConfiguration.)
        m (merge default-configuration m)]
    (set! (. cfg width) (m :width))
    (set! (. cfg height) (m :height))
    (set! (. cfg fullscreen) (m :fullscreen?))
    (set! (. cfg title) (m :title))
    (set! (. cfg vSyncEnabled) (m :vsync?))
    (set! (. cfg foregroundFPS) (m :max-fps))
    (set! (. cfg backgroundFPS) (m :max-fps))
    (set! (. cfg resizable) (m :resizable?))
    cfg))

(defn app
  [config]
  (LwjglApplication. listener (map->lwjgl-configuration config)))