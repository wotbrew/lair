(ns lair.gdx.cam
  (:import (com.badlogic.gdx.graphics Camera)
           (com.badlogic.gdx.math Vector3)))

(defn move!
  [^Camera cam x y]
  (.set (.position cam)
        x
        y
        0))

(defn size
  [^Camera cam]
  (vector (int (.viewportWidth cam))
          (int (.viewportHeight cam))))

(defn default-pos
  [^Camera cam]
  (vector (/ (.viewportWidth cam) 2)
          (/ (.viewportHeight cam) 2)))

(defn move-to-default!
  [cam]
  (let [[x y] (default-pos cam)]
    (move! cam x y)))

(defn update!
  [^Camera cam]
  (.update cam))

(defn project
  ([^Camera cam x y]
   (let [vec (Vector3. x y 1)]
     (.project cam vec)
     (vector (int (.x vec))
             (int (.y vec))))))

(defn unproject
  ([^Camera cam x y]
   (let [vec (Vector3. x y 1)]
     (.unproject cam vec)
     (vector (int (.x vec))
             (int (.y vec))))))

(defn combined
  [^Camera cam]
  (.combined cam))

(defn position
  [^Camera cam]
  (vector (.x ^Vector3 (.position cam))
          (.y ^Vector3 (.position cam))))

(defn shift!
  [^Camera cam x y]
  (.translate cam (float x) (float y) 0))
