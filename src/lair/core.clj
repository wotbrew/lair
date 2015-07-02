(ns lair.core
  (:require [clj-tuple :as tuple]
            [lair.gdx :as gdx]
            [lair.gfx :as gfx]
            [lair.global :as global]
            [clojure.tools.logging :refer [error info warn]]
            [lair.gdx.cam :as cam]))

;; GO FASTER STRIPES

(alter-var-root #'clojure.core/vector (constantly tuple/vector))
(alter-var-root #'clojure.core/hash-map (constantly tuple/hash-map))


;; MAIN LOOP

(defn frame!
  []
  (try
    (gdx/clear!)
    (let [batch @global/batch
          font @global/font
          game-camera @global/game-camera
          ui-camera @global/ui-camera
          game @global/game
          input (swap! global/input gdx/input)]
      (global/handle-input! input)
      (cam/update! game-camera)
      (gdx/with-batch
        batch
        (gdx/with-camera
          batch
          game-camera
          (gfx/draw-map! batch game :foo (global/ensure-cell-size))))
      (cam/update! ui-camera)
      (gdx/with-batch
        batch
        (gdx/with-camera
          batch
          ui-camera
          (gfx/draw-box! batch @global/lasso :green))))
    (catch Throwable e
      (error e "An error occurred rendering frame")
      (println e)
      (throw e))))

(defn begin-loop!
  []
  (future
    @(gdx/go (frame!)) (recur)))

(defn -main
  [& args]
  (gdx/app gdx/default-configuration)
  (begin-loop!))
