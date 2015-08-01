(ns lair.core
  (:require [clj-tuple :as tuple]
            [lair.gdx :as gdx]
            [lair.gfx :as gfx]
            [lair.ui :as ui]
            [lair.global :as global]
            [lair.event :as event]
            [clojure.tools.logging :refer [error info warn]]
            [lair.gdx.cam :as cam]
            [lair.anim :as anim])
  (:import [java.util.concurrent ExecutorService]))

;; GO FASTER STRIPES
(require '[clj-tuple :as tuple])
(alter-var-root #'clojure.core/vector (constantly tuple/vector))
(alter-var-root #'clojure.core/hash-map (constantly tuple/hash-map))

(def ^ExecutorService event-executor clojure.lang.Agent/pooledExecutor)

;; EVENTS
(defn fire-events!
  [input]
  (let [game-events (promise)
        f (fn []
            (event/publish! @game-events)
            (event/publish-input! input))]
    (send global/game #(let [events (:events %)]
                         (deliver game-events events)
                         (dissoc % :events)))
    (.submit event-executor ^Callable f)))

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
          input (swap! global/input gdx/input)
          anims (future (anim/animate-all! (gdx/delta)))]
      (fire-events! input)
      (when (= (:screen (deref @ui/ui)) :main)
        (cam/update! game-camera)
        (gdx/with-batch
          batch
          (gdx/with-camera
            batch
            game-camera
            (gfx/draw-map! batch game :foo (global/ensure-cell-size)))))
      (cam/update! ui-camera)
      (gdx/with-batch
        batch
        (gdx/with-camera
          batch
          ui-camera
          (ui/draw-ui! batch game)))
      @anims)
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
