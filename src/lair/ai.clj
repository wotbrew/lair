(ns lair.ai
  (:require [lair.global :as global]
            [lair.util :as util]
            [lair.point :as point]
            [clojure.tools.logging :refer [info]]
            [clojure.core.async :as async :refer [<! >!]]))


(def ai-threads (atom {}))
(def ai-state (atom {}))

(defn remember!
  [e k v]
  (swap! ai-state assoc-in [e k] v))

(defn forget!
  ([e k]
   (swap! ai-state util/dissoc-in [e k]))
  ([e k & ks]
   (swap! ai-state #(reduce (fn [m k] (util/dissoc-in m [e k])) (util/dissoc-in % [e k]) ks))))

(defn ai-tick-ms
  [e]
  100)

(defn remember!-path!
  [e goal]
  (async/go
    (if-let [pth (seq @(global/path e goal))]
      (do
        (remember! e :path pth)
        (remember! e :path-goal goal))
      (do (forget! e :path :path-goal)))))

(defn path-to!
  [e]
  (async/go
    (let [st (get @ai-state e)]
      (let [goal (:move-to st)
            path (:path st)
            path-goal (:path-goal st)]
        (cond
         (nil? goal) (forget! e :path)
         (nil? path) (<! (remember!-path! e goal))
         (not= goal path-goal) (<! (remember!-path! e goal))

         (not (point/adjacent? (first path) (global/point-of e)))
         (<! (remember!-path! e goal)))))))

(defn step!
  [e]
  (async/go
    (let [st (get @ai-state e)]
      (when-let [path (seq (:path st))]
        (global/step! e (first path))
        (remember! e :path (rest path))))))

(defn ai-tick
  [e]
  (async/go
    (<! (path-to! e))
    (<! (step! e))

    :wait))

(defn spawn
  [e]
  (info "Creating AI for" e)
  (when-not (get @ai-threads e)
    (swap! ai-threads assoc e :dummy)
    (swap! ai-threads assoc e
           (async/go-loop
               []
             (when (get @ai-threads e)
               (case (<! (ai-tick e))
                 :continue (recur)
                 :wait (do (<! (async/timeout (ai-tick-ms e)))
                           (recur))
                 (do (info "AI thread exiting")
                     (swap! ai-threads dissoc e)
                     (swap! ai-state dissoc e))))))))
