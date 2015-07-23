(ns lair.ai
  (:require [lair.global :as global]
            [lair.game :as game]
            [lair.util :as util]
            [lair.point :as point]
            [lair.shape :as shape]
            [clojure.tools.logging :refer [info error]]
            [clojure.core.async :as async :refer [<! >!]]))

(defmacro go
  [msg & body]
  `(async/go
     (try
       ~@body
       (catch Throwable e#
         (error e# (format "An error occurred in ai thread (%s)" ~msg))
         (throw e#)))))

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

(defprotocol IBehaviour
  (should-perform? [this e])
  (perform! [this e]))

(defrecord Every [behaviours]
  IBehaviour
  (should-perform? [this e]
    true)
  (perform! [this e]
    (async/go
      (loop [behaviours behaviours]
        (when-let [[head & tail] (seq behaviours)]
          (when (should-perform? head e)
            (<! (perform! head e)))
          (recur tail))))))

(defrecord PEvery [behaviours]
  IBehaviour
  (should-perform? [this e]
    true)
  (perform! [this e]
    (async/merge
     (for [b behaviours
           :when (should-perform? b e)]
       (perform! b e)))))

(defrecord OneOf [behaviours]
  IBehaviour
  (should-perform? [this e]
    true)
  (perform! [this e]
    (async/go
      (loop [behaviours behaviours]
        (when-let [[head & tail] (seq behaviours)]
          (or (when (should-perform? head e) (<! (perform! head e)))
              (recur tail)))))))

(defrecord Sequence [behaviours]
  IBehaviour
  (should-perform? [this e]
    true)
  (perform! [this e]
    (async/go
      (loop [behaviours behaviours]
        (when-let [[head & tail] (seq behaviours)]
          (or (when (should-perform? head e)
                (not= :exit (<! (perform! head e))))
              (recur tail)))))))

(defrecord FindPath []
  IBehaviour
  (should-perform? [this e]
    (let [{:keys [path path-goal goal]} (get @ai-state e)]
      (when goal
        (or (empty? path)
            (not= goal path-goal)
            (not (point/adjacent? (first path) (global/point-of e)))))))
  (perform! [this e]
    (async/go
      (when-let [goal (-> @ai-state (get e) :goal)]
        (if-let [pth (seq (rest @(global/path e goal)))]
          (do (remember! e :path pth)
              (remember! e :path-goal goal)
              :done)
          (do (forget! e :path :path-goal)
              :exit))))))

(defrecord Step []
  IBehaviour
  (should-perform? [this e]
    (when-let [path (seq (-> @ai-state (get e) :path))]
      (point/adjacent? (first path) (global/point-of e))))
  (perform! [this e]
    (async/go
      (when-let [path (seq (-> @ai-state (get e) :path))]
        (global/step! e (first path))
        (remember! e :path (rest path))
        :done))))

(defrecord Look []
  IBehaviour
  (should-perform? [this e]
    (some? (global/pos-of e)))
  (perform! [this e]
    (async/go
      (let [pos (global/pos-of e)
            [x y] (:pt pos)
            map (:map pos)
            fov (game/fov @global/game e)]
        (global/send-game game/explore map fov)))))

(def player-tree
  (->PEvery
   [(->Every
     [(->FindPath)
      (->Step)])
    (->Look)]))


(defn ai-tick
  [e]
  (async/go
    (<! (perform! player-tree e))
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
