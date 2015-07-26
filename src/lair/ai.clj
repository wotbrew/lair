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
  ([e k v]
   (swap! ai-state assoc-in [e k] v))
  ([e k v & kvs]
   (swap! ai-state #(reduce (fn [m [k v]] (assoc-in m [e k] v)) (assoc-in % [e k] v) (partition 2 kvs)))))

(defn forget!
  ([e k]
   (swap! ai-state util/dissoc-in [e k]))
  ([e k & ks]
   (swap! ai-state #(reduce (fn [m k] (util/dissoc-in m [e k])) (util/dissoc-in % [e k]) ks))))

(defn recall
  [e k]
  (-> (get @ai-state e)
      (get k)))

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
      (when-let [goal (recall e :goal)]
        (if-let [pth (seq (rest @(global/path e goal)))]
          (do (remember! e
                         :path pth
                         :path-goal goal)
              :done)
          (do (forget! e :path :path-goal)
              :exit))))))

(defrecord Step []
  IBehaviour
  (should-perform? [this e]
    (when-let [path (seq (recall e :path))]
      (point/adjacent? (first path) (global/point-of e))))
  (perform! [this e]
    (async/go
      (when-let [path (seq (recall e :path))]
        (global/step! e (first path))
        (remember! e :path (rest path))
        :done))))

(defrecord Look []
  IBehaviour
  (should-perform? [this e]
    (and (global/pos-of e)
         (recall e :relook?)))
  (perform! [this e]
    (async/go
      (when-let [i (recall e :relook?)]
        (global/look! e)
        (swap! ai-state update-in [e :relook?]
               #(if (= % i)
                  nil
                  %)))
      :done)))

(def player-tree
  (->PEvery
   [(->Every
     [(->FindPath)
      (->Step)])
    (->Look)]))

(defrecord Examine []
  IBehaviour
  (should-perform? [this e]
    true)
  (perform! [this e]
    (async/go
      (let [vis (global/visible-entities e)]))))

(def enemy-tree
  (->PEvery
   [(->Every
     [(->FindPath)
      (->Step)])
    (->Look)
    (->Examine)]))

(defn ai-tick
  [e]
  (async/go
    (when-let [tree (recall e :tree)]
      (<! (perform! tree e)))
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
