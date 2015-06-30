(ns lair.game.attr
  (:require [lair.util :as util])
  (:refer-clojure :exclude [find update remove merge]))

(defn all
  [m e]
  (-> m ::eav (get e)))

(defn find
  [m e k]
  (-> m ::eav (get e) (get k)))

(defn with
  [m k v]
  (-> m ::ave (get k) (get v)))

(defn index-eav
  [m e k v]
  (assoc-in m [::eav e k] v))

(defn unindex-eav
  [m e k]
  (util/dissoc-in m [::eav e k]))

(defn index-ave
  [m e k v]
  (update-in m [::ave k v] util/set-conj e))

(defn unindex-ave
  ([m e k v]
   (util/disjoc-in m [::ave k v] e)))

(defn unindex
  [m e k]
  (-> (unindex-ave m e k (find m e k))
      (unindex-eav e k)))

(defn index
  [m e k v]
  (-> (unindex m e k)
      (index-eav e k v)
      (index-ave e k v)))

(defn add
  ([m e k v]
    (index m e k v))
  ([m e k v & kvs]
    (reduce #(add %1 e (first %2) (second %2))
            (add m e k v)
            (partition 2 kvs))))

(defn merge
  ([m e attrs]
    (reduce-kv #(add %1 e %2 %3) m attrs))
  ([m e attrs & more]
    (reduce #(merge %1 e %2) m (cons attrs more))))

(defn remove
  ([m e k]
    (unindex m e k))
  ([m e k & ks]
    (reduce #(remove %1 e %2) (remove m e k) ks)))

(defn clear
  [m e]
  (reduce-kv (fn [m k _] (remove m e k)) m (all m e)))

(defn update
  ([m e k f]
    (let [v (find m e k)]
      (add m e k (f v))))
  ([m e k f & args]
    (update m e k #(apply f % args))))