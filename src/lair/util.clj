(ns lair.util)

;;util
(defmacro xor
  ([] false)
  ([a] true)
  ([a b]
   `(if ~a
      (if ~b false true)
      (if ~b true false)))
  ([a b & more]
   `(xor  (xor ~a ~b) (xor ~@more))))

(def vec-conj (fnil conj []))
(def set-conj (fnil conj #{}))
(def sorted-assoc (fnil assoc (sorted-map)))

(defn sorted-assoc-in
  [m [k & ks] v]
  (let [m (or m (sorted-map))]
    (if ks
      (assoc m k (sorted-assoc-in (get m k) ks v))
      (assoc m k v))))

(defn sorted-update-in
  ([m ks f & args]
    (sorted-update-in m ks #(apply f args)))
  ([m [k & ks] f]
   (let [m (or m (sorted-map))]
     (if ks
       (assoc m k (sorted-update-in (get m k) ks f))
       (update m k f)))))

(defn disjoc
  [m k x]
  (if-let [coll (not-empty (disj (get m k) x))]
    (assoc m k coll)
    (dissoc m k)))

(defn disjoc-in
  [m [k & ks] x]
  (if ks
    (if-let [nextmap (get m k)]
      (let [newmap (disjoc-in nextmap ks x)]
        (if (seq newmap)
          (assoc m k newmap)
          (dissoc m k)))
      m)
    (disjoc m k x)))

(defn dissoc-in
  "Dissociates an entry from a nested associative structure returning a new
  nested structure. keys is a sequence of keys. Any empty maps that result
  will not be present in the new structure."
  [m [k & ks]]
  (if ks
    (if-let [nextmap (get m k)]
      (let [newmap (dissoc-in nextmap ks)]
        (if (seq newmap)
          (assoc m k newmap)
          (dissoc m k)))
      m)
    (dissoc m k)))