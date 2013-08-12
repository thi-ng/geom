(ns thi.ng.data.core)

(defn index
  [i t]
  (if-let [tt (get i t)] [i tt] [(conj i t) t]))

(defn index!
  [i t]
  (if-let [tt (get i t)] [i tt] [(conj! i t) t]))

(defn index-kv
  [m k v]
  (if-let [[k vv] (find m k)]
    (assoc m k (conj vv v))
    (assoc m k #{v})))

(defn index-kv!
  [m k v]
  (if-let [vv (get m k)]
    (assoc! m k (conj vv v))
    (assoc! m k #{v})))

(defn bisect
  ([f coll]
    (let [[m n] (reduce
                  (fn [[m n] v] (if (f v) [(conj! m v) n] [m (conj! n v)]))
                  [(transient []) (transient [])] coll)]
      [(persistent! m) (persistent! n)]))
  ([f f2 coll]
    (let [[m n] (bisect f coll)]
      [(f2 m) (f2 n)])))

(defn successive-nth
  "Returns a lazyseq of `n`-element vectors, each one containing
  a successive elements of the original collection.

      (successive-nth 3 [1 2 3 4])
      => ([1 2 3] [2 3 4] [3 4 5])"
  [n coll]
  (lazy-seq
    (let [s (take n coll)]
      (if (= n (count s))
        (cons (vec s) (successive-nth n (rest coll)))))))

(defn successive-nth-indexed
  "Returns a lazyseq of nested 2-element vectors, each one containing
  a vector of `n` successive elements of the original collection and
  an sequence index.

      (successive-nth-indexed 2 [10 20 30 40])
      => ([[10 20] 0] [[20 30] 1] [[30 40] 2])"
  ([n coll] (successive-nth-indexed n 0 coll))
  ([n idx coll]
    (lazy-seq
      (let [s (take n coll)]
        (if (= n (count s))
          (cons [(vec s) idx]
            (successive-nth-indexed n (inc idx) (rest coll))))))))

(defn value-set
  ([idx v] (into #{} (get idx v)))
  ([f idx v] (persistent! (reduce #(conj! %1 (f %2)) (transient #{}) (get idx v)))))

(defn apply-to-keys
  "Applies `f` with `args` to all given `keys` in `type`."
  [type keys f & args]
  (reduce (fn[acc k] (assoc acc k (apply f (get type k) args))) type keys))

(defn apply-to-pairs
  [f1 f2 coll]
  (when (> (count coll) 1)
    (reduce f1 (map (fn [[a b]] (f2 a b))
      (successive-nth 2 coll)))))

(defn wrap-seq
  [s head tail]
  (concat
    (if (sequential? head) (concat head s) (cons head s))
    (if (sequential? tail) tail [tail])))

(defn all-after
  "Returns a new collection of all items after `item` in original `coll`.
  If `coll` is a vector, the new collection is created with `subvec`.
  Returns original coll if item isn't found."
  [item coll]
  (let [idx (inc (.indexOf coll item))]
    (if (pos? idx)
      (if (vector? coll) (subvec coll idx) (drop idx coll))
      coll)))

(defn iterate-while
  [pred f x]
  (when (pred x)
    (cons x (lazy-seq (iterate-while pred f (f x))))))
