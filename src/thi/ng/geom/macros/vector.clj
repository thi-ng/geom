(ns thi.ng.geom.macros.vector
  (:require
   [clojure.walk :refer :all]
   [thi.ng.dstruct.core :as d]
   [thi.ng.xerror.core :as err]))

(def ^:private swizzle-keys
  {\x 0 \y 1 \z 2 \w 3})

(def ^:private swizzle-perms
  (->> [(d/cartesian-product '#{x y z} '#{x y z} '#{x y z})
        (d/cartesian-product '#{x y} '#{x y})
        (d/cartesian-product '#{x z} '#{x z})
        (d/cartesian-product '#{y z} '#{y z})
        '((x) (y) (z))]
       (apply concat)
       (map #(symbol (apply str %)))))

(defmacro defswizzle
  [btype key]
  (let [id (str key)
        [a b v] (repeatedly 3 gensym)
        c (count id)
        type (if (== 3 c) 'Vec3 'Vec2)
        ;;pre (if (>= (.indexOf id "z") 0) {:pre [`(instance? ~'Vec3 ~v)]} {})
        ]
    (if (== 1 c)
      `(defn ~key
         [~v]
         ;;~pre
         (let [~(with-meta a {:tag "doubles"}) (if (instance? ~'Vec2 ~v)
                                                 (.-buf ~(with-meta v {:tag "Vec2"}))
                                                 (.-buf ~(with-meta v {:tag "Vec3"})))]
           (aget ~a ~(swizzle-keys (first id)))))
      `(defn ~key
         [~v]
         ;;~pre
         (let [~(with-meta a {:tag "doubles"}) (if (instance? ~'Vec2 ~v)
                                                 (.-buf ~(with-meta v {:tag "Vec2"}))
                                                 (.-buf ~(with-meta v {:tag "Vec3"})))
               ~(with-meta b {:tag "doubles"}) (~@btype ~c)]
           ~@(map-indexed
              (fn [i ch]
                (list 'aset b i (list 'double (list 'aget a (swizzle-keys ch)))))
              id)
           (new ~type ~b nil (meta ~v)))))))

(defmacro lookup2
  [src k nf]
  `(if (= ~k \x)
     (aget ~src 0)
     (if (= ~k \y)
       (aget ~src 1)
       (or ~nf (err/key-error! ~k)))))

(defmacro rewrite-v2
  [src & body]
  (let [[a x y] (repeatedly gensym)]
    `(let [~(with-meta a {:tag "doubles"}) ~src
           ~x (aget ~a 0)
           ~y (aget ~a 1)]
       ~@(postwalk-replace {'x x 'y y} body))))

(defmacro rewrite-v2-no-let
  [src & body]
  (let [a (gensym)]
    `(let [~(with-meta a {:tag "doubles"}) ~src]
       ~@(postwalk-replace {'x `(aget ~a 0) 'y `(aget ~a 1)} body))))

(defmacro rewrite-v2-v-no-let
  [src v default & body]
  (let [a (gensym)
        b (gensym)]
    `(let [~(with-meta a {:tag "doubles"}) ~src]
       (if (instance? ~'Vec2 ~v)
         (let [~(with-meta b {:tag "doubles"}) (.-buf ~(with-meta v {:tag "Vec2"}))]
           ~@(postwalk-replace
              {'x `(aget ~a 0) 'y `(aget ~a 1) 'vx `(aget ~b 0) 'vy `(aget ~b 1)}
              body))
         (do ~@(postwalk-replace
                {'x `(aget ~a 0) 'y `(aget ~a 1) 'vx `(nth ~v 0 ~default) 'vy `(nth ~v 1 ~default)}
                body))))))

(defmacro rewrite-v2-v
  [src v default & body]
  (let [[a b x y vx vy] (repeatedly gensym)]
    `(let [~(with-meta a {:tag "doubles"}) ~src
           ~x (aget ~a 0)
           ~y (aget ~a 1)]
       (if (instance? ~'Vec2 ~v)
         (let [~(with-meta b {:tag "doubles"}) (.-buf ~(with-meta v {:tag "Vec2"}))
               ~vx (aget ~b 0)
               ~vy (aget ~b 1)]
           ~@(postwalk-replace {'x x 'y y 'vx vx 'vy vy} body))
         (let [~vx (nth ~v 0 ~default), ~vy (nth ~v 1 ~default)]
           ~@(postwalk-replace {'x x 'y y 'vx vx 'vy vy} body))))))

(defmacro rewrite-v2-nv
  [src v default & body]
  (let [[a v? nv vx vy] (repeatedly gensym)]
    `(let [~(with-meta a {:tag "doubles"}) ~src
           ~v? (instance? ~'Vec2 ~v)
           ~nv (if ~v? (.-buf ~(with-meta v {:tag "Vec2"})) (number? ~v))
           ~vx (if ~v? (aget ~nv 0) (if ~nv ~v (nth ~v 0 ~default)))
           ~vy (if ~v? (aget ~nv 1) (if ~nv ~v (nth ~v 1 ~default)))]
       ~@(postwalk-replace
          {'x `(aget ~a 0) 'y `(aget ~a 1) 'vx vx 'vy vy}
          body))))

(defmacro rewrite-v2-nv-no-let
  [src v default & body]
  (let [a (gensym), b (gensym)]
    `(let [~(with-meta a {:tag "doubles"}) ~src]
       (if (instance? ~'Vec2 ~v)
         (let [~(with-meta b {:tag "doubles"}) (.-buf ~(with-meta v {:tag "Vec2"}))]
           ~@(postwalk-replace
              {'x `(aget ~a 0) 'y `(aget ~a 1) 'vx `(aget ~b 0) 'vy `(aget ~b 1)}
              body))
         (if (number? ~v)
           (do ~@(postwalk-replace
                  {'x `(aget ~a 0) 'y `(aget ~a 1) 'vx v 'vy v} body))
           (do ~@(postwalk-replace
                  {'x `(aget ~a 0) 'y `(aget ~a 1)
                   'vx `(nth ~v 0 ~default) 'vy `(nth ~v 1 ~default)}
                  body)))))))

(defmacro rewrite-v2-nv-nv
  [src v v2 d1 d2 & body]
  (let [[a b c x y bx by cx cy v1? v2? n1? n2?] (repeatedly gensym)]
    `(let [~v1? (instance? ~'Vec2 ~v)
           ~v2? (instance? ~'Vec2 ~v2)
           ~n1? (if-not ~v1? (number? ~v))
           ~n2? (if-not ~v2? (number? ~v2))
           ~(with-meta a {:tag "doubles"}) ~src
           ~(with-meta b {:tag "doubles"}) (if ~v1? (.-buf ~(with-meta v {:tag "Vec2"})))
           ~(with-meta c {:tag "doubles"}) (if ~v2? (.-buf ~(with-meta v2 {:tag "Vec2"})))
           ~x (aget ~a 0)
           ~y (aget ~a 1)
           ~bx (if ~v1? (aget ~b 0) (if ~n1? ~v (nth ~v 0 ~d1)))
           ~by (if ~v1? (aget ~b 1) (if ~n1? ~v (nth ~v 1 ~d1)))
           ~cx (if ~v2? (aget ~c 0) (if ~n2? ~v2 (nth ~v2 0 ~d2)))
           ~cy (if ~v2? (aget ~c 1) (if ~n2? ~v2 (nth ~v2 1 ~d2)))]
       ~@(postwalk-replace {'x x 'y y 'bx bx 'by by 'cx cx 'cy cy} body))))

(defmacro v2-op0
  [btype op src meta]
  `(let [dest# (~@btype 2)]
     (rewrite-v2-no-let
      ~src
      (aset dest# 0 (double (~op ~'x)))
      (aset dest# 1 (double (~op ~'y))))
     (new ~'Vec2 dest# nil ~meta)))

(defmacro vec-op0!
  [op src n]
  `(do
     ~@(map #(list 'aset src % (list 'double (list op (list 'aget src %)))) (range n))
     (set! ~'_hash nil)))

(defmacro v2-op1-xy
  [btype op src v v2 d meta]
  (let [[a b c dest x y bx by cx cy n1? n2? v1? v2?] (repeatedly gensym)]
    `(let [~(with-meta a {:tag "doubles"}) ~src
           ~dest (~@btype 2)
           ~x (aget ~a 0)
           ~y (aget ~a 1)
           ~n1? (number? ~v)
           ~n2? (number? ~v2)]
       (if (if ~n1? ~n2? false)
         (do (aset ~dest 0 (double (~op ~x ~v)))
             (aset ~dest 1 (double (~op ~y ~v2))))
         (let [~v1? (if-not ~n1? (instance? ~'Vec2 ~v))
               ~v2? (if-not ~n2? (instance? ~'Vec2 ~v2))
               ~(with-meta b {:tag "doubles"}) (if ~v1? (.-buf ~(with-meta v {:tag "Vec2"})))
               ~(with-meta c {:tag "doubles"}) (if ~v2? (.-buf ~(with-meta v2 {:tag "Vec2"})))
               ~bx (if ~v1? (aget ~b 0) (if ~n1? ~v (nth ~v 0 ~d)))
               ~by (if ~v1? (aget ~b 1) (if ~n1? ~v (nth ~v 1 ~d)))
               ~cx (if ~v2? (aget ~c 0) (if ~n2? ~v2 (nth ~v2 0 ~d)))
               ~cy (if ~v2? (aget ~c 1) (if ~n2? ~v2 (nth ~v2 1 ~d)))]
           (aset ~dest 0 (double (~op (~op ~x ~bx) ~cx)))
           (aset ~dest 1 (double (~op (~op ~y ~by) ~cy)))))
       (new ~'Vec2 ~dest nil ~meta))))

(defmacro v2-op1-xy!
  [op src v v2 d]
  (let [[a b c bx by cx cy n1? n2? v1? v2?] (repeatedly gensym)]
    `(let [~n1? (number? ~v)
           ~n2? (number? ~v2)]
       (if (if ~n1? ~n2? false)
         (do (aset ~src 0 (double (~op (aget ~src 0) ~v)))
             (aset ~src 1 (double (~op (aget ~src 1) ~v2))))
         (let [~v1? (if-not ~n1? (instance? ~'Vec2 ~v))
               ~v2? (if-not ~n2? (instance? ~'Vec2 ~v2))
               ~(with-meta b {:tag "doubles"}) (if ~v1? (.-buf ~(with-meta v {:tag "Vec2"})))
               ~(with-meta c {:tag "doubles"}) (if ~v2? (.-buf ~(with-meta v2 {:tag "Vec2"})))
               ~bx (if ~v1? (aget ~b 0) (if ~n1? ~v (nth ~v 0 ~d)))
               ~by (if ~v1? (aget ~b 1) (if ~n1? ~v (nth ~v 1 ~d)))
               ~cx (if ~v2? (aget ~c 0) (if ~n2? ~v2 (nth ~v2 0 ~d)))
               ~cy (if ~v2? (aget ~c 1) (if ~n2? ~v2 (nth ~v2 1 ~d)))]
           (aset ~src 0 (double (~op (~op (aget ~src 0) ~bx) ~cx)))
           (aset ~src 1 (double (~op (~op (aget ~src 1) ~by) ~cy)))))
       (set! ~'_hash nil))))

(defmacro v2-op1
  [btype op src v meta]
  `(let [dest# (~@btype 2)]
     (rewrite-v2-nv-no-let
      ~src ~v 0.0
      (aset dest# 0 (double (~op ~'x ~'vx)))
      (aset dest# 1 (double (~op ~'y ~'vy))))
     (new ~'Vec2 dest# nil ~meta)))

(defmacro v2-op1!
  [op src v]
  `(rewrite-v2-nv-no-let
    ~src ~v 0.0
    (aset ~src 0 (double (~op ~'x ~'vx)))
    (aset ~src 1 (double (~op ~'y ~'vy)))
    (set! ~'_hash nil)))

(defmacro v2-op2
  [btype op op2 src v v2 d1 d2 meta]
  `(let [dest# (~@btype 2)]
     (rewrite-v2-nv-nv
      ~src ~v ~v2 ~d1 ~d2
      (aset dest# 0 (double (~op2 (~op ~'x ~'bx) ~'cx)))
      (aset dest# 1 (double (~op2 (~op ~'y ~'by) ~'cy))))
     (new ~'Vec2 dest# nil ~meta)))

(defmacro v2-op2!
  [op op2 src v v2 d1 d2]
  `(rewrite-v2-nv-nv
    ~src ~v ~v2 ~d1 ~d2
    (aset ~src 0 (double (~op2 (~op ~'x ~'bx) ~'cx)))
    (aset ~src 1 (double (~op2 (~op ~'y ~'by) ~'cy)))
    (set! ~'_hash nil)))

(defmacro v2-op2-no-let
  [btype op op2 src v v2 d1 d2 meta]
  `(let [dest# (~@btype 2)]
     (rewrite-v2-nv-nv-no-let
      ~src ~v ~v2 ~d1 ~d2
      (aset dest# 0 (double (~op2 (~op ~'x ~'bx) ~'cx)))
      (aset dest# 1 (double (~op2 (~op ~'y ~'by) ~'cy))))
     (new ~'Vec2 dest# nil ~meta)))

(defmacro lookup3
  [src k nf]
  `(case ~k
     \x (aget ~src 0)
     \y (aget ~src 1)
     \z (aget ~src 2)
     (or ~nf (err/key-error! ~k))))

(defmacro rewrite-v3
  [src & body]
  (let [[a x y z] (repeatedly 4 gensym)]
    `(let [~(with-meta a {:tag "doubles"}) ~src, ~x (aget ~a 0), ~y (aget ~a 1), ~z (aget ~a 2)]
       ~@(postwalk-replace {'x x 'y y 'z z} body))))

(defmacro rewrite-v3-no-let
  [src & body]
  (let [a (gensym)]
    `(let [~(with-meta a {:tag "doubles"}) ~src]
       ~@(postwalk-replace
          {'x `(aget ~a 0) 'y `(aget ~a 1) 'z `(aget ~a 2)}
          body))))

(defmacro rewrite-v3-v-no-let
  [src v default & body]
  (let [a (gensym)
        b (gensym)]
    `(let [~(with-meta a {:tag "doubles"}) ~src]
       (if (instance? ~'Vec3 ~v)
         (let [~(with-meta b {:tag "doubles"}) (.-buf ~(with-meta v {:tag "Vec3"}))]
           ~@(postwalk-replace
              {'x `(aget ~a 0) 'y `(aget ~a 1) 'z `(aget ~a 2)
               'vx `(aget ~b 0) 'vy `(aget ~b 1) 'vz `(aget ~b 2)}
              body))
         (do ~@(postwalk-replace
                {'x `(aget ~a 0) 'y `(aget ~a 1) 'z `(aget ~a 2)
                 'vx `(nth ~v 0 ~default) 'vy `(nth ~v 1 ~default) 'vz `(nth ~v 2 ~default)}
                body))))))

(defmacro rewrite-v3-v
  [src v default & body]
  (let [[a b x y z vx vy vz] (repeatedly 8 gensym)]
    `(let [~(with-meta a {:tag "doubles"}) ~src, ~x (aget ~a 0), ~y (aget ~a 1), ~z (aget ~a 2)]
       (if (instance? ~'Vec3 ~v)
         (let [~(with-meta b {:tag "doubles"}) (.-buf ~(with-meta v {:tag "Vec3"})), ~vx (aget ~b 0), ~vy (aget ~b 1), ~vz (aget ~b 2)]
           ~@(postwalk-replace {'x x 'y y 'z z 'vx vx 'vy vy 'vz vz} body))
         (let [~vx (nth ~v 0 ~default), ~vy (nth ~v 1 ~default), ~vz (nth ~v 2 ~default)]
           ~@(postwalk-replace {'x x 'y y 'z z 'vx vx 'vy vy 'vz vz} body))))))

(defmacro rewrite-v3-nv
  [src v default & body]
  (let [[a v? nv vx vy vz] (repeatedly 6 gensym)]
    `(let [~(with-meta a {:tag "doubles"}) ~src
           ~v? (instance? ~'Vec3 ~v)
           ~nv (if ~v? (.-buf ~(with-meta v {:tag "Vec3"})) (number? ~v))
           ~vx (if ~v? (aget ~nv 0) (if ~nv ~v (nth ~v 0 ~default)))
           ~vy (if ~v? (aget ~nv 1) (if ~nv ~v (nth ~v 1 ~default)))
           ~vz (if ~v? (aget ~nv 2) (if ~nv ~v (nth ~v 2 ~default)))]
       ~@(postwalk-replace
          {'x `(aget ~a 0) 'y `(aget ~a 1) 'z `(aget ~a 2) 'vx vx 'vy vy 'vz vz}
          body))))

(defmacro rewrite-v3-nv-no-let
  [src v default & body]
  (let [a (gensym), b (gensym)]
    `(let [~(with-meta a {:tag "doubles"}) ~src]
       (if (instance? ~'Vec3 ~v)
         (let [~(with-meta b {:tag "doubles"}) (.-buf ~(with-meta v {:tag "Vec3"}))]
           ~@(postwalk-replace
              {'x `(aget ~a 0) 'y `(aget ~a 1) 'z `(aget ~a 2)
               'vx `(aget ~b 0) 'vy `(aget ~b 1) 'vz `(aget ~b 2)}
              body))
         (if (number? ~v)
           (do ~@(postwalk-replace
                  {'x `(aget ~a 0) 'y `(aget ~a 1) 'z `(aget ~a 2) 'vx v 'vy v 'vz v} body))
           (do ~@(postwalk-replace
                  {'x `(aget ~a 0) 'y `(aget ~a 1) 'z `(aget ~a 2)
                   'vx `(nth ~v 0 ~default) 'vy `(nth ~v 1 ~default) 'vz `(nth ~v 2 ~default)}
                  body)))))))

(defmacro rewrite-v3-nv-nv
  [src v v2 d1 d2 & body]
  (let [[a b c x y z bx by bz cx cy cz v1? v2? n1? n2?] (repeatedly 16 gensym)]
    `(let [~v1? (instance? ~'Vec3 ~v)
           ~v2? (instance? ~'Vec3 ~v2)
           ~n1? (if-not ~v1? (number? ~v))
           ~n2? (if-not ~v2? (number? ~v2))
           ~(with-meta a {:tag "doubles"}) ~src
           ~(with-meta b {:tag "doubles"}) (if ~v1? (.-buf ~(with-meta v {:tag "Vec3"})))
           ~(with-meta c {:tag "doubles"}) (if ~v2? (.-buf ~(with-meta v2 {:tag "Vec3"})))
           ~x (aget ~a 0), ~y (aget ~a 1), ~z (aget ~a 2)
           ~bx (if ~v1? (aget ~b 0) (if ~n1? ~v (nth ~v 0 ~d1)))
           ~by (if ~v1? (aget ~b 1) (if ~n1? ~v (nth ~v 1 ~d1)))
           ~bz (if ~v1? (aget ~b 2) (if ~n1? ~v (nth ~v 2 ~d1)))
           ~cx (if ~v2? (aget ~c 0) (if ~n2? ~v2 (nth ~v2 0 ~d2)))
           ~cy (if ~v2? (aget ~c 1) (if ~n2? ~v2 (nth ~v2 1 ~d2)))
           ~cz (if ~v2? (aget ~c 2) (if ~n2? ~v2 (nth ~v2 2 ~d2)))]
       ~@(postwalk-replace
          {'x x 'y y 'z z 'bx bx 'by by 'bz bz 'cx cx 'cy cy 'cz cz} body))))

(defmacro rewrite-v3-nv-nv-no-let
  [src v v2 d1 d2 & body]
  (let [[a b c x y z bx by bz cx cy cz v1? v2? n1? n2?] (repeatedly 16 gensym)]
    `(let [~v1? (instance? ~'Vec3 ~v)
           ~v2? (instance? ~'Vec3 ~v2)
           ~n1? (if-not ~v1? (number? ~v))
           ~n2? (if-not ~v2? (number? ~v2))
           ~(with-meta a {:tag "doubles"}) ~src
           ~(with-meta b {:tag "doubles"}) (if ~v1? (.-buf ~(with-meta v {:tag "Vec3"})))
           ~(with-meta c {:tag "doubles"}) (if ~v2? (.-buf ~(with-meta v2 {:tag "Vec3"})))]
       ~@(postwalk-replace
          {'x `(aget ~a 0) 'y `(aget ~a 1) 'z `(aget ~a 2)
           'bx `(if ~v1? (aget ~b 0) (if ~n1? ~v (nth ~v 0 ~d1)))
           'by `(if ~v1? (aget ~b 1) (if ~n1? ~v (nth ~v 1 ~d1)))
           'bz `(if ~v1? (aget ~b 2) (if ~n1? ~v (nth ~v 2 ~d1)))
           'cx `(if ~v2? (aget ~c 0) (if ~n2? ~v2 (nth ~v2 0 ~d2)))
           'cy `(if ~v2? (aget ~c 1) (if ~n2? ~v2 (nth ~v2 1 ~d2)))
           'cz `(if ~v2? (aget ~c 2) (if ~n2? ~v2 (nth ~v2 2 ~d2)))}
          body))))

(defmacro v3-op0
  [btype op src meta]
  `(let [dest# (~@btype 3)]
     (rewrite-v3-no-let
      ~src
      (aset dest# 0 (double (~op ~'x)))
      (aset dest# 1 (double (~op ~'y)))
      (aset dest# 2 (double (~op ~'z))))
     (new ~'Vec3 dest# nil ~meta)))

(defmacro v3-op1-xyz
  ([btype op src vs meta]
   `(v3-op1-xyz ~op ~btype ~src (nth ~vs 0 0.0) (nth ~vs 1 0.0) (nth ~vs 2 0.0) ~meta))
  ([btype op src x y z meta]
   (let [a (gensym)]
     `(let [~(with-meta a {:tag "doubles"}) ~src
            dest# (~@btype 3)]
        (aset dest# 0 (double (~op (aget ~a 0) ~x)))
        (aset dest# 1 (double (~op (aget ~a 1) ~y)))
        (aset dest# 2 (double (~op (aget ~a 2) ~z)))
        (new ~'Vec3 dest# nil ~meta)))))

(defmacro v3-op1-xyz!
  ([op src vs]
   `(v3-op1-xyz ~op ~src (nth ~vs 0 0.0) (nth ~vs 1 0.0) (nth ~vs 2 0.0)))
  ([op src x y z]
   `(do
      (aset ~src 0 (double (~op (aget ~src 0) ~x)))
      (aset ~src 1 (double (~op (aget ~src 1) ~y)))
      (aset ~src 2 (double (~op (aget ~src 2) ~z)))
      (set! ~'_hash nil))))

(defmacro v3-op1
  [btype op src v meta]
  `(let [dest# (~@btype 3)]
     (rewrite-v3-nv-no-let
      ~src ~v 0.0
      (aset dest# 0 (double (~op ~'x ~'vx)))
      (aset dest# 1 (double (~op ~'y ~'vy)))
      (aset dest# 2 (double (~op ~'z ~'vz))))
     (new ~'Vec3 dest# nil ~meta)))

(defmacro v3-op1!
  [op src v]
  `(rewrite-v3-nv-no-let
    ~src ~v 0.0
    (aset ~src 0 (double (~op ~'x ~'vx)))
    (aset ~src 1 (double (~op ~'y ~'vy)))
    (aset ~src 2 (double (~op ~'z ~'vz)))
    (set! ~'_hash nil)))

(defmacro v3-op2
  [btype op op2 src v v2 d1 d2 meta]
  `(let [dest# (~@btype 3)]
     (rewrite-v3-nv-nv
      ~src ~v ~v2 ~d1 ~d2
      (aset dest# 0 (double (~op2 (~op ~'x ~'bx) ~'cx)))
      (aset dest# 1 (double (~op2 (~op ~'y ~'by) ~'cy)))
      (aset dest# 2 (double (~op2 (~op ~'z ~'bz) ~'cz))))
     (new ~'Vec3 dest# nil ~meta)))

(defmacro v3-op2!
  [op op2 src v v2 d1 d2]
  `(rewrite-v3-nv-nv
    ~src ~v ~v2 ~d1 ~d2
    (aset ~src 0 (double (~op2 (~op ~'x ~'bx) ~'cx)))
    (aset ~src 1 (double (~op2 (~op ~'y ~'by) ~'cy)))
    (aset ~src 2 (double (~op2 (~op ~'z ~'bz) ~'cz)))
    (set! ~'_hash nil)))

(defmacro v3-op2-no-let
  [btype op op2 src v v2 d1 d2 meta]
  `(let [dest# (~@btype 3)]
     (rewrite-v3-nv-nv-no-let
      ~src ~v ~v2 ~d1 ~d2
      (aset dest# 0 (double (~op2 (~op ~'x ~'bx) ~'cx)))
      (aset dest# 1 (double (~op2 (~op ~'y ~'by) ~'cy)))
      (aset dest# 2 (double (~op2 (~op ~'z ~'bz) ~'cz))))
     (new ~'Vec3 dest# nil ~meta)))
