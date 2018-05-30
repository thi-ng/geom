(ns thi.ng.geom.vector
  #?(:clj
     (:import
      [java.nio FloatBuffer])
     :cljs
     (:require-macros
      [thi.ng.geom.macros.vector :as vm :refer [defswizzle]]
      [thi.ng.math.macros :as mm]))
  (:require
   [thi.ng.geom.core :as g]
   [thi.ng.math.core :as m :refer [*eps* PI TWO_PI INF- INF+]]
   [thi.ng.dstruct.streams :as streams]
   [thi.ng.xerror.core :as err]
   #?@(:clj
       [[thi.ng.geom.macros.vector :as vm :refer [defswizzle]]
        [thi.ng.math.macros :as mm]
        [clojure.core.protocols :as cp]])))

(declare V2 V3 vec2 vec3 vec2-reduce* vec3-reduce* swizzle-assoc* swizzle2-fns swizzle3-fns)

(deftype Vec2
    #?(:clj
       [^doubles buf ^:unsynchronized-mutable _hash _meta]
       :cljs
       [buf ^:mutable _hash _meta])

  #?@(:clj
      [clojure.lang.IObj
       (meta
        [_] _meta)
       (withMeta
        [_ m] (Vec2. (double-array buf) _hash m))

       Cloneable
       (clone
        [_]
        (let [^doubles buf' (double-array 2)]
          (aset buf' 0 (aget buf 0))
          (aset buf' 1 (aget buf 1))
          (Vec2. buf' _hash _meta)))

       clojure.lang.ILookup
       (valAt
        [_ k]
        (if (keyword? k)
          (if-let [f (swizzle2-fns k)]
            (f _)
            (err/key-error! k))
          (if (and (>= k 0) (< k 2))
            (aget buf k)
            (err/key-error! k))))
       (valAt
        [_ k nf]
        (if (keyword? k)
          (if-let [f (swizzle2-fns k)] (f _) nf)
          (if (and (>= k 0) (< k 2)) (aget buf k) nf)))

       java.util.concurrent.Callable
       (call
        [_] (.invoke ^clojure.lang.IFn _))

       java.lang.Runnable
       (run
         [_] (.invoke ^clojure.lang.IFn _))

       clojure.lang.IFn
       (invoke
        [_ k]
        (if (keyword? k)
          (if-let [f (swizzle2-fns k)]
            (f _)
            (err/key-error! k))
          (if (and (>= k 0) (< k 2))
            (aget buf k)
            (err/key-error! k))))
       (invoke
        [_ k nf]
        (if (keyword? k)
          (if-let [f (swizzle2-fns k)] (f _) nf)
          (if (and (>= k 0) (< k 2)) (aget buf k) nf)))
       (applyTo
        [_ args]
        (case (count args)
          1 (.invoke ^clojure.lang.IFn _ (first args))
          2 (.invoke ^clojure.lang.IFn _ (first args) (nth args 1))
          (err/illegal-arg! (str "wrong number of args (" (count args) ")"))))

       clojure.lang.Associative
       clojure.lang.IPersistentVector
       (count
        [_] 2)
       (length
        [_] 2)
       (containsKey
        [_ k]
        (if (number? k)
          (and (>= k 0) (< k 2))
          (if (swizzle2-fns k) true false)))
       (entryAt
        [_ k] (clojure.lang.MapEntry. k (aget buf k)))
       (assoc
        [_ k v]
        (cond
          (number? k)  (if (or (== k 0) (== k 1))
                         (let [^doubles b (double-array buf)]
                           (aset b k (double v)) (Vec2. b nil _meta))
                         (if (== k 2) (conj _ v) (err/key-error! k)))
          (keyword? k) (if (= :z k)
                         (conj _ v)
                         (Vec2. (swizzle-assoc* buf (double-array buf) {\x 0 \y 1} k v) nil _meta))))
       (assocN
        [_ k v]
        (let [b (double-array buf)] (aset b k (double v)) (Vec2. b nil _meta)))

       java.util.Collection
       (isEmpty
        [_] false)
       (iterator
        [_] (.iterator ^java.util.Collection (list (aget buf 0) (aget buf 1))))
       (toArray
        [_] (object-array _))
       (size
        [_] 2)

       clojure.lang.IPersistentCollection
       clojure.lang.Indexed
       clojure.lang.Sequential
       clojure.lang.Seqable
       clojure.lang.Reversible
       java.util.List
       (seq
        [_] (seq buf))
       (empty
        [_] (err/unsupported!))
       (cons
        [_ x] (with-meta (vec3 (aget buf 0) (aget buf 1) x) _meta))
       (peek
        [_] (aget buf 1))
       (pop
        [_] (with-meta [(aget buf 0)] _meta))
       (rseq
        [_] (seq ((swizzle2-fns :yx) _)))
       (get
        [_ n] (aget buf n))
       (nth
        [_ n] (aget buf n))
       (nth
        [_ n nf] (if (>= n 0) (if (< n 2) (aget buf n) nf)))
       (equiv
        [_ o]
        (if (instance? Vec2 o)
          (let [^doubles b' (.-buf ^Vec2 o)]
            (and (== (aget buf 0) (aget b' 0)) (== (aget buf 1) (aget b' 1))))
          (and (sequential? o) (== 2 (count o))
               (clojure.lang.Util/equiv (aget buf 0) (first o))
               (clojure.lang.Util/equiv (aget buf 1) (nth o 1)))))
       (equals
        [_ o]
        (if (instance? Vec2 o)
          (let [^doubles b' (.-buf ^Vec2 o)]
            (and (== (aget buf 0) (aget b' 0)) (== (aget buf 1) (aget b' 1))))
          (and (sequential? o) (== 2 (count o))
               (clojure.lang.Util/equals (aget buf 0) (first o))
               (clojure.lang.Util/equals (aget buf 1) (nth o 1)))))

       Comparable
       (compareTo
        [_ o]
        (if (instance? Vec2 o)
          (let [^doubles b' (.-buf ^Vec2 o)
                c (compare (aget buf 0) (aget b' 0))]
            (if (== 0 c) (compare (aget buf 1) (aget b' 1)) c))
          (let [c (count o)]
            (if (== 2 c) (- (compare o _)) (- 2 c)))))
       (hashCode
        [_]
        (-> 31
            (unchecked-add-int (hash (aget buf 0)))
            (unchecked-multiply-int 31)
            (unchecked-add-int (hash (aget buf 1)))))

       clojure.lang.IHashEq
       (hasheq
        [_]
        (or _hash
            (set! _hash
                  (mix-collection-hash
                   (-> 31
                       (unchecked-add-int (hash (aget buf 0)))
                       (unchecked-multiply-int 31)
                       (unchecked-add-int (hash (aget buf 1))))
                   2))))

       cp/InternalReduce
       (internal-reduce
        [_ f start]
        (let [acc (f start (aget buf 0))]
          (if (reduced? acc)
            @acc
            (let [acc (f acc (aget buf 1))]
              (if (reduced? acc)
                @acc
                acc)))))

       cp/CollReduce
       (coll-reduce
        [_ f]
        (let [acc (f (aget buf 0) (aget buf 1))] (if (reduced? acc) @acc acc)))
       (coll-reduce
        [_ f start]
        (let [acc (f start (aget buf 0))]
          (if (reduced? acc)
            @acc
            (let [acc (f acc (aget buf 1))]
              (if (reduced? acc)
                @acc
                acc)))))]

      :cljs
      [IMeta
       (-meta [_] _meta)

       IWithMeta
       (-with-meta [_ m] (Vec2. (js/Float32Array. buf) _hash m))

       ICloneable
       (-clone
        [_] (Vec2. (js/Float32Array. buf) _hash _meta))

       ILookup
       (-lookup
        [_ k]
        (if (keyword? k)
          (if-let [f (swizzle2-fns k)]
            (f _)
            (err/key-error! k))
          (if (and (>= k 0) (< k 2))
            (aget buf k)
            (err/key-error! k))))
       (-lookup
        [_ k nf]
        (if (keyword? k)
          (if-let [f (swizzle2-fns k)] (f _) nf)
          (if (and (>= k 0) (< k 2)) (aget buf k) nf)))

       IFn
       (-invoke
        [_ k]
        (if (keyword? k)
          (if-let [f (swizzle2-fns k)]
            (f _)
            (err/key-error! k))
          (if (and (>= k 0) (< k 2))
            (aget buf k)
            (err/key-error! k))))
       (-invoke
        [_ k nf]
        (if (keyword? k)
          (if-let [f (swizzle2-fns k)] (f _) nf)
          (if (and (>= k 0) (< k 2)) (aget buf k) nf)))

       ICounted
       (-count [_] 2)

       IAssociative
       (-contains-key?
        [_ k]
        (if (number? k)
          (and (>= k 0) (< k 2))
          (if (swizzle2-fns k) true false)))
       (-assoc
        [_ k v]
        (cond
          (number? k)  (if (or (== k 0) (== k 1))
                         (let [^doubles b (js/Float32Array. buf)]
                           (aset b k (double v)) (Vec2. b nil _meta))
                         (if (== k 2) (conj _ v) (err/key-error! k)))
          (keyword? k) (if (= :z k)
                         (conj _ v)
                         (Vec2. (swizzle-assoc* buf (js/Float32Array. buf) {\x 0 \y 1} k v) nil _meta))))

       IVector
       (-assoc-n
        [_ n v]
        (let [b (js/Float32Array. buf)] (aset b n v) (Vec2. b nil _meta)))

       ISequential
       ISeq
       (-first
        [_] (aget buf 0))
       (-rest
        [_] (cons (aget buf 1) nil))

       INext
       (-next
        [_] (cons (aget buf 1) nil))

       ISeqable
       (-seq [_] _)

       IReversible
       (-rseq [_] ((swizzle2-fns :yx) _))

       IIndexed
       (-nth [_ n] (if (>= n 0) (if (< n 2) (aget buf n) (err/key-error! n))))
       (-nth [_ n nf] (if (>= n 0) (if (< n 2) (aget buf n) nf)))

       ICollection
       (-conj [_ x] (with-meta (vec3 (aget buf 0) (aget buf 1) x) _meta))

       IStack
       (-peek [_] (aget buf 1))
       (-pop [_] (with-meta [(aget buf 0)] _meta))

       IComparable
       (-compare
        [_ o]
        (if (instance? Vec2 o)
          (let [^doubles b' (.-buf ^Vec2 o)
                c (compare (aget buf 0) (aget b' 0))]
            (if (== 0 c) (compare (aget buf 1) (aget b' 1)) c))
          (let [c (count o)]
            (if (== 2 c) (- (compare o _)) (- 2 c)))))

       IHash
       (-hash
        [_]
        (or _hash
            (set! (.-_hash _)
                  (mix-collection-hash
                   (-> 31 (+ (hash (aget buf 0)))
                       (bit-or 0)
                       (imul 31) (+ (hash (aget buf 1)))
                       (bit-or 0))
                   2))))

       IEquiv
       (-equiv
        [_ o]
        (if (instance? Vec2 o)
          (let [^doubles b' (.-buf ^Vec2 o)]
            (and (== (aget buf 0) (aget b' 0)) (== (aget buf 1) (aget b' 1))))
          (and (sequential? o) (== 2 (count o))
               (= (aget buf 0) (first o))
               (= (aget buf 1) (nth o 1)))))

       IReduce
       (-reduce
        [coll f]
        (let [acc (f (aget buf 0) (aget buf 1))] (if (reduced? acc) @acc acc)))
       (-reduce
        [coll f start]
        (let [acc (f start (aget buf 0))]
          (if (reduced? acc)
            @acc
            (let [acc (f acc (aget buf 1))]
              (if (reduced? acc)
                @acc
                acc)))))

       IPrintWithWriter
       (-pr-writer
        [_ writer opts]
        (pr-sequential-writer writer pr-writer "#vec2 [" " " "]" opts (seq _)))
       ])

  Object
  (toString [_] (str "[" (aget buf 0) " " (aget buf 1) "]"))

  streams/IBuffer
  (get-float-buffer
    [_]
    #?(:clj
       (doto (FloatBuffer/allocate 2)
         (.put (float (aget buf 0)))
         (.put (float (aget buf 1)))
         (.rewind))
       :cljs buf))

  streams/IIntoBuffer
  (into-float-buffer
    [_ dest stride idx]
    #?@(:clj
        [(.position ^FloatBuffer dest (int idx))
         (.put ^FloatBuffer dest (float (aget buf 0)))
         (.put ^FloatBuffer dest (float (aget buf 1)))]
        :cljs
        [(.set dest buf idx)])
    (unchecked-add-int idx stride))

  m/IMathOps
  (+ [_] _)
  (+ [_ v]       (vm/v2-op1 #?(:clj (double-array) :cljs (new js/Float32Array)) + buf v _meta))
  (+ [_ v1 v2]   (vm/v2-op1-xy #?(:clj (double-array) :cljs (new js/Float32Array)) + buf v1 v2 0.0 _meta))
  (- [_]         (vm/v2-op0 #?(:clj (double-array) :cljs (new js/Float32Array)) - buf _meta))
  (- [_ v]       (vm/v2-op1 #?(:clj (double-array) :cljs (new js/Float32Array)) - buf v _meta))
  (- [_ v1 v2]   (vm/v2-op1-xy #?(:clj (double-array) :cljs (new js/Float32Array)) - buf v1 v2 0.0 _meta))
  (* [_] _)
  (* [_ v]       (vm/v2-op1 #?(:clj (double-array) :cljs (new js/Float32Array)) * buf v _meta))
  (* [_ v1 v2]   (vm/v2-op1-xy #?(:clj (double-array) :cljs (new js/Float32Array)) * buf v1 v2 1.0 _meta))
  (div [_]       (vm/v2-op0 #?(:clj (double-array) :cljs (new js/Float32Array)) / buf _meta))
  (div [_ v]     (vm/v2-op1 #?(:clj (double-array) :cljs (new js/Float32Array)) / buf v _meta))
  (div [_ v1 v2] (vm/v2-op1-xy #?(:clj (double-array) :cljs (new js/Float32Array)) / buf v1 v2 0.0 _meta))
  (madd [_ a b]  (vm/v2-op2 #?(:clj (double-array) :cljs (new js/Float32Array)) * + buf a b 1.0 0.0 _meta))
  (addm [_ a b]  (vm/v2-op2 #?(:clj (double-array) :cljs (new js/Float32Array)) + * buf a b 0.0 1.0 _meta))
  (msub [_ a b]  (vm/v2-op2 #?(:clj (double-array) :cljs (new js/Float32Array)) * - buf a b 1.0 0.0 _meta))
  (subm [_ a b]  (vm/v2-op2 #?(:clj (double-array) :cljs (new js/Float32Array)) - * buf a b 0.0 1.0 _meta))
  (abs [_]       (vm/v2-op0 #?(:clj (double-array) :cljs (new js/Float32Array)) m/abs* buf _meta))

  m/IMutableMathOps
  (+! [_]         _)
  (+! [_ v]       (vm/v2-op1! + buf v) _)
  (+! [_ v1 v2]   (vm/v2-op1-xy! + buf v1 v2 0.0) _)
  (-! [_]         (vm/vec-op0! - buf 2) _)
  (-! [_ v]       (vm/v2-op1! - buf v) _)
  (-! [_ v1 v2]   (vm/v2-op1-xy! - buf v1 v2 0.0) _)
  (*! [_]         _)
  (*! [_ v]       (vm/v2-op1! * buf v) _)
  (*! [_ v1 v2]   (vm/v2-op1-xy! * buf v1 v2 0.0) _)
  (div! [_]       (vm/vec-op0! / buf 2) _)
  (div! [_ v]     (vm/v2-op1! / buf v) _)
  (div! [_ v1 v2] (vm/v2-op1-xy! / buf v1 v2 0.0) _)
  (madd! [_ a b]  (vm/v2-op2! * + buf a b 1.0 0.0) _)
  (addm! [_ a b]  (vm/v2-op2! + * buf a b 0.0 1.0) _)
  (msub! [_ a b]  (vm/v2-op2! * - buf a b 1.0 0.0) _)
  (subm! [_ a b]  (vm/v2-op2! - * buf a b 0.0 1.0) _)
  (abs! [_]       (vm/vec-op0! m/abs* buf 2) _)

  g/IClear
  (clear* [_] (Vec2. #?(:clj (double-array 2) :cljs (js/Float32Array. 2)) nil nil))
  (clear! [_] (aset buf 0 0.0) (aset buf 1 0.0) (set! _hash nil) _)

  m/ICrossProduct
  (cross [_ v] (vm/rewrite-v2-v-no-let buf v 0.0 (mm/msub x vy y vx)))

  m/IDeltaEquals
  (delta=
    [_ v] (m/delta= _ v *eps*))
  (delta=
    [_ v eps]
    (if (sequential? v)
      (if (== 2 (count v))
        (vm/rewrite-v2-v-no-let
         buf v 0.0 (if (m/delta= x vx eps) (m/delta= y vy eps))))))

  g/IDistance
  (dist
    [_ v] (Math/sqrt (g/dist-squared _ v)))
  (dist-squared
    [_ v]
    (vm/rewrite-v2-v buf v 0.0
                     (let [dx (- x vx)
                           dy (- y vy)]
                       (mm/madd dx dx dy dy))))

  m/IDotProduct
  (dot [_ v] (vm/rewrite-v2-v-no-let buf v 0.0 (mm/madd x vx y vy)))

  g/IHeading
  (heading
    [_]
    (let [t (Math/atan2 (aget buf 1) (aget buf 0))]
      (if (neg? t) (+ t TWO_PI) t)))
  (heading-xy [_] (g/heading _))
  (angle-between
    [_ a]
    (let [t (- (g/heading a) (g/heading _))]
      (if (neg? t) (+ t TWO_PI) t)))
  (slope-xy [_] (/ (aget buf 1) (aget buf 0)))

  m/IInterpolate
  (mix
    [_ v]
    (let [^doubles b #?(:clj (double-array 2) :cljs (js/Float32Array. 2))]
      (vm/rewrite-v2-nv-no-let
       buf v 0.0
       (aset b 0 (double (* (+ x vx) 0.5)))
       (aset b 1 (double (* (+ y vy) 0.5))))
      (Vec2. b nil _meta)))
  (mix
    [_ v t]
    (let [^doubles b #?(:clj (double-array 2) :cljs (js/Float32Array. 2))]
      (vm/rewrite-v2-nv-nv
       buf v t 0.0 0.0
       (aset b 0 (double (+ (* (- bx x) cx) x)))
       (aset b 1 (double (+ (* (- by y) cy) y))))
      (Vec2. b nil _meta)))
  (mix
    [_ b c d u v]
    (let [^doubles b' #?(:clj (double-array 2) :cljs (js/Float32Array. 2))
          dv? (instance? Vec2 d)
          dn? (number? d)
          ^doubles dv (if dv? (.-buf ^Vec2 d))
          dx (if dv? (aget dv 0) (if dn? d (nth d 0 0.0)))
          dy (if dv? (aget dv 1) (if dn? d (nth d 1 0.0)))]
      (vm/rewrite-v2-nv-nv
       buf b c 0.0 0.0
       (let [x1 (+ (* (- bx x) u) x)
             y1 (+ (* (- by y) u) y)]
         (aset b' 0 (double (+ (* (- (+ (* (- dx cx) u) cx) x1) v) x1)))
         (aset b' 1 (double (+ (* (- (+ (* (- dy cy) u) cy) y1) v) y1)))))
      (Vec2. b' nil _meta)))
  (mix-with
    [_ v t f]
    (let [^doubles b #?(:clj (double-array 2) :cljs (js/Float32Array. 2))]
      (vm/rewrite-v2-nv-nv
       buf v t 0.0 0.0
       (aset b 0 (double (f x bx cx)))
       (aset b 1 (double (f y by cy))))
      (Vec2. b nil _meta)))
  (step
    [_ e]
    (let [^doubles b #?(:clj (double-array 2) :cljs (js/Float32Array. 2))]
      (vm/rewrite-v2-nv-no-let
       buf e 0.0
       (aset b 0 (double (m/step* vx x)))
       (aset b 1 (double (m/step* vy y))))
      (Vec2. b nil _meta)))
  (smoothstep
    [_ e1 e2]
    (let [^doubles b #?(:clj (double-array 2) :cljs (js/Float32Array. 2))]
      (vm/rewrite-v2-nv-nv
       buf e1 e2 0.0 0.0
       (aset b 0 (double (m/smoothstep* bx cx x)))
       (aset b 1 (double (m/smoothstep* bx cy y))))
      (Vec2. b nil _meta)))

  m/IInvert
  (invert [_] (m/- _))

  m/ILimit
  (limit
    [_ len]
    (if (> (m/mag-squared _) (* len len))
      (m/normalize _ len)
      _))

  m/IMagnitude
  (mag
    [_] (vm/rewrite-v2 buf (Math/sqrt (mm/madd x x y y))))
  (mag-squared
    [_] (vm/rewrite-v2 buf (mm/madd x x y y)))

  m/IMinMax
  (min
    [_ v] (vm/v2-op1 #?(:clj (double-array) :cljs (new js/Float32Array)) mm/min buf v _meta))
  (min
    [_ v v2] (vm/v2-op2 #?(:clj (double-array) :cljs (new js/Float32Array)) mm/min mm/min buf v v2 0.0 0.0 _meta))
  (max
    [_ v] (vm/v2-op1 #?(:clj (double-array) :cljs (new js/Float32Array)) mm/max buf v _meta))
  (max
    [_ v v2] (vm/v2-op2 #?(:clj (double-array) :cljs (new js/Float32Array)) mm/max mm/max buf v v2 0.0 0.0 _meta))

  g/INormal
  (normal
    [_]
    (let [^doubles b #?(:clj (double-array 2) :cljs (js/Float32Array. 2))]
      (aset b 0 (double (- (aget buf 1))))
      (aset b 1 (double (aget buf 0)))
      (Vec2. b nil _meta)))

  m/INormalize
  (normalize
    [_]
    (vm/rewrite-v2
     buf
     (let [l (Math/sqrt (mm/madd x x y y))]
       (if (pos? l)
         (let [^doubles b #?(:clj (double-array 2) :cljs (js/Float32Array. 2))]
           (aset b 0 (double (/ x l)))
           (aset b 1 (double (/ y l)))
           (Vec2. b nil _meta))
         _))))
  (normalize
    [_ len]
    (vm/rewrite-v2
     buf
     (let [l (Math/sqrt (mm/madd x x y y))]
       (if (pos? l)
         (let [l (/ len l)
               ^doubles b #?(:clj (double-array 2) :cljs (js/Float32Array. 2))]
           (aset b 0 (double (* x l)))
           (aset b 1 (double (* y l)))
           (Vec2. b nil _meta))
         _))))
  (normalized?
    [_] (m/delta= 1.0 (m/mag-squared _)))

  g/IPolar
  (as-polar
    [_]
    (let [^doubles b #?(:clj (double-array 2) :cljs (js/Float32Array. 2))]
      (aset b 0 (double (m/mag _)))
      (aset b 1 (double (g/heading _)))
      (Vec2. b nil _meta)))
  (as-cartesian
    [_]
    (vm/rewrite-v2
     buf
     (let [^doubles b #?(:clj (double-array 2) :cljs (js/Float32Array. 2))]
       (aset b 0 (double (* x (Math/cos y))))
       (aset b 1 (double (* x (Math/sin y))))
       (Vec2. b nil _meta))))

  g/IReflect
  (reflect
    [_ v]
    (let [^doubles b #?(:clj (double-array 2) :cljs (js/Float32Array. 2))]
      (vm/rewrite-v2-v buf v 0.0
                       (let [d (* (+ (* x vx) (* y vy)) 2.0)]
                         (aset b 0 (double (mm/msub vx d x)))
                         (aset b 1 (double (mm/msub vy d y)))
                         (Vec2. b nil _meta)))))
  g/IScale
  (scale
    [_ v]
    (vm/v2-op1 #?(:clj (double-array) :cljs (new js/Float32Array)) * buf v _meta))

  g/ITranslate
  (translate
    [_ v]
    (vm/v2-op1 #?(:clj (double-array) :cljs (new js/Float32Array)) + buf v _meta))

  g/IRotate
  (rotate
    [_ theta]
    (let [s (Math/sin theta) c (Math/cos theta)
          ^doubles b #?(:clj (double-array 2) :cljs (js/Float32Array. 2))]
      (vm/rewrite-v2
       buf
       (aset b 0 (double (mm/msub x c y s)))
       (aset b 1 (double (mm/madd x s y c)))
       (Vec2. b nil _meta))))

  g/ITransform
  (transform
    [_ m] (g/transform-vector m _))

  g/IVectorReduce
  (reduce-vector
    [_ f xs]
    (let [^doubles buf' #?(:clj (double-array 2) :cljs (js/Float32Array. buf))]
      #?@(:clj
          [(aset buf' 0 (aget buf 0))
           (aset buf' 1 (aget buf 1))])
      (Vec2. (vec2-reduce* f buf' xs) nil _meta)))
  (reduce-vector
    [_ f f2 xs]
    (let [^doubles buf' #?(:clj (double-array 2) :cljs (js/Float32Array. buf))]
      #?@(:clj
          [(aset buf' 0 (aget buf 0))
           (aset buf' 1 (aget buf 1))])
      (vec2-reduce* f buf' xs)
      (aset buf' 0 (double (f2 (aget buf' 0) 0)))
      (aset buf' 1 (double (f2 (aget buf' 1) 1)))
      (Vec2. buf' nil _meta))))

(deftype Vec3
    #?(:clj
       [^doubles buf ^:unsynchronized-mutable _hash _meta]
       :cljs
       [buf ^:mutable _hash _meta])

  #?@(:clj
      [clojure.lang.IObj
       (meta
        [_] _meta)
       (withMeta
        [_ m] (Vec3. (double-array buf) _hash m))

       Cloneable
       (clone
        [_]
        (let [^doubles buf' (double-array 3)]
          (aset buf' 0 (aget buf 0))
          (aset buf' 1 (aget buf 1))
          (aset buf' 2 (aget buf 2))
          (Vec2. buf' _hash _meta)))

       clojure.lang.ILookup
       (valAt
        [_ k]
        (if (keyword? k)
          (if-let [f (swizzle3-fns k)]
            (f _)
            (err/key-error! k))
          (if (and (>= k 0) (<= k 2))
            (aget buf k)
            (err/key-error! k))))
       (valAt
        [_ k nf]
        (if (keyword? k)
          (if-let [f (swizzle3-fns k)] (f _) nf)
          (if (and (>= k 0) (<= k 2)) (aget buf k) nf)))

       java.util.concurrent.Callable
       (call
        [_] (.invoke ^clojure.lang.IFn _))

       java.lang.Runnable
       (run
         [_] (.invoke ^clojure.lang.IFn _))

       clojure.lang.IFn
       (invoke
        [_ k]
        (if (keyword? k)
          (if-let [f (swizzle3-fns k)]
            (f _)
            (err/key-error! k))
          (if (and (>= k 0) (<= k 2))
            (aget buf k)
            (err/key-error! k))))
       (invoke
        [_ k nf]
        (if (keyword? k)
          (if-let [f (swizzle3-fns k)] (f _) nf)
          (if (and (>= k 0) (<= k 2)) (aget buf k) nf)))
       (applyTo
        [_ args]
        (case (count args)
          1 (.invoke ^clojure.lang.IFn _ (first args))
          2 (.invoke ^clojure.lang.IFn _ (first args) (nth args 1))
          (err/illegal-arg! (str "wrong number of args (" (count args) ")"))))

       clojure.lang.Associative
       clojure.lang.IPersistentVector
       (count
        [_] 3)
       (length
        [_] 3)
       (containsKey
        [_ k]
        (if (number? k)
          (and (>= k 0) (<= k 2))
          (if (swizzle3-fns k) true false)))
       (entryAt
        [_ k] (clojure.lang.MapEntry. k (aget buf k)))
       (assoc
        [_ k v]
        (cond
          (number? k)  (if (and (>= k 0) (<= k 2))
                         (let [^doubles b (double-array buf)]
                           (aset b k (double v)) (Vec3. b nil _meta))
                         (if (== k 3) (conj _ v) (err/key-error! k)))
          (keyword? k) (if (= :w k)
                         (conj _ v)
                         (Vec3. (swizzle-assoc* buf (double-array buf) {\x 0 \y 1 \z 2} k v) nil _meta))))
       (assocN
        [_ k v]
        (let [b (double-array buf)] (aset b k (double v)) (Vec3. b nil _meta)))

       java.util.Collection
       (isEmpty
        [_] false)
       (iterator
        [_] (.iterator ^java.util.Collection (list (aget buf 0) (aget buf 1) (aget buf 2))))
       (toArray
        [_] (object-array _))
       (size
        [_] 3)

       clojure.lang.IPersistentCollection
       clojure.lang.Indexed
       clojure.lang.Sequential
       clojure.lang.Seqable
       java.util.List
       (seq
        [_] (seq buf))
       (empty
        [_] (err/unsupported!))
       (cons
        [_ x] (with-meta [(aget buf 0) (aget buf 1) (aget buf 2) x] _meta))
       (peek
        [_] (aget buf 2))
       (pop
        [_]
        (let [^doubles b (double-array 2)]
          (aset b 0 (aget buf 0))
          (aset b 1 (aget buf 1))
          (Vec2. b nil _meta)))
       (rseq
        [_] (seq ((swizzle3-fns :zyx) _)))
       (get
        [_ n] (aget buf n))
       (nth
        [_ n] (aget buf n))
       (nth
        [_ n nf] (if (>= n 0) (if (< n 3) (aget buf n) nf)))
       (equiv
        [_ o]
        (if (instance? Vec3 o)
          (let [^doubles b' (.-buf ^Vec3 o)]
            (and (== (aget buf 0) (aget b' 0)) (== (aget buf 1) (aget b' 1)) (== (aget buf 2) (aget b' 2))))
          (and (sequential? o) (== 3 (count o))
               (clojure.lang.Util/equiv (aget buf 0) (first o))
               (clojure.lang.Util/equiv (aget buf 1) (nth o 1))
               (clojure.lang.Util/equiv (aget buf 2) (nth o 2)))))
       (equals
        [_ o]
        (if (instance? Vec3 o)
          (let [^doubles b' (.-buf ^Vec3 o)]
            (and (== (aget buf 0) (aget b' 0)) (== (aget buf 1) (aget b' 1)) (== (aget buf 2) (aget b' 2))))
          (and (sequential? o) (== 3 (count o))
               (clojure.lang.Util/equals (aget buf 0) (first o))
               (clojure.lang.Util/equals (aget buf 1) (nth o 1))
               (clojure.lang.Util/equals (aget buf 2) (nth o 2)))))

       Comparable
       (compareTo
        [_ o]
        (if (instance? Vec3 o)
          (let [^doubles b' (.-buf ^Vec3 o)
                c (compare (aget buf 0) (aget b' 0))]
            (if (== 0 c)
              (let [c (compare (aget buf 1) (aget b' 1))]
                (if (== 0 c)
                  (compare  (aget buf 2) (aget b' 2))
                  c))
              c))
          (let [c (count o)]
            (if (== 3 c) (- (compare o _)) (- 3 c)))))
       (hashCode
        [_]
        (-> 31
            (unchecked-add-int (hash (aget buf 0)))
            (unchecked-multiply-int 31)
            (unchecked-add-int (hash (aget buf 1)))
            (unchecked-multiply-int 31)
            (unchecked-add-int (hash (aget buf 2)))))

       clojure.lang.IHashEq
       (hasheq
        [_]
        (or _hash
            (set! _hash
                  (mix-collection-hash
                   (-> 31
                       (unchecked-add-int (hash (aget buf 0)))
                       (unchecked-multiply-int 31)
                       (unchecked-add-int (hash (aget buf 1)))
                       (unchecked-multiply-int 31)
                       (unchecked-add-int (hash (aget buf 2))))
                   3))))

       cp/InternalReduce
       (internal-reduce
        [_ f start]
        (let [acc (f start (aget buf 0))]
          (if (reduced? acc)
            @acc
            (let [acc (f acc (aget buf 1))]
              (if (reduced? acc)
                @acc
                (let [acc (f acc (aget buf 2))]
                  (if (reduced? acc)
                    @acc
                    acc)))))))

       cp/CollReduce
       (coll-reduce
        [_ f]
        (let [acc (f (aget buf 0) (aget buf 1))]
          (if (reduced? acc)
            @acc
            (let [acc (f acc (aget buf 2))]
              (if (reduced? acc)
                @acc
                acc)))))
       (coll-reduce
        [_ f start]
        (let [acc (f start (aget buf 0))]
          (if (reduced? acc)
            @acc
            (let [acc (f acc (aget buf 1))]
              (if (reduced? acc)
                @acc
                (let [acc (f acc (aget buf 2))]
                  (if (reduced? acc)
                    @acc
                    acc)))))))]

      :cljs
      [IMeta
       (-meta
        [_] _meta)

       IWithMeta
       (-with-meta
        [_ m] (Vec3. (js/Float32Array. buf) _hash m))

       ICloneable
       (-clone
        [_] (Vec3. (js/Float32Array. buf) _hash _meta))

       ILookup
       (-lookup
        [_ k]
        (if (keyword? k)
          (if-let [f (swizzle3-fns k)]
            (f _)
            (err/key-error! k))
          (if (and (>= k 0) (<= k 2))
            (aget buf k)
            (err/key-error! k))))
       (-lookup
        [_ k nf]
        (if (keyword? k)
          (if-let [f (swizzle3-fns k)]
            (f _)
            (err/key-error! k))
          (if (and (>= k 0) (<= k 2))
            (aget buf k)
            (err/key-error! k))))

       IFn
       (-invoke
        [_ k]
        (if (keyword? k)
          (if-let [f (swizzle3-fns k)]
            (f _)
            (err/key-error! k))
          (if (and (>= k 0) (<= k 2))
            (aget buf k)
            (err/key-error! k))))
       (-invoke
        [_ k nf]
        (if (keyword? k)
          (if-let [f (swizzle3-fns k)] (f _) nf)
          (if (and (>= k 0) (<= k 2)) (aget buf k) nf)))

       ICounted
       (-count
        [_] 3)

       IAssociative
       (-contains-key?
        [_ k]
        (if (number? k)
          (and (>= k 0) (<= k 2))
          (if (swizzle3-fns k) true false)))
       (-assoc
        [_ k v]
        (cond
          (number? k)  (if (and (>= k 0) (<= k 2))
                         (let [^doubles b (js/Float32Array. buf)]
                           (aset b k (double v)) (Vec3. b nil _meta))
                         (if (== k 3) (conj _ v) (err/key-error! k)))
          (keyword? k) (if (= :w k)
                         (conj _ v)
                         (Vec3. (swizzle-assoc* buf (js/Float32Array. buf) {\x 0 \y 1 \z 2} k v) nil _meta))))

       IVector
       (-assoc-n
        [_ n v]
        (let [b (js/Float32Array. buf)] (aset b n v) (Vec3. b nil _meta)))

       ISequential
       ISeq
       (-first
        [_] (aget buf 0))
       (-rest
        [_] (cons (aget buf 1) (cons (aget buf 2) nil)))

       INext
       (-next
        [_] (cons (aget buf 1) (cons (aget buf 2) nil)))

       ISeqable
       (-seq
        [_] _)

       IReversible
       (-rseq
        [_] ((swizzle3-fns :zyx) _))

       IIndexed
       (-nth
        [_ n] (if (>= n 0) (if (< n 3) (aget buf n) (err/key-error! n))))
       (-nth
        [_ n nf] (if (>= n 0) (if (< n 3) (aget buf n) nf)))

       ICollection
       (-conj
        [_ x] (with-meta [(aget buf 0) (aget buf 1) (aget buf 2) x] _meta))

       IStack
       (-peek
        [_] (aget buf 2))
       (-pop
        [_]
        (let [b (js/Float32Array. 2)]
          (aset b 0 (aget buf 0))
          (aset b 1 (aget buf 1))
          (Vec2. b nil _meta)))

       IComparable
       (-compare
        [_ o]
        (if (instance? Vec3 o)
          (let [^doubles b' (.-buf ^Vec3 o)
                c (compare (aget buf 0) (aget b' 0))]
            (if (== 0 c)
              (let [c (compare (aget buf 1) (aget b' 1))]
                (if (== 0 c)
                  (compare  (aget buf 2) (aget b' 2))
                  c))
              c))
          (let [c (count o)]
            (if (== 3 c) (- (compare o _)) (- 3 c)))))

       IHash
       (-hash
        [_]
        (or _hash
            (set! (.-_hash _)
                  (mix-collection-hash
                   (-> 31 (+ (hash (aget buf 0)))
                       (bit-or 0)
                       (imul 31) (+ (hash (aget buf 1)))
                       (bit-or 0)
                       (imul 31) (+ (hash (aget buf 2)))
                       (bit-or 0))
                   3))))

       IEquiv
       (-equiv
        [_ o]
        (if (instance? Vec3 o)
          (let [^doubles b' (.-buf ^Vec3 o)]
            (and (== (aget buf 0) (aget b' 0)) (== (aget buf 1) (aget b' 1)) (== (aget buf 2) (aget b' 2))))
          (and (sequential? o) (== 3 (count o))
               (= (aget buf 0) (first o))
               (= (aget buf 1) (nth o 1))
               (= (aget buf 2) (nth o 2)))))

       IReduce
       (-reduce
        [coll f]
        (let [acc (f (aget buf 0) (aget buf 1))]
          (if (reduced? acc)
            @acc
            (let [acc (f acc (aget buf 2))]
              (if (reduced? acc)
                @acc
                acc)))))
       (-reduce
        [coll f start]
        (let [acc (f start (aget buf 0))]
          (if (reduced? acc)
            @acc
            (let [acc (f acc (aget buf 1))]
              (if (reduced? acc)
                @acc
                (let [acc (f acc (aget buf 2))]
                  (if (reduced? acc)
                    @acc
                    acc)))))))

       IPrintWithWriter
       (-pr-writer
        [_ writer opts]
        (pr-sequential-writer writer pr-writer "#vec3 [" " " "]" opts (seq _)))])

  Object
  (toString
    [_] (str "[" (aget buf 0) " " (aget buf 1) " " (aget buf 2) "]"))

  streams/IBuffer
  (get-float-buffer
    [_]
    #?(:clj
       (doto (FloatBuffer/allocate 3)
         (.put (float (aget buf 0)))
         (.put (float (aget buf 1)))
         (.put (float (aget buf 2)))
         (.rewind))
       :cljs buf))

  streams/IIntoBuffer
  (into-float-buffer
    [_ dest stride idx]
    #?@(:clj
        [(.position ^FloatBuffer dest (int idx))
         (.put ^FloatBuffer dest (float (aget buf 0)))
         (.put ^FloatBuffer dest (float (aget buf 1)))
         (.put ^FloatBuffer dest (float (aget buf 2)))]
        :cljs
        [(.set dest buf idx)])
    (unchecked-add-int idx stride))

  m/IMathOps
  (+ [_] _)
  (+ [_ v]       (vm/v3-op1 #?(:clj (double-array) :cljs (new js/Float32Array)) + buf v _meta))
  (+ [_ v1 v2]   (vm/v3-op2 #?(:clj (double-array) :cljs (new js/Float32Array)) + + buf v1 v2 0.0 0.0 _meta))
  (+ [_ x y z]   (vm/v3-op1-xyz #?(:clj (double-array) :cljs (new js/Float32Array)) + buf x y z _meta))
  (- [_]         (vm/v3-op0 #?(:clj (double-array) :cljs (new js/Float32Array)) - buf _meta))
  (- [_ v]       (vm/v3-op1 #?(:clj (double-array) :cljs (new js/Float32Array)) - buf v _meta))
  (- [_ v1 v2]   (vm/v3-op2 #?(:clj (double-array) :cljs (new js/Float32Array)) - - buf v1 v2 0.0 0.0 _meta))
  (- [_ x y z]   (vm/v3-op1-xyz #?(:clj (double-array) :cljs (new js/Float32Array)) - buf x y z _meta))
  (* [_] _)
  (* [_ v]       (vm/v3-op1 #?(:clj (double-array) :cljs (new js/Float32Array)) * buf v _meta))
  (* [_ v1 v2]   (vm/v3-op2 #?(:clj (double-array) :cljs (new js/Float32Array)) * * buf v1 v2 0.0 0.0 _meta))
  (* [_ x y z]   (vm/v3-op1-xyz #?(:clj (double-array) :cljs (new js/Float32Array)) * buf x y z _meta))
  (div [_]       (vm/v3-op0 #?(:clj (double-array) :cljs (new js/Float32Array)) / buf _meta))
  (div [_ v]     (vm/v3-op1 #?(:clj (double-array) :cljs (new js/Float32Array)) / buf v _meta))
  (div [_ v1 v2] (vm/v3-op2 #?(:clj (double-array) :cljs (new js/Float32Array)) / / buf v1 v2 0.0 0.0 _meta))
  (div [_ x y z] (vm/v3-op1-xyz #?(:clj (double-array) :cljs (new js/Float32Array)) / buf x y z _meta))
  (madd [_ a b]  (vm/v3-op2 #?(:clj (double-array) :cljs (new js/Float32Array)) * + buf a b 1.0 0.0 _meta))
  (addm [_ a b]  (vm/v3-op2 #?(:clj (double-array) :cljs (new js/Float32Array)) + * buf a b 0.0 1.0 _meta))
  (msub [_ a b]  (vm/v3-op2 #?(:clj (double-array) :cljs (new js/Float32Array)) * - buf a b 1.0 0.0 _meta))
  (subm [_ a b]  (vm/v3-op2 #?(:clj (double-array) :cljs (new js/Float32Array)) - * buf a b 0.0 1.0 _meta))
  (abs [_]       (vm/v3-op0 #?(:clj (double-array) :cljs (new js/Float32Array)) m/abs* buf _meta))

  m/IMutableMathOps
  (+! [_]         _)
  (+! [_ v]       (vm/v3-op1! + buf v) _)
  (+! [_ v1 v2]   (vm/v3-op2! + + buf v1 v2 0.0 0.0) _)
  (+! [_ x y z]   (vm/v3-op1-xyz! + buf x y z) _)
  (-! [_]         (vm/vec-op0! - buf 3) _)
  (-! [_ v]       (vm/v3-op1! - buf v) _)
  (-! [_ v1 v2]   (vm/v3-op2! - - buf v1 v2 0.0 0.0) _)
  (-! [_ x y z]   (vm/v3-op1-xyz! - buf x y z) _)
  (*! [_]         _)
  (*! [_ v]       (vm/v3-op1! * buf v) _)
  (*! [_ v1 v2]   (vm/v3-op2! * * buf v1 v2 0.0 0.0) _)
  (*! [_ x y z]   (vm/v3-op1-xyz! * buf x y z) _)
  (div! [_]       (vm/vec-op0! / buf 3) _)
  (div! [_ v]     (vm/v3-op1! / buf v) _)
  (div! [_ v1 v2] (vm/v3-op2! / / buf v1 v2 0.0 0.0) _)
  (div! [_ x y z] (vm/v3-op1-xyz! / buf x y z) _)
  (madd! [_ a b]  (vm/v3-op2! * + buf a b 1.0 0.0) _)
  (addm! [_ a b]  (vm/v3-op2! + * buf a b 0.0 1.0) _)
  (msub! [_ a b]  (vm/v3-op2! * - buf a b 1.0 0.0) _)
  (subm! [_ a b]  (vm/v3-op2! - * buf a b 0.0 1.0) _)
  (abs! [_]       (vm/vec-op0! m/abs* buf 3) _)

  g/IClear
  (clear* [_] (Vec3. #?(:clj (double-array 3) :cljs (js/Float32Array. 3)) nil nil))
  (clear! [_] (aset buf 0 0.0) (aset buf 1 0.0) (aset buf 2 0.0) (set! _hash nil) _)

  m/ICrossProduct
  (cross
    [_ v]
    (let [^doubles b #?(:clj (double-array 3) :cljs (js/Float32Array. 3))]
      (vm/rewrite-v3-v buf v 0.0
                       (aset b 0 (double (mm/msub y vz vy z)))
                       (aset b 1 (double (mm/msub z vx vz x)))
                       (aset b 2 (double (mm/msub x vy vx y))))
      (Vec3. b nil _meta)))

  m/IDeltaEquals
  (delta=
    [_ v] (m/delta= _ v *eps*))
  (delta=
    [_ v eps]
    (if (sequential? v)
      (if (== 3 (count v))
        (vm/rewrite-v3-v-no-let
         buf v 0.0
         (if (m/delta= x vx eps)
           (if (m/delta= y vy eps)
             (m/delta= z vz eps)))))))

  g/IDistance
  (dist
    [_ v] (Math/sqrt (g/dist-squared _ v)))
  (dist-squared
    [_ v]
    (vm/rewrite-v3-v buf v 0.0
                     (let [dx (- x vx)
                           dy (- y vy)
                           dz (- z vz)]
                       (mm/madd dx dx dy dy dz dz))))

  m/IDotProduct
  (dot [_ v] (vm/rewrite-v3-v-no-let buf v 0.0 (+ (+ (* x vx) (* y vy)) (* z vz))))

  g/IHeading
  (heading [_] (g/heading-xy _))
  (heading-xy
    [_]
    (let [t (Math/atan2 (aget buf 1) (aget buf 0))]
      (if (neg? t) (+ t TWO_PI) t)))
  (heading-xz
    [_]
    (let [t (Math/atan2 (aget buf 2) (aget buf 0))]
      (if (neg? t) (+ t TWO_PI) t)))
  (heading-yz
    [_]
    (let [t (Math/atan2 (aget buf 2) (aget buf 1))]
      (if (neg? t) (+ t TWO_PI) t)))
  (angle-between
    [_ v]
    (let [v (if (instance? Vec3 v) v (vec3 v))]
      (Math/acos (m/dot (m/normalize _) (m/normalize v)))))
  (slope-xy [_] (/ (aget buf 1) (aget buf 0)))
  (slope-xz [_] (/ (aget buf 2) (aget buf 0)))
  (slope-yz [_] (/ (aget buf 2) (aget buf 1)))

  m/IInterpolate
  (mix
    [_ v]
    (let [^doubles b #?(:clj (double-array 3) :cljs (js/Float32Array. 3))]
      (vm/rewrite-v3-nv-no-let
       buf v 0.0
       (aset b 0 (double (* (+ x vx) 0.5)))
       (aset b 1 (double (* (+ y vy) 0.5)))
       (aset b 2 (double (* (+ z vz) 0.5))))
      (Vec3. b nil _meta)))
  (mix
    [_ v t]
    (let [^doubles b #?(:clj (double-array 3) :cljs (js/Float32Array. 3))]
      (vm/rewrite-v3-nv-nv
       buf v t 0.0 0.0
       (aset b 0 (double (+ (* (- bx x) cx) x)))
       (aset b 1 (double (+ (* (- by y) cy) y)))
       (aset b 2 (double (+ (* (- bz z) cz) z))))
      (Vec3. b nil _meta)))
  (mix
    [_ b c d u v]
    (let [^doubles b' #?(:clj (double-array 3) :cljs (js/Float32Array. 3))
          dv? (instance? Vec3 d)
          dn? (number? d)
          ^doubles dv (if dv? (.-buf ^Vec3 d))
          dx (if dv? (aget dv 0) (if dn? d (nth d 0 0.0)))
          dy (if dv? (aget dv 1) (if dn? d (nth d 1 0.0)))
          dz (if dv? (aget dv 2) (if dn? d (nth d 2 0.0)))]
      (vm/rewrite-v3-nv-nv
       buf b c 0.0 0.0
       (let [x1 (+ (* (- bx x) u) x)
             y1 (+ (* (- by y) u) y)
             z1 (+ (* (- bz z) u) z)]
         (aset b' 0 (double (+ (* (- (+ (* (- dx cx) u) cx) x1) v) x1)))
         (aset b' 1 (double (+ (* (- (+ (* (- dy cy) u) cy) y1) v) y1)))
         (aset b' 2 (double (+ (* (- (+ (* (- dz cz) u) cz) z1) v) z1)))))
      (Vec3. b' nil _meta)))
  (mix-with
    [_ v t f]
    (let [^doubles b #?(:clj (double-array 3) :cljs (js/Float32Array. 3))]
      (vm/rewrite-v3-nv-nv
       buf v t 0.0 0.0
       (aset b 0 (double (f x bx cx)))
       (aset b 1 (double (f y by cy)))
       (aset b 2 (double (f z bz cz))))
      (Vec2. b nil _meta)))
  (step
    [_ e]
    (let [^doubles b #?(:clj (double-array 3) :cljs (js/Float32Array. 3))]
      (vm/rewrite-v3-nv-no-let
       buf e 0.0
       (aset b 0 (double (m/step* vx x)))
       (aset b 1 (double (m/step* vy y)))
       (aset b 2 (double (m/step* vz z))))
      (Vec2. b nil _meta)))
  (smoothstep
    [_ e1 e2]
    (let [^doubles b #?(:clj (double-array 3) :cljs (js/Float32Array. 3))]
      (vm/rewrite-v3-nv-nv
       buf e1 e2 0.0 0.0
       (aset b 0 (double (m/smoothstep* bx cx x)))
       (aset b 1 (double (m/smoothstep* by cy y)))
       (aset b 2 (double (m/smoothstep* bz cz z))))
      (Vec2. b nil _meta)))

  m/IInvert
  (invert [_] (m/- _))

  m/ILimit
  (limit
    [_ len]
    (if (> (m/mag-squared _) (* len len))
      (m/normalize _ len)
      _))

  m/IMagnitude
  (mag
    [_] (vm/rewrite-v3 buf (Math/sqrt (mm/madd x x y y z z))))
  (mag-squared
    [_] (vm/rewrite-v3 buf (mm/madd x x y y z z)))

  m/IMinMax
  (min
    [_ v] (vm/v3-op1 #?(:clj (double-array) :cljs (new js/Float32Array)) mm/min buf v _meta))
  (min
    [_ v v2] (vm/v3-op2 #?(:clj (double-array) :cljs (new js/Float32Array)) mm/min mm/min buf v v2 0.0 0.0 _meta))
  (max
    [_ v] (vm/v3-op1 #?(:clj (double-array) :cljs (new js/Float32Array)) mm/max buf v _meta))
  (max
    [_ v v2] (vm/v3-op2 #?(:clj (double-array) :cljs (new js/Float32Array)) mm/max mm/max buf v v2 0.0 0.0 _meta))

  m/INormalize
  (normalize
    [_]
    (vm/rewrite-v3
     buf
     (let [l (Math/sqrt (mm/madd x x y y z z))]
       (if (pos? l)
         (let [^doubles b #?(:clj (double-array 3) :cljs (js/Float32Array. 3))]
           (aset b 0 (double (/ x l)))
           (aset b 1 (double (/ y l)))
           (aset b 2 (double (/ z l)))
           (Vec3. b nil _meta))
         _))))
  (normalize
    [_ len]
    (vm/rewrite-v3
     buf
     (let [l (Math/sqrt (mm/madd x x y y z z))]
       (if (pos? l)
         (let [l (/ len l)
               ^doubles b #?(:clj (double-array 3) :cljs (js/Float32Array. 3))]
           (aset b 0 (double (* x l)))
           (aset b 1 (double (* y l)))
           (aset b 2 (double (* z l)))
           (Vec3. b nil _meta))
         _))))
  (normalized?
    [_] (m/delta= 1.0 (m/mag-squared _)))

  g/IPolar
  (as-polar
    [_]
    (let [r (m/mag _)
          ^doubles b #?(:clj (double-array 3) :cljs (js/Float32Array. 3))]
      (aset b 0 (double r))
      (aset b 1 (double (Math/asin (/ (aget buf 2) r))))
      (aset b 2 (double (Math/atan2 (aget buf 1) (aget buf 0))))
      (Vec3. b nil _meta)))
  (as-cartesian
    [_]
    (let [b buf
          x (aget b 0)
          y (aget b 1)
          z (aget b 2)
          rcos (* x (Math/cos y))
          ^doubles b' #?(:clj (double-array 3) :cljs (js/Float32Array. 3))]
      (aset b' 0 (double (* rcos (Math/cos z))))
      (aset b' 1 (double (* rcos (Math/sin z))))
      (aset b' 2 (double (* x    (Math/sin y))))
      (Vec3. b' nil _meta)))

  g/IReflect
  (reflect
    [_ v]
    (let [^doubles b #?(:clj (double-array 3) :cljs (js/Float32Array. 3))]
      (vm/rewrite-v3-v buf v 0.0
                       (let [d (* (+ (+ (* x vx) (* y vy)) (* z vz)) 2.0)]
                         (aset b 0 (double (mm/msub vx d x)))
                         (aset b 1 (double (mm/msub vy d y)))
                         (aset b 2 (double (mm/msub vz d z)))
                         (Vec3. b nil _meta)))))
  g/IScale
  (scale
    [_ v]
    (vm/v3-op1 #?(:clj (double-array) :cljs (new js/Float32Array)) * buf v _meta))

  g/ITranslate
  (translate
    [_ v]
    (vm/v3-op1 #?(:clj (double-array) :cljs (new js/Float32Array)) + buf v _meta))

  g/IRotate
  (rotate [_ theta] (g/rotate-z _ theta))

  g/IRotate3D
  (rotate-x
    [_ theta]
    (let [s (Math/sin theta) c (Math/cos theta)
          ^doubles b #?(:clj (double-array 3) :cljs (js/Float32Array. 3))]
      (vm/rewrite-v3
       buf
       (aset b 0 (double x))
       (aset b 1 (double (mm/msub y c z s)))
       (aset b 2 (double (mm/madd y s z c)))
       (Vec3. b nil _meta))))
  (rotate-y
    [_ theta]
    (let [s (Math/sin theta) c (Math/cos theta)
          ^doubles b #?(:clj (double-array 3) :cljs (js/Float32Array. 3))]
      (vm/rewrite-v3
       buf
       (aset b 0 (double (mm/madd x c z s)))
       (aset b 1 (double y))
       (aset b 2 (double (mm/msub z c x s)))
       (Vec3. b nil _meta))))
  (rotate-z
    [_ theta]
    (let [s (Math/sin theta) c (Math/cos theta)
          ^doubles b #?(:clj (double-array 3) :cljs (js/Float32Array. 3))]
      (vm/rewrite-v3
       buf
       (aset b 0 (double (mm/msub x c y s)))
       (aset b 1 (double (mm/madd x s y c)))
       (aset b 2 (double z))
       (Vec3. b nil _meta))))
  (rotate-around-axis
    [_ v theta]
    (vm/rewrite-v3-v
     buf v 0.0
     (let [ux' (* vx x), uy' (* vx y), uz' (* vx z)
           vx' (* vy x), vy' (* vy y), vz' (* vy z)
           wx' (* vz x), wy' (* vz y), wz' (* vz z)
           vx2 (* vx vx), vy2 (* vy vy), vz2 (* vz vz)
           s (Math/sin theta), c (Math/cos theta)
           uvw (mm/add ux' vy' wz')
           ^doubles b #?(:clj (double-array 3) :cljs (js/Float32Array. 3))]
       (aset b 0 (double (mm/madd uvw vx
                                  (mm/msub (+ vy2 vz2) x (+ vy' wz') vx) c
                                  (mm/subm vz' wy' s))))

       (aset b 1 (double (mm/madd uvw vy
                                  (mm/msub (+ vx2 vz2) y (+ ux' wz') vy) c
                                  (mm/subm wx' uz' s))))

       (aset b 2 (double (mm/madd uvw vz
                                  (mm/msub (+ vx2 vy2) z (+ ux' vy') vz) c
                                  (mm/subm uy' vx' s))))
       (Vec3. b nil _meta))))

  g/ITransform
  (transform
    [_ m] (g/transform-vector m _))

  g/IVectorReduce
  (reduce-vector
    [_ f xs]
    (let [^doubles buf' #?(:clj (double-array 3) :cljs (js/Float32Array. buf))]
      #?@(:clj
          [(aset buf' 0 (aget buf 0))
           (aset buf' 1 (aget buf 1))
           (aset buf' 2 (aget buf 2))])
      (Vec3. (vec3-reduce* f buf' xs) nil _meta)))
  (reduce-vector
    [_ f f2 xs]
    (let [^doubles buf' #?(:clj (double-array 3) :cljs (js/Float32Array. buf))]
      #?@(:clj
          [(aset buf' 0 (aget buf 0))
           (aset buf' 1 (aget buf 1))
           (aset buf' 2 (aget buf 2))])
      (vec3-reduce* f buf' xs)
      (aset buf' 0 (double (f2 (aget buf' 0) 0)))
      (aset buf' 1 (double (f2 (aget buf' 1) 1)))
      (aset buf' 2 (double (f2 (aget buf' 2) 2)))
      (Vec3. buf' nil _meta))))

(defswizzle #?(:clj (double-array) :cljs (new js/Float32Array)) x)
(defswizzle #?(:clj (double-array) :cljs (new js/Float32Array)) xx)
(defswizzle #?(:clj (double-array) :cljs (new js/Float32Array)) xxx)
(defswizzle #?(:clj (double-array) :cljs (new js/Float32Array)) xxy)
(defswizzle #?(:clj (double-array) :cljs (new js/Float32Array)) xxz)
(defswizzle #?(:clj (double-array) :cljs (new js/Float32Array)) xy)
(defswizzle #?(:clj (double-array) :cljs (new js/Float32Array)) xyx)
(defswizzle #?(:clj (double-array) :cljs (new js/Float32Array)) xyy)
(defswizzle #?(:clj (double-array) :cljs (new js/Float32Array)) xyz)
(defswizzle #?(:clj (double-array) :cljs (new js/Float32Array)) xz)
(defswizzle #?(:clj (double-array) :cljs (new js/Float32Array)) xzx)
(defswizzle #?(:clj (double-array) :cljs (new js/Float32Array)) xzy)
(defswizzle #?(:clj (double-array) :cljs (new js/Float32Array)) xzz)
(defswizzle #?(:clj (double-array) :cljs (new js/Float32Array)) y)
(defswizzle #?(:clj (double-array) :cljs (new js/Float32Array)) yx)
(defswizzle #?(:clj (double-array) :cljs (new js/Float32Array)) yxx)
(defswizzle #?(:clj (double-array) :cljs (new js/Float32Array)) yxy)
(defswizzle #?(:clj (double-array) :cljs (new js/Float32Array)) yxz)
(defswizzle #?(:clj (double-array) :cljs (new js/Float32Array)) yy)
(defswizzle #?(:clj (double-array) :cljs (new js/Float32Array)) yyx)
(defswizzle #?(:clj (double-array) :cljs (new js/Float32Array)) yyy)
(defswizzle #?(:clj (double-array) :cljs (new js/Float32Array)) yyz)
(defswizzle #?(:clj (double-array) :cljs (new js/Float32Array)) yz)
(defswizzle #?(:clj (double-array) :cljs (new js/Float32Array)) yzx)
(defswizzle #?(:clj (double-array) :cljs (new js/Float32Array)) yzy)
(defswizzle #?(:clj (double-array) :cljs (new js/Float32Array)) yzz)
(defswizzle #?(:clj (double-array) :cljs (new js/Float32Array)) z)
(defswizzle #?(:clj (double-array) :cljs (new js/Float32Array)) zx)
(defswizzle #?(:clj (double-array) :cljs (new js/Float32Array)) zxx)
(defswizzle #?(:clj (double-array) :cljs (new js/Float32Array)) zxy)
(defswizzle #?(:clj (double-array) :cljs (new js/Float32Array)) zxz)
(defswizzle #?(:clj (double-array) :cljs (new js/Float32Array)) zy)
(defswizzle #?(:clj (double-array) :cljs (new js/Float32Array)) zyx)
(defswizzle #?(:clj (double-array) :cljs (new js/Float32Array)) zyy)
(defswizzle #?(:clj (double-array) :cljs (new js/Float32Array)) zyz)
(defswizzle #?(:clj (double-array) :cljs (new js/Float32Array)) zz)
(defswizzle #?(:clj (double-array) :cljs (new js/Float32Array)) zzx)
(defswizzle #?(:clj (double-array) :cljs (new js/Float32Array)) zzy)
(defswizzle #?(:clj (double-array) :cljs (new js/Float32Array)) zzz)

(def swizzle2-fns
  {:x x :xx xx :xy xy :y y :yx yx :yy yy})

(def swizzle3-fns
  {:x x, :xx xx, :xxx xxx, :xxy xxy, :xxz xxz, :xy xy, :xyx xyx,
   :xyy xyy, :xyz xyz, :xz xz, :xzx xzx, :xzy xzy, :xzz xzz,
   :y y, :yx yx, :yxx yxx, :yxy yxy, :yxz yxz, :yy yy, :yyx yyx,
   :yyy yyy, :yyz yyz, :yz yz, :yzx yzx, :yzy yzy, :yzz yzz,
   :z z, :zx zx, :zxx zxx, :zxy zxy, :zxz zxz, :zy zy, :zyx zyx,
   :zyy zyy, :zyz zyz, :zz zz, :zzx zzx, :zzy zzy, :zzz zzz})

(defn swizzle-assoc*
  #?(:clj [^doubles src ^doubles dest keymap k v] :cljs [src dest keymap k v])
  (let [n (name k)
        c (count n)]
    (if-let [idx (and (== 1 c) (keymap (first n)))]
      (do (aset dest (int idx) (double v)) dest)
      (if (and (<= c (count keymap)) (== c (count v) (count (into #{} n))))
        (loop [i 0, n n]
          (if n
            (if-let [idx (keymap (first n))]
              (do (aset dest (int idx) (double (v i)))
                  (recur (inc i) (next n)))
              (err/key-error! k))
            dest))
        (err/key-error! k)))))

(defn vec2-reduce*
  [op ^doubles acc xs]
  (transduce
   (map (fn [^Vec2 x] (.-buf x)))
   (fn
     ([a] a)
     ([^doubles a ^doubles b]
      (aset a 0 (double (op (aget a 0) (aget b 0))))
      (aset a 1 (double (op (aget a 1) (aget b 1))))
      a))
   acc xs))

(defn vec3-reduce*
  [op ^doubles acc xs]
  (transduce
   (map (fn [^Vec3 x] (.-buf x)))
   (fn
     ([a] a)
     ([^doubles a ^doubles b]
      (aset a 0 (double (op (aget a 0) (aget b 0))))
      (aset a 1 (double (op (aget a 1) (aget b 1))))
      (aset a 2 (double (op (aget a 2) (aget b 2))))
      a))
   acc xs))

(def V2 (Vec2. #?(:clj (double-array 2) :cljs (js/Float32Array. 2)) nil nil))
(def V3 (Vec3. #?(:clj (double-array 3) :cljs (js/Float32Array. 3)) nil nil))

(defn vec2
  ([] V2)
  ([v]
   (cond
     (instance? Vec2 v) v
     (number? v)        (vec2 v v)
     (sequential? v)    (vec2 (nth v 0 0.0) (nth v 1 0.0))
     (map? v)           (vec2 (get v :x 0) (get v :y 0))
     :else              (err/type-error! "Vec2" v)))
  ([x y]
   (let [^doubles b #?(:clj (double-array 2) :cljs (js/Float32Array. 2))]
     (aset b 0 (double x))
     (aset b 1 (double y))
     (Vec2. b nil nil))))

(defn vec3
  ([] V3)
  ([v]
   (cond
     (instance? Vec3 v) v
     (number? v)        (vec3 v v v)
     (sequential? v)    (vec3 (nth v 0 0.0) (nth v 1 0.0) (nth v 2 0.0))
     (map? v)           (vec3 (get v :x 0.0) (get v :y 0.0) (get v :z 0.0))
     :else              (err/type-error! "Vec3" v)))
  ([v z]
   (cond
     (sequential? v) (vec3 (nth v 0 0.0) (nth v 1 0.0) z)
     (map? v)        (vec3 (get v :x 0.0) (get v :y 0.0) z)
     (number? v)     (vec3 v z 0)
     :else           (err/type-error! "Vec3" v)))
  ([x y z]
   (let [^doubles b #?(:clj (double-array 3) :cljs (js/Float32Array. 3))]
     (aset b 0 (double x))
     (aset b 1 (double y))
     (aset b 2 (double z))
     (Vec3. b nil nil))))

(defn vec2-with-meta
  ([v meta]
   (cond
     (instance? Vec2 v) (with-meta v meta)
     (number? v)        (vec2-with-meta v v meta)
     (sequential? v)    (vec2-with-meta (nth v 0 0.0) (nth v 1 0.0) meta)
     (map? v)           (vec2-with-meta (get v :x 0.0) (get v :y 0.0) meta)
     :else              (err/type-error! "Vec2" v)))
  ([x y meta]
   (let [^doubles b #?(:clj (double-array 2) :cljs (js/Float32Array. 2))]
     (aset b 0 (double x))
     (aset b 1 (double y))
     (Vec2. b nil meta))))

(defn vec3-with-meta
  ([v meta]
   (cond
     (instance? Vec3 v) (with-meta v meta)
     (number? v)        (vec3-with-meta v v v meta)
     (sequential? v)    (vec3-with-meta (nth v 0 0.0) (nth v 1 0.0) (nth v 2 0.0) meta)
     (map? v)           (vec3-with-meta (get v :x 0.0) (get v :y 0.0) (get v :z 0.0) meta)
     :else              (err/type-error! "Vec3" v)))
  ([x y z meta]
   (let [^doubles b #?(:clj (double-array 3) :cljs (js/Float32Array. 3))]
     (aset b 0 (double x))
     (aset b 1 (double y))
     (aset b 2 (double z))
     (Vec3. b nil meta))))

(defn vec2? [x] (instance? Vec2 x))
(defn vec3? [x] (instance? Vec3 x))

(def V2X (vec2 1 0))
(def V2Y (vec2 0 1))

(def V3X (vec3 1 0 0))
(def V3Y (vec3 0 1 0))
(def V3Z (vec3 0 0 1))

(def V2INF- (vec2 INF-))
(def V2INF+ (vec2 INF+))

(def V3INF- (vec3 INF-))
(def V3INF+ (vec3 INF+))

(defn randvec2
  ([] (m/normalize (vec2 (m/randnorm) (m/randnorm))))
  ([n] (m/normalize (vec2 (m/randnorm) (m/randnorm)) n)))

(defn randvec3
  ([] (m/normalize (vec3 (m/randnorm) (m/randnorm) (m/randnorm))))
  ([n] (m/normalize (vec3 (m/randnorm) (m/randnorm) (m/randnorm)) n)))
