(ns thi.ng.geom.quaternion
  #?(:cljs
     (:require-macros
      [thi.ng.math.macros :as mm]))
  (:require
   [thi.ng.geom.core :as g]
   [thi.ng.geom.vector :as v :refer [vec2 vec3 V3X V3Y V3Z]]
   [thi.ng.geom.matrix :as mat]
   [thi.ng.xerror.core :as err]
   [thi.ng.math.core :as m :refer [*eps* PI TWO_PI]]
   #?(:clj [thi.ng.math.macros :as mm]))
  #?(:clj
     (:import
      [thi.ng.geom.matrix Matrix44])))

(declare swizzle4 swizzle4-assoc)

(deftype Quat4
    #?(:clj
       [^double x ^double y ^double z ^double w
        ^:unsynchronized-mutable _hasheq
        ^:unsynchronized-mutable _hashc]
       :cljs
       [x y z w ^:mutable _hasheq])

  #?@(:clj
      [clojure.lang.ILookup
       (valAt
        [_ k] (swizzle4 _ k nil))
       (valAt
        [_ k nf] (swizzle4 _ k nf))

       java.util.concurrent.Callable
       (call
        [_] (.invoke ^clojure.lang.IFn _))

       java.lang.Runnable
       (run
         [_] (.invoke ^clojure.lang.IFn _))

       clojure.lang.IFn
       (invoke
        [_ k] (swizzle4 _ k nil))
       (invoke
        [_ k nf] (swizzle4 _ k nf))
       (applyTo
        [_ args]
        (condp == (count args)
          1 (swizzle4 _ (first args) nil)
          2 (swizzle4 _ (first args) (second args))
          (err/arity-error! (count args))))

       clojure.lang.Associative
       clojure.lang.IPersistentVector
       (count
        [_] 4)
       (length
        [_] 4)
       (containsKey
        [_ k] (not (nil? (#{0 1 2 3 :x :y :z :w} k))))
       (entryAt
        [_ k] (clojure.lang.MapEntry. k (swizzle4 _ k nil)))
       (assoc
        [_ k v] (swizzle4-assoc _ k v))
       (assocN
        [_ k v]
        (case (int k)
          0 (Quat4. v y z w -1 #?(:clj -1))
          1 (Quat4. x v z w -1 #?(:clj -1))
          2 (Quat4. x y v w -1 #?(:clj -1))
          3 (Quat4. x y z v -1 #?(:clj -1))
          (err/key-error! k)))

       java.util.Collection
       (isEmpty
        [_] false)
       (iterator
        [_] (.iterator ^java.util.Collection (list x y z w)))
       (toArray
        [_] (double-array 4 [x y z w]))
       (size
        [_] 4)

       clojure.lang.IPersistentCollection
       clojure.lang.Indexed
       clojure.lang.Sequential
       clojure.lang.Seqable
       java.util.List
       (seq
        [_] (seq [x y z w]))
       (empty
        [_] (err/unsupported!))
       (cons
        [_ v] [x y z w v])
       (peek
        [_] w)
       (pop
        [_] (vec3 x y z))
       (rseq
        [_] (Quat4. w z y x -1 #?(:clj -1)))
       (get
        [_ k] (case (int k) 0 x, 1 y, 2 z, 3 w, (err/key-error! k)))
       (nth
        [_ k] (case (int k) 0 x, 1 y, 2 z, 3 w, (err/key-error! k)))
       (nth
        [_ k nf] (case (int k) 0 x, 1 y, 2 z, 3 w, nf))
       (equiv
        [_ o]
        (if (instance? Quat4 o)
          (and (clojure.lang.Numbers/equiv x (.-x ^Quat4 o))
               (clojure.lang.Numbers/equiv y (.-y ^Quat4 o))
               (clojure.lang.Numbers/equiv z (.-z ^Quat4 o))
               (clojure.lang.Numbers/equiv w (.-w ^Quat4 o)))
          (and (instance? java.util.Collection o)
               (== 4 (count o))
               (clojure.lang.Util/equiv x (first o))
               (clojure.lang.Util/equiv y (nth o 1))
               (clojure.lang.Util/equiv z (nth o 2))
               (clojure.lang.Util/equiv w (nth o 3)))))
       (equals
        [_ o]
        (if (instance? Quat4 o)
          (and (clojure.lang.Util/equals x (.-x ^Quat4 o))
               (clojure.lang.Util/equals y (.-y ^Quat4 o))
               (clojure.lang.Util/equals z (.-z ^Quat4 o))
               (clojure.lang.Util/equals w (.-w ^Quat4 o)))
          (and (instance? java.util.Collection o)
               (== 4 (count o))
               (clojure.lang.Util/equals x (first o))
               (clojure.lang.Util/equals y (nth o 1))
               (clojure.lang.Util/equals z (nth o 2))
               (clojure.lang.Util/equals w (nth o 3)))))

       Comparable
       (compareTo
        [_ o]
        (if (instance? Quat4 o)
          (let [c (compare x (.-x ^Quat4 o))]
            (if (== 0 c)
              (let [c (compare y (.-y ^Quat4 o))]
                (if (== 0 c)
                  (let [c (compare z (.-z ^Quat4 o))]
                    (if (== 0 c)
                      (compare w (.-w ^Quat4 o))
                      c))
                  c))
              c))
          (let [c (count o)]
            (if (== 4 c) (- (compare o _)) (- 4 c)))))
       (hashCode
        [_]
        (if (== -1 _hashc)
          (set! _hashc
                (-> 31
                    (unchecked-add-int (hash x))
                    (unchecked-multiply-int 31)
                    (unchecked-add-int (hash y))
                    (unchecked-multiply-int 31)
                    (unchecked-add-int (hash z))
                    (unchecked-multiply-int 31)
                    (unchecked-add-int (hash w))))
          _hashc))

       clojure.lang.IHashEq
       (hasheq
        [_]
        (if (== -1 _hasheq)
          (set! _hasheq
                (mix-collection-hash
                 (if (== -1 _hashc)
                   (set! _hashc
                         (-> 31
                             (unchecked-add-int (hash x))
                             (unchecked-multiply-int 31)
                             (unchecked-add-int (hash y))
                             (unchecked-multiply-int 31)
                             (unchecked-add-int (hash z))
                             (unchecked-multiply-int 31)
                             (unchecked-add-int (hash w))))
                   _hashc)
                 4))
          _hasheq))]

      :cljs
      [ICloneable
       (-clone
        [_] (Quat4. x y z w -1))

       ILookup
       (-lookup
        [_ k] (swizzle4 _ k nil))
       (-lookup
        [_ k nf] (swizzle4 _ k nf))

       IFn
       (-invoke
        [_ k] (swizzle4 _ k nil))
       (-invoke
        [_ k nf] (swizzle4 _ k nf))

       ICounted
       (-count
        [_] 4)

       IAssociative
       (-contains-key?
        [_ k] (not (nil? (#{0 1 2 3 :x :y :z :w} k))))
       (-assoc
        [_ k v] (swizzle4-assoc _ k v))

       IVector
       (-assoc-n
        [_ k v]
        (case (int k)
          0 (Quat4. v y z w -1 #?(:clj -1))
          1 (Quat4. x v z w -1 #?(:clj -1))
          2 (Quat4. x y v w -1 #?(:clj -1))
          3 (Quat4. x y z v -1 #?(:clj -1))
          (err/key-error! k)))

       ISequential
       ISeq
       (-first
        [_] x)
       (-rest
        [_] (cons y (cons z (cons w nil))))

       INext
       (-next
        [_] (cons y (cons z (cons w nil))))

       ISeqable
       (-seq
        [_] _)

       IReversible
       (-rseq
        [_] (Quat4. w z y x -1 #?(:clj -1)))

       IIndexed
       (-nth
        [_ k] (case (int k) 0 x, 1 y, 2 z, 3 w, (err/key-error! k)))
       (-nth
        [_ k nf] (case (int k) 0 x, 1 y, 2 z, 3 w, nf))

       ICollection
       (-conj
        [_ v] [x y z w v])

       IStack
       (-peek
        [_] w)
       (-pop
        [_] (vec3 x y z))

       IComparable
       (-compare
        [_ o]
        (if (instance? Quat4 o)
          (let [c (compare x (.-x ^Quat4 o))]
            (if (== 0 c)
              (let [c (compare y (.-y ^Quat4 o))]
                (if (== 0 c)
                  (let [c (compare z (.-z ^Quat4 o))]
                    (if (== 0 c)
                      (compare w (.-w ^Quat4 o))
                      c))
                  c))
              c))
          (let [c (count o)]
            (if (== 4 c) (- (compare o _)) (- 4 c)))))

       IHash
       (-hash
        [_]
        (if (== -1 _hasheq)
          (set! _hasheq
                (mix-collection-hash
                 (-> (+ 31 (hash x))
                     (bit-or 0)
                     (imul 31) (+ (hash y))
                     (bit-or 0)
                     (imul 31) (+ (hash z))
                     (bit-or 0)
                     (imul 31) (+ (hash w))
                     (bit-or 0))
                 4))
          _hasheq))

       IEquiv
       (-equiv
        [_ o]
        (if (instance? Quat4 o)
          (and (== x (.-x ^Quat4 o)) (== y (.-y ^Quat4 o))
               (== z (.-z ^Quat4 o)) (== w (.-w ^Quat4 o)))
          (and (sequential? o) (== 4 (count o))
               (= x (first o)) (= y (nth o 1))
               (= z (nth o 2)) (= w (nth o 3)))))])
  Object
  (toString
    [_] (str "[" x " " y " " z " " w "]"))

  g/IScale
  (scale
    [_ s]
    (Quat4. (* x s) (* y s) (* z s) (* w s) -1 #?(:clj -1)))

  g/IRotate3D
  (rotate-x
    [_ theta]
    (let [t (/ theta 2.0)
          s (Math/sin t) c (Math/cos t)]
      (Quat4.
       (mm/madd x c w s)
       (mm/madd y c z s)
       (mm/msub z c y s)
       (mm/msub w c x s)
       -1 #?(:clj -1))))
  (rotate-y
    [_ theta]
    (let [t (/ theta 2.0)
          s (Math/sin t) c (Math/cos t)]
      (Quat4.
       (mm/msub x c z s)
       (mm/madd y c w s)
       (mm/madd z c x s)
       (mm/msub w c y s)
       -1 #?(:clj -1))))
  (rotate-z
    [_ theta]
    (let [t (/ theta 2.0)
          s (Math/sin t) c (Math/cos t)]
      (Quat4.
       (mm/madd x c y s)
       (mm/msub y c x s)
       (mm/madd z c w s)
       (mm/msub w c z s)
       -1 #?(:clj -1))))

  m/IMathOps
  (+
    [_ q]
    (let [q ^Quat4 q]
      (Quat4.
       (+ x (.-x q))
       (+ y (.-y q))
       (+ z (.-z q))
       (+ w (.-w q))
       -1 #?(:clj -1))))
  (-
    [_ q]
    (let [q ^Quat4 q]
      (Quat4.
       (- x (.-x q))
       (- y (.-y q))
       (- z (.-z q))
       (- w (.-w q))
       -1 #?(:clj -1))))
  (*
    [_ q]
    (let [q ^Quat4 q
          qx (.-x q) qy (.-y q) qz (.-z q) qw (.-w q)]
      (Quat4.
       (mm/maddsub x qw w qx y qz z qy)
       (mm/maddsub y qw w qy z qx x qz)
       (mm/maddsub z qw w qz x qy y qx)
       (mm/msub w qw x qx y qy z qz)
       -1 #?(:clj -1))))
  (*
    [_ q r]
    (let [q ^Quat4 q r ^Quat4 r
          qx (.-x q) qy (.-y q) qz (.-z q) qw (.-w q)
          rx (.-x r) ry (.-y r) rz (.-z r) rw (.-w r)
          x' (mm/maddsub x qw w qx y qz z qy)
          y' (mm/maddsub y qw w qy z qx x qz)
          z' (mm/maddsub z qw w qz x qy y qx)
          w' (mm/msub w qw x qx y qy z qz)]
      (Quat4.
       (mm/maddsub x' rw w' rx y' rz z' ry)
       (mm/maddsub y' rw w' ry z' rx x' rz)
       (mm/maddsub z' rw w' rz x' ry y' rx)
       (mm/msub w' rw x' rx y' ry z' rz)
       -1 #?(:clj -1))))

  m/IDeltaEquals
  (delta=
    [_ q] (m/delta= _ q *eps*))
  (delta=
    [_ q eps]
    (and (sequential? q)
         (== 4 (count q))
         (m/delta= x (first q) eps)
         (m/delta= y (nth q 1) eps)
         (m/delta= z (nth q 2) eps)
         (m/delta= w (nth q 3) eps)))

  m/IDotProduct
  (dot
    [_ q]
    (let [q ^Quat4 q]
      (mm/madd x (.-x q) y (.-y q) z (.-z q) w (.-w q))))

  m/IMagnitude
  (mag
    [_] (Math/sqrt (mm/madd x x y y z z w w)))
  (mag-squared
    [_] (mm/madd x x y y z z w w))

  m/INormalize
  (normalize [_]
    (let [m (Math/sqrt (mm/madd x x y y z z w w))]
      (if (> m *eps*)
        (Quat4. (/ x m) (/ y m) (/ z m) (/ w m) -1 #?(:clj -1))
        _)))

  m/IConjugate
  (conjugate [_]
    (Quat4. (- x) (- y) (- z) w -1 #?(:clj -1)))

  m/IInvert
  (invert [_]
    (let [d (m/mag-squared _)
          d (if (zero? d) 0.0 (/ d))
          id (- d)]
      (Quat4. (* x id) (* y id) (* z id) (* w d) -1 #?(:clj -1))))

  m/IInterpolate
  (mix [_ q] (m/mix _ q 0.5))
  (mix [_ q t]
    (let [d (m/dot _ q)]
      (if (< (m/abs* d) 1.0)
        (let [theta (Math/acos d)
              stheta (Math/sqrt (mm/madd d (- d) 1.0))
              [a b] (if (< (m/abs* stheta) 0.001)
                      [0.5 0.5]
                      [(/ (Math/sin (mm/subm 1.0 t theta)) stheta)
                       (/ (Math/sin (* t theta)) stheta)])
              q ^Quat4 q]
          (Quat4.
           (mm/madd x a (.-x q) b)
           (mm/madd y a (.-y q) b)
           (mm/madd z a (.-z q) b)
           (mm/madd w a (.-w q) b)
           -1 #?(:clj -1)))
        _)))

  g/IMatrixConvert
  (as-matrix
    [_] (g/as-matrix _ nil))
  (as-matrix
    [_ opts]
    (let [[tx ty tz] (get opts :translate)
          x2 (+ x x)
          y2 (+ y y)
          z2 (+ z z)
          xx (* x x2)
          xy (* x y2)
          xz (* x z2)
          yy (* y y2)
          yz (* y z2)
          zz (* z z2)
          wx (* w x2)
          wy (* w y2)
          wz (* w z2)]
      (thi.ng.geom.matrix.Matrix44.
       (- 1.0 (+ yy zz)) (+ xy wz) (- xz wy) 0.0
       (- xy wz) (- 1.0 (+ xx zz)) (+ yz wx) 0.0
       (+ xz wy) (- yz wx) (- 1.0 (+ xx yy)) 0.0
       (or tx 0.0) (or ty 0.0) (or tz 0.0) 1.0
       nil nil)))

  g/IVectorTransform
  (transform-vector
    [_ [vx vy vz :as v]]
    (let [ix (mm/maddsub w vx y vz z vy)
          iy (mm/maddsub w vy z vx x vz)
          iz (mm/maddsub w vz x vy y vx)
          nx (- x) ny (- y) nz (- z)
          iw (mm/msub nx vx y vy z vz)
          ^doubles b #?(:clj (double-array 3) :cljs (js/Float32Array. 3))]
      (aset b 0 (double (mm/maddsub ix w iw nx iy nz iz ny)))
      (aset b 1 (double (mm/maddsub iy w iw ny iz nx ix nz)))
      (aset b 2 (double (mm/maddsub iz w iw nz ix ny iy nx)))
      (thi.ng.geom.vector.Vec3. b nil (meta v)))))

#?(:clj (defmethod print-method Quat4 [^Quat4 o ^java.io.Writer w] (.write w (.toString o))))

(def Q (Quat4. 0.0 0.0 0.0 1.0 -1 #?(:clj -1)))

(defn quat
  ([] Q)
  ([[x y z] w] (Quat4. x y z w -1 #?(:clj -1)))
  ([[x y z w]] (Quat4. x y z w -1 #?(:clj -1)))
  ([x y z w] (Quat4. x y z w -1 #?(:clj -1))))

(defn quat-from-axis-angle
  [axis theta]
  (let [theta (/ theta 2.0)]
    (quat (m/normalize (vec3 axis) (Math/sin theta)) (Math/cos theta))))

(defn as-axis-angle
  [q]
  (let [[x y z w] (m/normalize q)
        theta (* 2.0 (Math/acos w))
        mag (Math/sqrt (- 1.0 (* w w)))]
    (if (< mag 0.0005)
      [(vec3 x y z) theta]
      [(vec3 (/ x mag) (/ y mag) (/ z mag)) theta])))

(defn quat-from-euler
  [order alpha beta gamma]
  (let [[a b c] (case order
                  :xyz [V3X V3Y V3Z]
                  :yxz [V3Y V3X V3Z]
                  :xzy [V3X V3Z V3Y]
                  :zxy [V3Z V3X V3Y]
                  :yzx [V3Y V3Z V3X]
                  :zyx [V3Z V3Y V3X]
                  (err/illegal-arg! order))]
    (m/* (quat-from-axis-angle a alpha)
         (quat-from-axis-angle b beta)
         (quat-from-axis-angle c gamma))))

;; Allan and Mark Watt's "Advanced Animation and Rendering Techniques"
;; (ACM Press 1992)
;; http://www.euclideanspace.com/maths/geometry/rotations/conversions/matrixToQuaternion/

(defn quat-from-matrix
  [^Matrix44 m]
  (let [trace (mm/add (.-m00 m) (.-m11 m) (.-m22 m))]
    (if (pos? trace)
      (let [s (/ 0.5 (Math/sqrt (inc trace)))]
        (Quat4.
         (mm/subm (.-m12 m) (.-m21 m) s)
         (mm/subm (.-m20 m) (.-m02 m) s)
         (mm/subm (.-m01 m) (.-m10 m) s)
         (/ 0.25 s)
         -1 #?(:clj -1)))
      (if (and (> (.-m00 m) (.-m11 m)) (> (.-m00 m) (.-m22 m)))
        (let [s (* 2.0 (Math/sqrt (inc (mm/sub (.-m00 m) (.-m11 m) (.-m22 m)))))
              s' (/ s)]
          (Quat4.
           (* 0.25 s)
           (mm/addm (.-m10 m) (.-m01 m) s')
           (mm/addm (.-m20 m) (.-m02 m) s')
           (mm/subm (.-m12 m) (.-m21 m) s')
           -1 #?(:clj -1)))
        (if (> (.-m11 m) (.-m22 m))
          (let [s (* 2.0 (Math/sqrt (inc (mm/sub (.-m11 m) (.-m22 m) (.-m00 m)))))
                s' (/ s)]
            (Quat4.
             (mm/addm (.-m10 m) (.-m01 m) s')
             (* 0.25 s)
             (mm/addm (.-m21 m) (.-m12 m) s')
             (mm/subm (.-m20 m) (.-m02 m) s')
             -1 #?(:clj -1)))
          (let [s (* 2.0 (Math/sqrt (inc (mm/sub (.-m22 m) (.-m00 m) (.-m11 m)))))
                s' (/ s)]
            (Quat4.
             (mm/addm (.-m20 m) (.-m02 m) s')
             (mm/addm (.-m21 m) (.-m12 m) s')
             (* 0.25 s)
             (mm/subm (.-m01 m) (.-m10 m) s')
             -1 #?(:clj -1))))))))

(defn alignment-quat
  [from to]
  (let [from  (m/normalize from)
        to    (m/normalize to)
        axis  (m/cross from to)
        theta (Math/atan2 (m/mag axis) (m/dot from to))]
    (quat-from-axis-angle axis theta)))

(defn lookup4
  [^Quat4 _ k nf]
  (case k
    \x (.-x _)
    \y (.-y _)
    \z (.-z _)
    \w (.-w _)
    (or nf (err/key-error! k))))

(defn swizzle4
  [^Quat4 _ k default]
  (if (number? k)
    (case (int k)
      0 (.-x _)
      1 (.-y _)
      2 (.-z _)
      3 (.-w _)
      (or default (err/key-error! k)))
    (case k
      :x (.-x _)
      :y (.-y _)
      :z (.-z _)
      :w (.-w _)
      (let [n (name k) c (count n)]
        (case c
          2 (vec2
             (lookup4 _ (first n) default)
             (lookup4 _ (nth n 1) default))
          3 (vec3
             (lookup4 _ (first n) default)
             (lookup4 _ (nth n 1) default)
             (lookup4 _ (nth n 2) default))
          4 (Quat4.
             (lookup4 _ (first n) default)
             (lookup4 _ (nth n 1) default)
             (lookup4 _ (nth n 2) default)
             (lookup4 _ (nth n 3) default)
             -1 #?(:clj -1))
          (or default (err/key-error! k)))))))

(defn swizzle-assoc*
  [_ ctor keymap k v]
  (let [n (name k)
        c (count n)]
    (if (and (<= c (count keymap)) (== c (count v) (count (into #{} n))))
      (loop [acc (vec (seq _)), i 0, n n]
        (if n
          (recur (assoc acc (keymap (first n)) (v i)) (inc i) (next n))
          (ctor acc)))
      (err/key-error! k))))

(defn swizzle4-assoc
  [^Quat4 _ k v]
  (if (number? k)
    (case (int k)
      0 (Quat4. v (.-y _) (.-z _) (.-w _) -1 #?(:clj -1))
      1 (Quat4. (.-x _) v (.-z _) (.-w _) -1 #?(:clj -1))
      2 (Quat4. (.-x _) (.-y _) v (.-w _) -1 #?(:clj -1))
      3 (Quat4. (.-x _) (.-y _) (.-z _) v -1 #?(:clj -1))
      (err/key-error! k))
    (case k
      :x (Quat4. v (.-y _) (.-z _) (.-w _) -1 #?(:clj -1))
      :y (Quat4. (.-x _) v (.-z _) (.-w _) -1 #?(:clj -1))
      :z (Quat4. (.-x _) (.-y _) v (.-w _) -1 #?(:clj -1))
      :w (Quat4. (.-x _) (.-y _) (.-z _) v -1 #?(:clj -1))
      (swizzle-assoc* _ quat {\x 0 \y 1 \z 2 \w 3} k v))))
