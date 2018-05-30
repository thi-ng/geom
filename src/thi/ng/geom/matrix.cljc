(ns thi.ng.geom.matrix
  #?(:cljs
     (:require-macros
      [thi.ng.math.macros :as mm]
      [thi.ng.geom.macros.matrix :as gmc]))
  (:require
   [thi.ng.geom.core :as g]
   [thi.ng.geom.utils :as gu]
   [thi.ng.geom.vector :as v :refer [vec2 vec3 V3 V3X]]
   [thi.ng.math.core :as m :refer [*eps* PI TWO_PI]]
   [thi.ng.xerror.core :as err]
   #?@(:clj
       [[thi.ng.math.macros :as mm]
        [thi.ng.geom.macros.matrix :as gmc]]))
  #?(:clj
     (:import
      [thi.ng.geom.vector Vec2 Vec3])))

#?(:clj
   (defn- hash-coll*
     [coll]
     (reduce
      #(-> % (unchecked-multiply-int 31) (unchecked-add-int (hash %2)))
      1 coll)))

(deftype Matrix32
    #?(:clj
       [^double m00 ^double m01 ^double m02
        ^double m10 ^double m11 ^double m12
        ^:unsynchronized-mutable _hasheq
        _meta]
       :cljs
       [m00 m01 m02 m10 m11 m12
        ^:mutable _hasheq
        _meta])

  #?@(:clj
      [clojure.lang.IObj
       (meta
        [_] _meta)
       (withMeta
        [_ m] (Matrix32. m00 m01 m02 m10 m11 m12 _hasheq m))

       java.util.Collection
       clojure.lang.IPersistentCollection
       clojure.lang.IPersistentVector
       clojure.lang.Seqable
       clojure.lang.Sequential
       clojure.lang.IHashEq
       (empty
        [_] (err/unsupported!))
       (count
        [_] 6)
       (seq
        [_] (seq [m00 m01 m02 m10 m11 m12]))
       (cons
        [_ x] [m00 m01 m02 m10 m11 m12 x])
       (nth
        [_ k]
        (case (int k)
          0 m00 1 m01 2 m02 3 m10 4 m11 5 m12
          (err/illegal-arg! k)))
       (nth
        [_ k nf]
        (case (int k) 0 m00 1 m01 2 m02 3 m10 4 m11 5 m12 nf))
       (equiv
        [_ o]
        (and
         (sequential? o)
         (== 6 (count o))
         (every? #(= (% 0) (% 1)) (map vector _ o))))
       (hasheq
        [_]
        (or _hasheq
            (set! _hasheq (hash-ordered-coll _))))
       (hashCode
        [_] (hash-coll* _))
       (equals
        [_ o]
        (and
         (sequential? o)
         (== 6 (count o))
         (every? #(clojure.lang.Util/equals (% 0) (% 1)) (map vector _ o))))
       (isEmpty
        [_] false)
       (iterator
        [_] (.iterator ^java.util.Collection (list m00 m01 m02 m10 m11 m12)))
       (toArray
        [_] (object-array _))
       (size
        [_] 6)
       (length
        [_] 6)]

      :cljs
      [IMeta
       (-meta
        [_] _meta)

       IWithMeta
       (-with-meta
        [_ m] (Matrix32. m00 m01 m02 m10 m11 m12 _hasheq m))

       ISequential
       ICounted
       (-count
        [_] 6)

       ISeqable
       (-seq
        [_] _)

       ISeq
       (-rest
        [_] (seq [m01 m02 m10 m11 m12]))
       (-first
        [_] m00)

       INext
       (-next
        [_] (seq [m01 m02 m10 m11 m12]))

       ICollection
       (-conj
        [_ x] [m00 m01 m02 m10 m11 m12 x])

       IIndexed
       (-nth
        [_ k]
        (case (int k)
          0 m00 1 m01 2 m02 3 m10 4 m11 5 m12
          (err/illegal-arg! k)))
       (-nth
        [_ k nf]
        (case (int k) 0 m00 1 m01 2 m02 3 m10 4 m11 5 m12 nf))

       IEquiv
       (-equiv
        [_ o]
        (and
         (sequential? o)
         (== 6 (count o))
         (every? #(= (% 0) (% 1)) (map vector _ o))))

       IHash
       (-hash
        [_]
        (or _hasheq
            (set! _hasheq (hash-ordered-coll _))))])

  Object
  (toString
    [_] (str "[" m00 " " m01 " " m02 " " m10 " " m11 " " m12 "]"))

  m/IMathOps
  (+
    [_ m]
    (let [m ^Matrix32 m]
      (Matrix32.
       (+ m00 (.-m00 m)) (+ m01 (.-m01 m)) (+ m02 (.-m02 m))
       (+ m10 (.-m10 m)) (+ m11 (.-m11 m)) (+ m12 (.-m12 m))
       nil _meta)))
  (-
    [_ m]
    (let [m ^Matrix32 m]
      (Matrix32.
       (- m00 (.-m00 m)) (- m01 (.-m01 m)) (- m02 (.-m02 m))
       (- m10 (.-m10 m)) (- m11 (.-m11 m)) (- m12 (.-m12 m))
       nil _meta)))
  (*
    [_ m]
    (let [m ^Matrix32 m]
      (Matrix32.
       (mm/madd m00 (.-m00 m) m01 (.-m10 m))
       (mm/madd m00 (.-m01 m) m01 (.-m11 m))
       (mm/madd m00 (.-m02 m) m01 (.-m12 m) m02)
       (mm/madd m10 (.-m00 m) m11 (.-m10 m))
       (mm/madd m10 (.-m01 m) m11 (.-m11 m))
       (mm/madd m10 (.-m02 m) m11 (.-m12 m) m12)
       nil _meta)))

  m/IDeltaEquals
  (delta=
    [_ m] (m/delta= _ m *eps*))
  (delta=
    [_ m eps]
    (and (sequential? m)
         (== 6 (count m))
         (m/delta= m00 (first m) eps)
         (m/delta= m01 (nth m 1) eps)
         (m/delta= m02 (nth m 2) eps)
         (m/delta= m10 (nth m 3) eps)
         (m/delta= m11 (nth m 4) eps)
         (m/delta= m12 (nth m 5) eps)))

  m/IDeterminant
  (determinant
    [_] (mm/msub m00 m11 m01 m10))

  m/IInvert
  (invert
    [_]
    (let [d (m/determinant _)]
      (when-not (zero? d)
        (Matrix32.
         (/ m11 d) (- (/ m01 d)) (/ (mm/msub m01 m12 m11 m02) d)
         (- (/ m10 d)) (/ m00 d) (/ (mm/msub m10 m02 m00 m12) d)
         nil _meta))))

  m/ITranspose
  (transpose
    [_]
    (Matrix32. m00 m10 m01 m11 m02 m12 nil _meta))

  g/IRotate
  (rotate
    [_ theta]
    (let [s (Math/sin theta), c (Math/cos theta)]
      (m/* _ (Matrix32. c (- s) 0.0, s c 0.0 nil _meta))))

  g/IScale
  (scale
    [_ s]
    (m/* _ (Matrix32.
            (if (number? s) s (nth s 0)) 0.0 0.0
            0.0 (if (number? s) s (nth s 1)) 0.0
            nil _meta)))

  g/IShear
  (shear
    [_ s]
    (m/* _ (Matrix32.
            1.0 (if (number? s) s (nth s 0)) 0.0,
            (if (number? s) s (nth s 1)) 1.0 0.0
            nil _meta)))

  g/ITranslate
  (translate
    [_ t]
    (m/* _ (Matrix32.
            1.0 0.0 (if (number? t) t (nth t 0))
            0.0 1.0 (if (number? t) t (nth t 1))
            nil _meta)))

  g/ITransform
  (transform
    [_ matrix] (m/* _ matrix))

  g/IVectorTransform
  (transform-vector
    [_ [x y :as v]]
    (let [^doubles b #?(:clj (double-array 2) :cljs (js/Float32Array. 2))]
      (aset b 0 (double (mm/madd x m00 y m01 m02)))
      (aset b 1 (double (mm/madd x m10 y m11 m12)))
      (thi.ng.geom.vector.Vec2. b nil (meta v)))))

;; This matrix implementation is using the same ordering as
;; OpenGL/WebGl, which expects matrices passed to it in column-major
;; order. In other words, each successive 4 values of this matrix
;; refer to a single column. Please bear this in mind, since the
;; visual layout of values in some parts of the source code below
;; seems to indicate the opposite ordering (row major).

(deftype Matrix44
    #?(:clj
       [^double m00 ^double m01 ^double m02 ^double m03
        ^double m10 ^double m11 ^double m12 ^double m13
        ^double m20 ^double m21 ^double m22 ^double m23
        ^double m30 ^double m31 ^double m32 ^double m33
        ^:unsynchronized-mutable _hasheq
        _meta]
       :cljs
       [m00 m01 m02 m03
        m10 m11 m12 m13
        m20 m21 m22 m23
        m30 m31 m32 m33
        ^:mutable _hasheq
        _meta])

  #?@(:clj
      [clojure.lang.IObj
       (meta
        [_] _meta)
       (withMeta
        [_ m] (Matrix44. m00 m01 m02 m03 m10 m11 m12 m13 m20 m21 m22 m23 m30 m31 m32 m33 _hasheq m))

       java.util.Collection
       clojure.lang.IPersistentCollection
       clojure.lang.IPersistentVector
       clojure.lang.Seqable
       clojure.lang.Sequential
       clojure.lang.IHashEq
       (empty
        [_] (err/unsupported!))
       (count
        [_] 16)
       (seq
        [_]
        (seq [m00 m01 m02 m03
              m10 m11 m12 m13
              m20 m21 m22 m23
              m30 m31 m32 m33]))
       (cons
        [_ x]
        [m00 m01 m02 m03
         m10 m11 m12 m13
         m20 m21 m22 m23
         m30 m31 m32 m33 x])
       (nth
        [_ k]
        (case (int k)
          0 m00  1 m01  2 m02  3 m03
          4 m10  5 m11  6 m12  7 m13
          8 m20  9 m21  10 m22 11 m23
          12 m30 13 m31 14 m32 15 m33
          (err/illegal-arg! k)))
       (nth
        [_ k nf]
        (case (int k)
          0 m00  1 m01  2 m02  3 m03
          4 m10  5 m11  6 m12  7 m13
          8 m20  9 m21  10 m22 11 m23
          12 m30 13 m31 14 m32 15 m33
          nf))
       (equiv
        [_ o]
        (and
         (sequential? o) (== 16 (count o))
         (every? #(= (% 0) (% 1)) (map vector _ o))))
       (hasheq
        [_] (or _hasheq (set! _hasheq (hash-ordered-coll _))))
       (hashCode
        [_] (hash-coll* _))
       (equals
        [_ o]
        (and
         (sequential? o)
         (== 16 (count o))
         (every? #(clojure.lang.Util/equals (% 0) (% 1)) (map vector _ o))))
       (isEmpty
        [_] false)
       (iterator
        [_]
        (.iterator
         ^java.util.Collection
         (list
          m00 m01 m02 m03
          m10 m11 m12 m13
          m20 m21 m22 m23
          m30 m31 m32 m33)))
       (toArray
        [_] (object-array _))
       (size
        [_] 16)
       (length
        [_] 16)]

      :cljs
      [IMeta
       (-meta
        [_] _meta)

       IWithMeta
       (-with-meta
        [_ m] (Matrix44. m00 m01 m02 m03 m10 m11 m12 m13 m20 m21 m22 m23 m30 m31 m32 m33 _hasheq m))

       ISequential
       ICounted
       (-count
        [_] 16)

       ISeqable
       (-seq
        [_] _)

       ISeq
       (-rest
        [_] (next _))
       (-first
        [_] m00)

       INext
       (-next
        [_]
        (seq [m01 m02 m03
              m10 m11 m12 m13
              m20 m21 m22 m23
              m30 m31 m32 m33]))

       ICollection
       (-conj
        [_ x]
        [m00 m01 m02 m03
         m10 m11 m12 m13
         m20 m21 m22 m23
         m30 m31 m32 m33 x])

       IIndexed
       (-nth
        [_ k]
        (case (int k)
          0 m00  1 m01  2 m02  3 m03
          4 m10  5 m11  6 m12  7 m13
          8 m20  9 m21  10 m22 11 m23
          12 m30 13 m31 14 m32 15 m33
          (err/illegal-arg! k)))
       (-nth
        [_ k nf]
        (case (int k)
          0 m00  1 m01  2 m02  3 m03
          4 m10  5 m11  6 m12  7 m13
          8 m20  9 m21  10 m22 11 m23
          12 m30 13 m31 14 m32 15 m33
          nf))

       IEquiv
       (-equiv
        [_ o]
        (and
         (sequential? o) (== 16 (count o))
         (every? #(= (% 0) (% 1)) (map vector _ o))))

       IHash
       (-hash
        [_] (or _hasheq (set! _hasheq (hash-ordered-coll _))))])

  Object
  (toString
    [_] (apply str (concat "[" (interpose \space _) "]")))

  m/IMathOps
  (+
    [_ m]
    (let [m ^Matrix44 m]
      (Matrix44.
       (+ m00 (.-m00 m)) (+ m01 (.-m01 m)) (+ m02 (.-m02 m)) (+ m03 (.-m03 m))
       (+ m10 (.-m10 m)) (+ m11 (.-m11 m)) (+ m12 (.-m12 m)) (+ m13 (.-m13 m))
       (+ m20 (.-m20 m)) (+ m21 (.-m21 m)) (+ m22 (.-m22 m)) (+ m23 (.-m23 m))
       (+ m30 (.-m30 m)) (+ m31 (.-m31 m)) (+ m32 (.-m32 m)) (+ m33 (.-m33 m))
       nil _meta)))
  (-
    [_ m]
    (let [m ^Matrix44 m]
      (Matrix44.
       (- m00 (.-m00 m)) (- m01 (.-m01 m)) (- m02 (.-m02 m)) (- m03 (.-m03 m))
       (- m10 (.-m10 m)) (- m11 (.-m11 m)) (- m12 (.-m12 m)) (- m13 (.-m13 m))
       (- m20 (.-m20 m)) (- m21 (.-m21 m)) (- m22 (.-m22 m)) (- m23 (.-m23 m))
       (- m30 (.-m30 m)) (- m31 (.-m31 m)) (- m32 (.-m32 m)) (- m33 (.-m33 m))
       nil _meta)))
  (*
    [_ m]
    (let [^Matrix44 m m]
      (Matrix44.
       (mm/madd m00 (.-m00 m) m10 (.-m01 m) m20 (.-m02 m) m30 (.-m03 m))
       (mm/madd m01 (.-m00 m) m11 (.-m01 m) m21 (.-m02 m) m31 (.-m03 m))
       (mm/madd m02 (.-m00 m) m12 (.-m01 m) m22 (.-m02 m) m32 (.-m03 m))
       (mm/madd m03 (.-m00 m) m13 (.-m01 m) m23 (.-m02 m) m33 (.-m03 m))

       (mm/madd m00 (.-m10 m) m10 (.-m11 m) m20 (.-m12 m) m30 (.-m13 m))
       (mm/madd m01 (.-m10 m) m11 (.-m11 m) m21 (.-m12 m) m31 (.-m13 m))
       (mm/madd m02 (.-m10 m) m12 (.-m11 m) m22 (.-m12 m) m32 (.-m13 m))
       (mm/madd m03 (.-m10 m) m13 (.-m11 m) m23 (.-m12 m) m33 (.-m13 m))

       (mm/madd m00 (.-m20 m) m10 (.-m21 m) m20 (.-m22 m) m30 (.-m23 m))
       (mm/madd m01 (.-m20 m) m11 (.-m21 m) m21 (.-m22 m) m31 (.-m23 m))
       (mm/madd m02 (.-m20 m) m12 (.-m21 m) m22 (.-m22 m) m32 (.-m23 m))
       (mm/madd m03 (.-m20 m) m13 (.-m21 m) m23 (.-m22 m) m33 (.-m23 m))

       (mm/madd m00 (.-m30 m) m10 (.-m31 m) m20 (.-m32 m) m30 (.-m33 m))
       (mm/madd m01 (.-m30 m) m11 (.-m31 m) m21 (.-m32 m) m31 (.-m33 m))
       (mm/madd m02 (.-m30 m) m12 (.-m31 m) m22 (.-m32 m) m32 (.-m33 m))
       (mm/madd m03 (.-m30 m) m13 (.-m31 m) m23 (.-m32 m) m33 (.-m33 m))
       nil _meta)))

  m/IDeltaEquals
  (delta=
    [_ m] (m/delta= _ m *eps*))
  (delta=
    [_ m eps]
    (and (sequential? m) (== 16 (count m)) (m/delta= (into [] _) m eps)))

  m/IDeterminant
  (determinant
    [_]
    (let [b00 (mm/msub m00 m11 m01 m10)
          b01 (mm/msub m00 m12 m02 m10)
          b02 (mm/msub m00 m13 m03 m10)
          b03 (mm/msub m01 m12 m02 m11)
          b04 (mm/msub m01 m13 m03 m11)
          b05 (mm/msub m02 m13 m03 m12)
          b06 (mm/msub m20 m31 m21 m30)
          b07 (mm/msub m20 m32 m22 m30)
          b08 (mm/msub m20 m33 m23 m30)
          b09 (mm/msub m21 m32 m22 m31)
          b10 (mm/msub m21 m33 m23 m31)
          b11 (mm/msub m22 m33 m23 m32)]
      (+ (mm/msub b00 b11 b01 b10 b04 b07)
         (mm/madd b02 b09 b03 b08 b05 b06))))

  m/IInvert
  (invert
    [_]
    (let [n00 (mm/msub m00 m11 m01 m10)
          n01 (mm/msub m00 m12 m02 m10)
          n02 (mm/msub m00 m13 m03 m10)
          n03 (mm/msub m01 m12 m02 m11)
          n04 (mm/msub m01 m13 m03 m11)
          n05 (mm/msub m02 m13 m03 m12)
          n06 (mm/msub m20 m31 m21 m30)
          n07 (mm/msub m20 m32 m22 m30)
          n08 (mm/msub m20 m33 m23 m30)
          n09 (mm/msub m21 m32 m22 m31)
          n10 (mm/msub m21 m33 m23 m31)
          n11 (mm/msub m22 m33 m23 m32)
          d (+ (mm/msub n00 n11 n01 n10 n04 n07)
               (mm/madd n02 n09 n03 n08 n05 n06))]
      (when-not (zero? d)
        (let [invd (/ 1.0 d)]
          (Matrix44.
           (gmc/inv-item m11 n11 m12 n10 m13 n09 invd)
           (gmc/inv-item m02 n10 m03 n09 (- m01) n11 invd)
           (gmc/inv-item m31 n05 m32 n04 m33 n03 invd)
           (gmc/inv-item m22 n04 m23 n03 (- m21) n05 invd)
           (gmc/inv-item m12 n08 m13 n07 (- m10) n11 invd)
           (gmc/inv-item m00 n11 m02 n08 m03 n07 invd)
           (gmc/inv-item m32 n02 m33 n01 (- m30) n05 invd)
           (gmc/inv-item m20 n05 m22 n02 m23 n01 invd)
           (gmc/inv-item m10 n10 m11 n08 m13 n06 invd)
           (gmc/inv-item m01 n08 m03 n06 (- m00) n10 invd)
           (gmc/inv-item m30 n04 m31 n02 m33 n00 invd)
           (gmc/inv-item m21 n02 m23 n00 (- m20) n04 invd)
           (gmc/inv-item m11 n07 m12 n06 (- m10) n09 invd)
           (gmc/inv-item m00 n09 m01 n07 m02 n06 invd)
           (gmc/inv-item m31 n01 m32 n00 (- m30) n03 invd)
           (gmc/inv-item m20 n03 m21 n01 m22 n00 invd)
           nil _meta)))))

  m/ITranspose
  (transpose
    [_]
    (Matrix44.
     m00 m10 m20 m30
     m01 m11 m21 m31
     m02 m12 m22 m32
     m03 m13 m23 m33
     nil _meta))

  g/IScale
  (scale
    [_ s]
    (let [[x y z] (if (number? s) [s s s] s)]
      (Matrix44.
       (* m00 x) (* m01 x) (* m02 x) (* m03 x)
       (* m10 y) (* m11 y) (* m12 y) (* m13 y)
       (* m20 z) (* m21 z) (* m22 z) (* m23 z)
       m30 m31 m32 m33
       nil _meta)))

  g/IRotate
  (rotate
    [_ theta] (g/rotate-z _ theta))

  g/IRotate3D
  (rotate-x
    [_ theta]
    (let [s (Math/sin theta), c (Math/cos theta)]
      (Matrix44.
       m00 m01 m02 m03
       (mm/madd m10 c m20 s) (mm/madd m11 c m21 s) (mm/madd m12 c m22 s) (mm/madd m13 c m23 s)
       (mm/msub m20 c m10 s) (mm/msub m21 c m11 s) (mm/msub m22 c m12 s) (mm/msub m23 c m13 s)
       m30 m31 m32 m33
       nil _meta)))

  (rotate-y
    [_ theta]
    (let [s (Math/sin theta), c (Math/cos theta)]
      (Matrix44.
       (mm/msub m00 c m20 s) (mm/msub m01 c m21 s) (mm/msub m02 c m22 s) (mm/msub m03 c m23 s)
       m10 m11 m12 m13
       (mm/madd m00 s m20 c) (mm/madd m01 s m21 c) (mm/madd m02 s m22 c) (mm/madd m03 s m23 c)
       m30 m31 m32 m33
       nil _meta)))

  (rotate-z
    [_ theta]
    (let [s (Math/sin theta), c (Math/cos theta)]
      (Matrix44.
       (mm/madd m00 c m10 s) (mm/madd m01 c m11 s) (mm/madd m02 c m12 s) (mm/madd m03 c m13 s)
       (mm/msub m10 c m00 s) (mm/msub m11 c m01 s) (mm/msub m12 c m02 s) (mm/msub m13 c m03 s)
       m20 m21 m22 m23
       m30 m31 m32 m33
       nil _meta)))

  (rotate-around-axis
    [_ [x y z] theta]
    (let [s (Math/sin theta), c (Math/cos theta)
          sx (* s x)  sy (* s y) sz (* s z)
          t (- 1.0 c) tx (* t x) ty (* t y)
          b00 (mm/madd tx x c)  b01 (mm/madd tx y sz) b02 (mm/msub tx z sy)
          b10 (mm/msub ty x sz) b11 (mm/madd ty y c)  b12 (mm/madd ty z sx)
          b20 (mm/madd tx z sy) b21 (mm/msub ty z sx) b22 (mm/madd (* t z) z c)]
      (Matrix44.
       (mm/madd m00 b00 m10 b01 m20 b02)
       (mm/madd m01 b00 m11 b01 m21 b02)
       (mm/madd m02 b00 m12 b01 m22 b02)
       (mm/madd m03 b00 m13 b01 m23 b02)
       (mm/madd m00 b10 m10 b11 m20 b12)
       (mm/madd m01 b10 m11 b11 m21 b12)
       (mm/madd m02 b10 m12 b11 m22 b12)
       (mm/madd m03 b10 m13 b11 m23 b12)
       (mm/madd m00 b20 m10 b21 m20 b22)
       (mm/madd m01 b20 m11 b21 m21 b22)
       (mm/madd m02 b20 m12 b21 m22 b22)
       (mm/madd m03 b20 m13 b21 m23 b22)
       m30 m31 m32 m33
       nil _meta)))

  g/ITranslate
  (translate
    [_ t]
    (let [[x y z] (if (number? t) [t t t] t)]
      (Matrix44.
       m00 m01 m02 m03
       m10 m11 m12 m13
       m20 m21 m22 m23
       (mm/madd m00 x m10 y m20 z m30)
       (mm/madd m01 x m11 y m21 z m31)
       (mm/madd m02 x m12 y m22 z m32)
       (mm/madd m03 x m13 y m23 z m33)
       nil _meta)))

  g/ITransform
  (transform
    [_ matrix] (m/* _ matrix))

  g/IVectorTransform
  (transform-vector
    [_ [x y z w :as v]]
    (if w
      [(mm/madd x m00 y m10 z m20 w m30)
       (mm/madd x m01 y m11 z m21 w m31)
       (mm/madd x m02 y m12 z m22 w m32)
       (mm/madd x m03 y m13 z m23 w m33)]
      (let [^doubles b #?(:clj (double-array 3) :cljs (js/Float32Array. 3))]
        (aset b 0 (double (mm/madd x m00 y m10 z m20 m30)))
        (aset b 1 (double (mm/madd x m01 y m11 z m21 m31)))
        (aset b 2 (double (mm/madd x m02 y m12 z m22 m32)))
        (thi.ng.geom.vector.Vec3. b nil (meta v))))))

#?(:clj (defmethod print-method Matrix32 [^Matrix32 o ^java.io.Writer w] (.write w (.toString o))))
#?(:clj (defmethod print-method Matrix44 [^Matrix44 o ^java.io.Writer w] (.write w (.toString o))))

(def M32
  (Matrix32.
   1.0 0.0 0.0
   0.0 1.0 0.0
   nil nil))

(def M44
  (Matrix44.
   1.0 0.0 0.0 0.0
   0.0 1.0 0.0 0.0
   0.0 0.0 1.0 0.0
   0.0 0.0 0.0 1.0
   nil nil))

(defn matrix32
  ([] M32)
  ([[m00 m01 m02 m10 m11 m12]]
   (Matrix32. m00 m01 m02 m10 m11 m12 nil nil))
  ([m00 m01 m02 m10 m11 m12]
   (Matrix32. m00 m01 m02 m10 m11 m12 nil nil)))

(defn matrix44
  ([] M44)
  ([[m00 m01 m02 m03 m10 m11 m12 m13 m20 m21 m22 m23 m30 m31 m32 m33]]
   (Matrix44. m00 m01 m02 m03 m10 m11 m12 m13 m20 m21 m22 m23 m30 m31 m32 m33 nil nil))
  ([m00 m01 m02 m03 m10 m11 m12 m13 m20 m21 m22 m23 m30 m31 m32 m33]
   (Matrix44. m00 m01 m02 m03 m10 m11 m12 m13 m20 m21 m22 m23 m30 m31 m32 m33 nil nil)))

;; FIXME create proper deftype for M33
(defn matrix44->matrix33
  [[m00 m01 m02 _ m10 m11 m12 _ m20 m21 m22]]
  [m00 m01 m02 m10 m11 m12 m20 m21 m22])

(defn matrix44->matrix33-rot
  [[m00 m01 m02 _ m10 m11 m12 _ m20 m21 m22]]
  (let [b01 (mm/msub m22 m11 m12 m21)
        b11 (mm/msub m12 m20 m22 m10)
        b21 (mm/msub m21 m10 m11 m20)
        invd (/ (mm/madd m00 b01 m01 b11 m02 b21))]
    [(* b01 invd)
     (* b11 invd)
     (* b21 invd)
     (* (mm/msub m02 m21 m22 m01) invd)
     (* (mm/msub m22 m00 m02 m20) invd)
     (* (mm/msub m01 m20 m21 m00) invd)
     (* (mm/msub m12 m01 m02 m11) invd)
     (* (mm/msub m02 m10 m12 m00) invd)
     (* (mm/msub m11 m00 m01 m10) invd)]))

;; https://github.com/stefanmirea/view-frustum-culling/blob/master/lab_camera.hpp#L163

(defn frustum
  "Sets up a viewing frustum, shaped like a truncated pyramid with the
  camera where the tip of the pyramid would be.
  This emulates the OpenGL function glFrustum()."
  [l t r b n f]
  (let [dx (/ (- r l))
        dy (/ (- t b))
        dz (/ (- n f))
        n2 (* 2.0 n)]
    (Matrix44.
     (* n2 dx) 0.0 0.0 0.0
     0.0 (* n2 dy) 0.0 0.0
     (mm/addm l r dx) (mm/addm t b dy) (mm/addm n f dz) -1.0
     0.0 0.0 (mm/mul n2 f dz) 0.0
     nil nil)))

(defn frustum-bounds
  "Given vertical FOV in degrees, aspect ratio and near plane
  distance, computes map of left/right/top/bottom view frustum
  bounds."
  [fovy aspect near]
  (let [top (* near (Math/tan (* 0.5 (m/radians fovy))))
        right (* top aspect)]
    {:left (- right)
     :right right
     :top top
     :bottom (- top)}))

(defn frustum-planes
  "Given a view matrix & projection matrix, returns vector of the
  frustum's 6 plane parameters, each a 2-element vector: [normal w]
  Planes are ordered: left, right, top, bottom, near, far. These
  coefficients can then (for example) be used for AABB-frustum culling
  with thi.ng.geom.utils.intersect/intersect-aabb-frustum?"
  [view proj]
  (let [^Matrix44 pv (m/transpose (m/* proj view))
        m30 (- (.-m30 pv))
        m31 (- (.-m31 pv))
        m32 (- (.-m32 pv))
        m33 (- (.-m33 pv))]
    [[(vec3 (- m30 (.-m00 pv)) (- m31 (.-m01 pv)) (- m32 (.-m02 pv))) (- m33 (.-m03 pv))]
     [(vec3 (+ m30 (.-m00 pv)) (+ m31 (.-m01 pv)) (+ m32 (.-m02 pv))) (+ m33 (.-m03 pv))]
     [(vec3 (- m30 (.-m10 pv)) (- m31 (.-m11 pv)) (- m32 (.-m12 pv))) (- m33 (.-m13 pv))]
     [(vec3 (+ m30 (.-m10 pv)) (+ m31 (.-m11 pv)) (+ m32 (.-m12 pv))) (+ m33 (.-m13 pv))]
     [(vec3 (- m30 (.-m20 pv)) (- m31 (.-m21 pv)) (- m32 (.-m22 pv))) (- m33 (.-m23 pv))]
     [(vec3 (+ m30 (.-m20 pv)) (+ m31 (.-m21 pv)) (+ m32 (.-m22 pv))) (+ m33 (.-m23 pv))]]))

(defn ortho
  "Returns an orthographic projection matrix, in which objects are the
  same size no matter how far away or nearby they are. This emulates
  the OpenGL function glOrtho()."
  ([]
   (ortho -1 -1 1 1 -1 1))
  ([view-rect]
   (let [a (apply / (get view-rect :size [1 1]))]
     (ortho (- a) 1 a -1 -1 1)))
  ([left top right bottom near far]
   (let[dx (/ (- left right))
        dy (/ (- bottom top))
        dz (/ (- near far))]
     (Matrix44.
      (* -2.0 dx) 0.0 0.0 0.0
      0.0 (* -2.0 dy) 0.0 0.0
      0.0 0.0 (* 2.0 dz) 0.0
      (mm/addm left right dx) (mm/addm top bottom dy) (mm/addm near far dz) 1.0
      nil nil))))

(defn perspective
  "Returns a perspective transform matrix, which makes far away
  objects appear smaller than nearby objects. `fovy` is the vertical
  angle of the field of view in degrees. The `aspect` argument should
  be a rect, a map with a `:size` key (vec2) or the width divided by
  the height of your viewport. `near` and `far` are the distances of
  the clipping planes."
  [fovy aspect near far]
  (let [f      (/ (Math/tan (* 0.5 (m/radians fovy))))
        nf     (/ (- near far))
        aspect (if (map? aspect) (apply / (get aspect :size [1 1])) aspect)]
    (Matrix44.
     (/ f aspect) 0.0 0.0 0.0
     0.0 f 0.0 0.0
     0.0 0.0 (mm/addm near far nf) -1.0
     0.0 0.0 (mm/mul 2.0 near far nf) 0.0
     nil nil)))

(defn perspective-frustum
  [fov aspect near far]
  (let [{:keys [left right top bottom]} (frustum-bounds fov aspect near)]
    (frustum left top right bottom near far)))

(defn look-at-vectors
  "Takes 6 numbers representing eye & target positions, computes up
  vector and returns vector of all three vec3's."
  [ex ey ez tx ty tz]
  (let [eye    (vec3 ex ey ez)
        target (vec3 tx ty tz)
        up     (gu/ortho-normal (m/- eye target) V3X)]
    [eye target up]))

(defn look-at
  "Returns a matrix that puts the camera at the eye position looking
  toward the target point with the given up direction."
  [eye target up]
  (let [dir (m/- eye target)]
    (if (m/delta= V3 dir)
      M44
      (let [[zx zy zz :as z] (m/normalize dir)
            [xx xy xz :as x] (gu/ortho-normal up z)
            [yx yy yz :as y] (gu/ortho-normal z x)]
        (Matrix44.
         xx yx zx 0.0
         xy yy zy 0.0
         xz yz zz 0.0
         (- (m/dot x eye)) (- (m/dot y eye)) (- (m/dot z eye)) 1.0
         nil nil)))))

(defn viewport-matrix
  "Given viewport width/height, computes a 2D transformation matrix
  mapping normalized coordinates to screen space. If invert-y? is
  true, the Y axis is flipped."
  ([width height]
   (viewport-matrix width height false))
  ([width height invert-y?]
   (let [w2 (/ width 2.0)
         h2 (/ height 2.0)]
     (-> M32
         (g/translate (vec2 w2 h2))
         (m/* (g/scale M32 (vec2 w2 (if invert-y? h2 (- h2)))))))))

(defn project-point
  "Projects 3D point p using MVP matrix into 2D and the applies
  viewport matrix to produce screen coordinate."
  [p mvp vtx]
  (let [[x y _ w] (g/transform-vector mvp (conj (vec3 p) 1))]
    (g/transform-vector vtx (m/div (vec2 x y) w))))

(defn project-point-z
  "Like project-point, but returns vec3 with z component representing
  depth value."
  [p mvp vtx]
  (let [[x y z w] (g/transform-vector mvp (conj (vec3 p) 1))]
    (vec3 (g/transform-vector vtx (m/div (vec2 x y) w)) (/ z w))))

(defn unproject-point
  "Takes a vec3 in screenspace, view matrix, projection matrix and
  screen rect. A second arity exists accepting an already inverted
  view-projection matrix instead of having to supply view & proj
  separately. Returns vector in world space or nil if matrix is not
  invertible."
  ([p view proj screen-rect]
   (if-let [inv-mat (m/invert (m/* proj view))]
     (unproject-point p inv-mat screen-rect)))
  ([p ^Matrix44 inv-mat {[vx vy] :p [w h] :size}]
   (unproject-point p inv-mat vx vy w h))
  ([[x y z] ^Matrix44 inv-mat vx vy w h]
   (let [x' (- (/ (* 2.0 (- x vx)) w) 1)
         y' (- (/ (* 2.0 (- (- h y 1) vy)) h) 1)
         z' (- (* z 2.0) 1)
         p' (g/transform-vector inv-mat [x' y' z'])]
     (m/* p' (/ (mm/madd
                 x' (.-m03 inv-mat)
                 y' (.-m13 inv-mat)
                 z' (.-m23 inv-mat)
                 (.-m33 inv-mat)))))))
