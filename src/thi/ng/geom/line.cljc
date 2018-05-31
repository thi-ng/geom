(ns thi.ng.geom.line
  #?(:cljs
     (:require-macros
      [thi.ng.math.macros :as mm]))
  (:require
   [thi.ng.geom.core :as g]
   [thi.ng.geom.utils :as gu]
   [thi.ng.geom.utils.intersect :as isec]
   [thi.ng.geom.vector :as v :refer [vec2 vec3]]
   [thi.ng.geom.attribs :as attr]
   [thi.ng.geom.basicmesh :as bm]
   #?(:clj [thi.ng.geom.types] :cljs [thi.ng.geom.types :refer [Circle2 Line2 Line3 LineStrip2 LineStrip3]])
   [thi.ng.dstruct.core :as d]
   [thi.ng.math.core :as m :refer [*eps*]]
   #?(:clj [thi.ng.math.macros :as mm]))
  #?(:clj
     (:import
      [thi.ng.geom.types Circle2 Line2 Line3 LineStrip2 LineStrip3])))

(defn line2
  ([[p q]] (Line2. [(vec2 p) (vec2 q)]))
  ([p q] (Line2. [(vec2 p) (vec2 q)]))
  ([px py qx qy] (Line2. [(vec2 px py) (vec2 qx qy)])))

(defn line3
  ([[p q]] (Line3. [(vec3 p) (vec3 q)]))
  ([p q] (Line3. [(vec3 p) (vec3 q)]))
  ([px py qx qy] (Line3. [(vec3 px py) (vec3 qx qy)]))
  ([px py pz qx qy qz] (Line3. [(vec3 px py pz) (vec3 qx qy qz)])))

(defn linestrip2
  ([points] (LineStrip2. (mapv vec2 points)))
  ([p q & more] (LineStrip2. (mapv vec2 (cons p (cons q more))))))

(defn linestrip3
  ([points] (LineStrip3. (mapv vec3 points)))
  ([p q & more] (LineStrip3. (mapv vec3 (cons p (cons q more))))))

;; These functions are used for both 2D/3D implementations:

(defn reflect-on-ray
  [ctor p q rc dir]
  (ctor
   (m/+ rc (g/reflect (m/- p rc) dir))
   (m/+ rc (g/reflect (m/- q rc) dir))))

(defn reflect-on-line
  [ctor p q rp rq]
  (reflect-on-ray ctor p q (m/mix rp rq) (m/normalize (m/- rq rp))))

(extend-type Line2

  g/IArea
  (area [_] 0)

  g/IBoundary
  (contains-point?
    [{p :points} q] (m/in-range? 0.0 1.0 (gu/closest-point-coeff (vec2 q) (nth p 0) (nth p 1))))

  g/IBounds
  (bounds [_] (gu/bounding-rect (get _ :points)))
  (width  [{p :points}] (m/abs* (- (nth (nth p 0) 0) (nth (nth p 1) 0))))
  (height [{p :points}] (m/abs* (- (nth (nth p 0) 1) (nth (nth p 1) 1))))
  (depth  [_] 0)

  g/IBoundingCircle
  (bounding-circle
    [{[p q] :points}]
    (Circle2. (m/mix p q) (* 0.5 (g/dist p q))))

  g/ICenter
  (center
    ([{[p q] :points}]
     (let [c (m/mix p q)]
       (Line2. [(m/- p c) (m/- q c)])))
    ([{[p q] :points} o]
     (let [c (m/- o (m/mix p q))]
       (Line2. [(m/+ p c) (m/+ q c)]))))
  (centroid [{p :points}] (m/mix (nth p 0) (nth p 1)))

  g/ICircumference
  (circumference [{p :points}] (g/dist (nth p 0) (nth p 1)))

  g/IClassify
  (classify-point
    [{[p q] :points} v]
    (m/signum (m/dot (m/- (vec2 v) p) (g/normal (m/- q p))) *eps*))

  g/IExtrude
  (extrude
    [{points :points :as _}
     {:keys [mesh depth scale offset attribs]
      :or {depth 1.0 scale 1.0} :as opts}]
    (let [tpoints (if (= 1.0 scale) points (get (g/scale-size _ scale) :points))
          off     (or offset (vec3 0 0 depth))
          a       (vec3 (nth points 0))
          b       (vec3 (nth points 1))
          ta      (m/+ off (nth tpoints 0))
          tb      (m/+ off (nth tpoints 1))]
      (g/add-face
       (or mesh (bm/basic-mesh))
       (attr/generate-face-attribs [a b tb ta] 0 attribs opts))))

  g/IVertexAccess
  (vertices
    ([_] (get _ :points))
    ([{p :points} res] (gu/sample-segment-with-res (nth p 0) (nth p 1) res true)))

  g/IEdgeAccess
  (edges
    ([_] [(get _ :points)])
    ([_ res] (d/successive-nth 2 (g/vertices _ res))))

  g/IGraph
  (vertex-neighbors
    [{[p q] :points} v] (condp = v, p q, q p, nil))
  (vertex-valence
    [{p :points} v] (if (or (= v (nth p 0)) (= v (nth p 1))) 1 0))

  g/IIntersect
  (intersect-line
    [{[a b] :points} l]
    (let [[c d] (get l :points l)]
      (isec/intersect-line2-line2? a b c d)))

  m/IMagnitude
  (mag [{p :points}] (g/dist (nth p 0) (nth p 1)))
  (mag-squared [{p :points}] (g/dist-squared (nth p 0) (nth p 1)))

  m/INormalize
  (normalize
    ([{[p q] :points}]
     (let [d (m/normalize (m/- q p))]
       (Line2. [p (m/+ p d)])))
    ([{[p q] :points} n]
     (let [d (m/normalize (m/- q p) n)]
       (Line2. [p (m/+ p d)]))))
  (normalized? [_] (m/delta= 1.0 (m/mag-squared _)))

  g/IProximity
  (closest-point
    [{p :points} q] (gu/closest-point-on-segment (vec2 q) (nth p 0) (nth p 1)))

  g/IReflect
  (reflect
    [{[p q] :points} r]
    (if (instance? Line2 r)
      (let [[pr qr] (get r :points)] (reflect-on-line line2 p q pr qr))
      (Line2. [(g/reflect p r) (g/reflect q r)])))
  g/ISample
  (point-at
    [{p :points} t] (m/mix (p 0) (p 1) t))
  (random-point
    [{p :points}] (m/mix (p 0) (p 1) (m/random)))
  (random-point-inside
    [{p :points}] (m/mix (p 0) (p 1) (m/random)))
  (sample-uniform
    [_ dist include-last?]
    (gu/sample-uniform dist include-last? (get _ :points)))

  g/IRotate
  (rotate
    [{p :points} theta]
    (Line2. [(g/rotate (nth p 0) theta) (g/rotate (nth p 1) theta)]))

  g/IScale
  (scale
    [{p :points} s] (Line2. [(m/* (nth p 0) s) (m/* (nth p 1) s)]))
  (scale-size
    [{[p q] :points} s]
    (let [c (m/mix p q)]
      (Line2. [(m/madd (m/- p c) s c) (m/madd (m/- q c) s c)])))

  g/ITranslate
  (translate
    [{p :points} t] (Line2. [(m/+ (nth p 0) t) (m/+ (nth p 1) t)]))

  g/ITransform
  (transform
    [{p :points} m]
    (Line2.
     [(g/transform-vector m (p 0)) (g/transform-vector m (p 1))]))

  g/IVolume
  (volume [_] 0.0))

(extend-type Line3

  g/IArea
  (area [_] 0.0)

  g/IBoundary
  (contains-point?
    [{p :points} q] (m/in-range? 0.0 1.0 (gu/closest-point-coeff (vec3 q) (nth p 0) (nth p 1))))

  g/IBounds
  (bounds [_] (gu/bounding-box (get _ :points)))
  (width  [{p :points}] (m/abs* (- (nth (nth p 0) 0) (nth (nth p 1) 0))))
  (height [{p :points}] (m/abs* (- (nth (nth p 0) 1) (nth (nth p 1) 1))))
  (depth  [{p :points}] (m/abs* (- (nth (nth p 0) 2) (nth (nth p 1) 2))))

  g/IBoundingSphere
  (bounding-sphere
    [{[p q] :points}]
    (gu/bounding-sphere (m/mix p q) (* 0.5 (g/dist p q))))

  g/ICenter
  (center
    ([{[p q] :points}]
     (let [c (m/mix p q)]
       (Line3. [(m/- p c) (m/- q c)])))
    ([{[p q] :points} o]
     (let [c (m/- o (m/mix p q))]
       (Line3. [(m/+ p c) (m/+ q c)]))))
  (centroid [{p :points}] (m/mix (nth p 0) (nth p 1)))

  g/ICircumference
  (circumference [{p :points}] (g/dist (nth p 0) (nth p 1)))

  g/IClassify
  (classify-point
    [{p :points} p] nil) ; TODO needs normal to define plane to classify

  g/IExtrude
  (extrude
    [{points :points :as _}
     {:keys [mesh depth scale offset attribs]
      :or {depth 1.0 scale 1.0} :as opts}]
    (let [tpoints (if (= 1.0 scale) points (get (g/scale-size _ scale) :points))
          off     (or offset (vec3 0 0 depth))
          a       (vec3 (nth points 0))
          b       (vec3 (nth points 1))
          ta      (m/+ off (nth tpoints 0))
          tb      (m/+ off (nth tpoints 1))]
      (g/add-face
       (or mesh (bm/basic-mesh))
       (attr/generate-face-attribs [a b tb ta] 0 attribs opts))))

  g/IVertexAccess
  (vertices
    ([_] (get _ :points))
    ([{p :points} res] (gu/sample-segment-with-res (nth p 0) (nth p 1) res true)))

  g/IEdgeAccess
  (edges
    ([_] [(get _ :points)])
    ([_ res] (d/successive-nth 2 (g/vertices _ res))))

  g/IGraph
  (vertex-neighbors
    [{[p q] :points} v] (condp = v, p q, q p, nil))
  (vertex-valence
    [{p :points} v] (if (or (= v (nth p 0)) (= v (nth p 1))) 1 0))

  g/IIntersect
  (intersect-line
    [{[a b] :points} l]
    (let [[c d] (get l :points l)]
      (gu/closest-line-between a b c d)))
  (intersect-ray [_ r])
  (intersect-shape [_ s])

  m/IMagnitude
  (mag [{p :points}] (g/dist (nth p 0) (nth p 1)))
  (mag-squared [{p :points}] (g/dist-squared (nth p 0) (nth p 1)))

  m/INormalize
  (normalize
    ([{[p q] :points}]
     (let [d (m/normalize (m/- q p))]
       (Line3. [p (m/+ p d)])))
    ([{[p q] :points} n]
     (let [d (m/normalize (m/- q p) n)]
       (Line3. [p (m/+ p d)]))))
  (normalized? [_] (m/delta= 1.0 (m/mag-squared _)))

  g/IProximity
  (closest-point
    [{p :points} q] (gu/closest-point-on-segment (vec3 q) (nth p 0) (nth p 1)))

  g/IReflect
  (reflect
    [{[p q] :points} r]
    (if (instance? Line3 r)
      (let [[pr qr] (get r :points)] (reflect-on-line line3 p q pr qr))
      (Line3. [(g/reflect p r) (g/reflect q r)])))

  g/ISample
  (point-at
    [{p :points} t] (m/mix (p 0) (p 1) t))
  (random-point
    [{p :points}] (m/mix (p 0) (p 1) (m/random)))
  (random-point-inside
    [{p :points}] (m/mix (p 0) (p 1) (m/random)))
  (sample-uniform
    [_ dist include-last?]
    (gu/sample-uniform dist include-last? (get _ :points)))

  g/IRotate3D
  (rotate-x
    [{p :points} theta]
    (Line3. [(g/rotate-x (nth p 0) theta) (g/rotate-x (nth p 1) theta)]))
  (rotate-y
    [{p :points} theta]
    (Line3. [(g/rotate-y (nth p 0) theta) (g/rotate-y (nth p 1) theta)]))
  (rotate-z
    [{p :points} theta]
    (Line3. [(g/rotate-z (nth p 0) theta) (g/rotate-z (nth p 1) theta)]))
  (rotate-around-axis
    [{p :points} axis theta]
    (Line3.
     [(g/rotate-around-axis (nth p 0) axis theta)
      (g/rotate-around-axis (nth p 1) axis theta)]))

  g/IRotate
  (rotate
    [{p :points} theta]
    (Line3. [(g/rotate (nth p 0) theta) (g/rotate (nth p 1) theta)]))

  g/IScale
  (scale
    [{p :points} s] (Line3. [(m/* (nth p 0) s) (m/* (nth p 1) s)]))
  (scale-size
    [{[p q] :points} s]
    (let [c (m/mix p q)]
      (Line3. [(m/madd (m/- p c) s c) (m/madd (m/- q c) s c)])))

  g/ITranslate
  (translate
    [{p :points} t] (Line3. [(m/+ (nth p 0) t) (m/+ (nth p 1) t)]))

  g/ITransform
  (transform
    [{p :points} m]
    (Line3.
     [(g/transform-vector m (p 0)) (g/transform-vector m (p 1))]))

  g/IVolume
  (volume [_] 0.0))

(extend-type LineStrip2

  g/IArea
  (area [_] 0.0)

  g/IBoundary
  (contains-point? [_ a])

  g/IBounds
  (bounds [_] (gu/bounding-rect (get _ :points)))
  (width [_] (gu/axis-range 0 (get _ :points)))
  (height [_] (gu/axis-range 1 (get _ :points)))
  (depth [_] 0)

  g/IBoundingCircle
  (bounding-circle
    [_] (gu/bounding-circle (g/centroid _) (get _ :points)))

  g/ICenter
  (center
    ([_] (LineStrip2. (gu/center (vec2) (get _ :points))))
    ([_ o] (LineStrip2. (gu/center (g/centroid _) (vec2 o) (get _ :points)))))
  (centroid
    [_] (gu/centroid (get _ :points)))

  g/ICircumference
  (circumference
    [_] (m/mag _))

  g/IClassify
  (classify-point [_ v])

  g/IExtrude
  (extrude
    [{:keys [points] :as _}
     {:keys [mesh depth offset scale attribs]
      :or {depth 1.0 scale 1.0} :as opts}]
    (let [tpoints (if (= 1.0 scale) points (get (g/scale-size _ scale) :points))
          off     (or offset (vec3 0 0 depth))
          bottom  (mapv vec3 points)
          top     (mapv #(m/+ off %) tpoints)]
      (->> (interleave
            (d/successive-nth 2 bottom)
            (d/successive-nth 2 top))
           (partition 2)
           (map-indexed
            (fn [i [[a1 a2] [b1 b2]]]
              (attr/generate-face-attribs [a1 b1 b2 a2] i attribs opts)))
           (g/into (or mesh (bm/basic-mesh))))))

  g/IVertexAccess
  (vertices
    ([_] (get _ :points))
    ([{points :points} res]
     (map #(gu/point-at % points) (m/norm-range res)))) ;; TODO transduce

  g/IEdgeAccess
  (edges
    ([_] (d/successive-nth 2 (get _ :points)))
    ([_ res] (d/successive-nth 2 (g/vertices _ res))))

  g/IGraph
  (vertex-neighbors
    [_ v] (d/neighbors v (get _ :points)))
  (vertex-valence
    [{points :points} v]
    (if-let [p (d/neighbors v points)]
      (if (= (first points) (peek points))
        2
        (if (or (= p (first points)) (= p (peek points)))
          1 2))
      0))

  g/IIntersect
  (intersect-line [_ l])

  m/IMagnitude
  (mag
    [_] (d/reduce-pairs + g/dist (get _ :points)))
  (mag-squared
    [_] (d/reduce-pairs + g/dist-squared (get _ :points)))

  g/IReflect
  (reflect
    [_ r]
    (LineStrip2. (mapv #(g/reflect % r) (get _ :points))))

  g/ISample
  (point-at
    [_ t] (gu/point-at t (get _ :points) nil))
  (random-point
    [_] (gu/point-at (m/random) (get _ :points) nil))
  (random-point-inside
    [_] (g/random-point _))
  (sample-uniform
    [_ udist include-last?]
    (gu/sample-uniform udist include-last? (get _ :points)))

  g/IRotate
  (rotate
    [_ theta] (LineStrip2. (mapv #(g/rotate % theta) (get _ :points))))

  g/IScale
  (scale
    [_ s] (LineStrip2. (mapv #(m/* % s) (get _ :points))))
  (scale-size
    [_ s] (LineStrip2. (gu/scale-size s (get _ :points))))

  g/ITranslate
  (translate
    [_ t] (LineStrip2. (mapv #(m/+ % t) (get _ :points))))

  g/ITransform
  (transform
    [_ m] (LineStrip2. (mapv #(g/transform-vector m %) (get _ :points))))

  g/IVolume
  (volume [_] 0.0))

(extend-type LineStrip3

  g/IArea
  (area [_] 0.0)

  g/IBoundary
  (contains-point? [_ a])

  g/IBounds
  (bounds [_] (gu/bounding-box (get _ :points)))
  (width [_] (gu/axis-range 0 (get _ :points)))
  (height [_] (gu/axis-range 1 (get _ :points)))
  (depth [_] (gu/axis-range 2 (get _ :points)))

  g/IBoundingSphere
  (bounding-sphere
    [_] (gu/bounding-sphere (g/centroid _) (get _ :points)))

  g/ICenter
  (center
    ([_] (LineStrip3. (gu/center (vec3) (get _ :points))))
    ([_ o] (LineStrip3. (gu/center (g/centroid _) (vec3 o) (get _ :points)))))
  (centroid
    [_] (gu/centroid (get _ :points)))

  g/ICircumference
  (circumference
    [_] (m/mag _))

  g/IClassify
  (classify-point [_ v])

  g/IExtrude
  (extrude
    [{:keys [points] :as _}
     {:keys [mesh depth offset scale attribs]
      :or {depth 1.0 scale 1.0} :as opts}]
    (let [tpoints (if (= 1.0 scale) points (get (g/scale-size _ scale) :points))
          off     (or offset (vec3 0 0 depth))
          top     (mapv #(m/+ off %) tpoints)]
      (->> (interleave
            (d/successive-nth 2 points)
            (d/successive-nth 2 top))
           (partition 2)
           (map-indexed
            (fn [i [[a1 a2] [b1 b2]]]
              (attr/generate-face-attribs [a1 b1 b2 a2] i attribs opts)))
           (g/into (or mesh (bm/basic-mesh))))))

  g/IVertexAccess
  (vertices
    ([_] (get _ :points))
    ([{points :points} res]
     (map #(gu/point-at % points) (m/norm-range res)))) ;; TODO transduce

  g/IEdgeAccess
  (edges
    ([_] (d/successive-nth 2 (get _ :points)))
    ([_ res] (d/successive-nth 2 (g/vertices _ res))))

  g/IGraph
  (vertex-neighbors
    [_ v] (d/neighbors v (get _ :points)))
  (vertex-valence
    [{points :points} v]
    (if-let [p (d/neighbors v points)]
      (if (= (first points) (peek points))
        2
        (if (or (= p (first points)) (= p (peek points)))
          1 2))
      0))

  g/IIntersect
  (intersect-line [_ l])

  m/IMagnitude
  (mag
    [_] (d/reduce-pairs + g/dist (get _ :points)))
  (mag-squared
    [_] (d/reduce-pairs + g/dist-squared (get _ :points)))

  g/IReflect
  (reflect
    [_ r]
    (LineStrip3. (mapv #(g/reflect % r) (get _ :points))))

  g/ISample
  (point-at
    [_ t] (gu/point-at t (get _ :points) nil))
  (random-point
    [_] (gu/point-at (m/random) (get _ :points) nil))
  (random-point-inside
    [_] (g/random-point _))
  (sample-uniform
    [_ udist include-last?]
    (gu/sample-uniform udist include-last? (get _ :points)))

  g/IRotate
  (rotate
    [_ theta] (LineStrip3. (mapv #(g/rotate % theta) (get _ :points))))

  g/IRotate3D
  (rotate-x
    [_ theta] (LineStrip3. (mapv #(g/rotate-x % theta) (get _ :points))))
  (rotate-y
    [_ theta] (LineStrip3. (mapv #(g/rotate-y % theta) (get _ :points))))
  (rotate-z
    [_ theta] (LineStrip3. (mapv #(g/rotate-z % theta) (get _ :points))))
  (rotate-around-axis
    [_ axis theta]
    (LineStrip3.
     (mapv #(g/rotate-around-axis % axis theta) (get _ :points))))

  g/IScale
  (scale
    [_ s] (LineStrip3. (mapv #(m/* % s) (get _ :points))))
  (scale-size
    [_ s] (LineStrip3. (gu/scale-size s (get _ :points))))

  g/ITranslate
  (translate
    [_ t] (LineStrip3. (mapv #(m/+ % t) (get _ :points))))

  g/ITransform
  (transform
    [_ m] (LineStrip3. (mapv #(g/transform-vector m %) (get _ :points))))

  g/IVolume
  (volume [_] 0.0))
