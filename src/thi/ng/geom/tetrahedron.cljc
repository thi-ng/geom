(ns thi.ng.geom.tetrahedron
  #?(:cljs (:require-macros [thi.ng.math.macros :as mm]))
  (:require
   [thi.ng.geom.core :as g]
   [thi.ng.geom.utils :as gu]
   [thi.ng.geom.utils.intersect :as isec]
   [thi.ng.geom.vector :as v :refer [vec3]]
   [thi.ng.geom.triangle :as t]
   [thi.ng.geom.basicmesh :as bm]
   #?(:clj [thi.ng.geom.types] :cljs [thi.ng.geom.types :refer [Tetrahedron]])
   [thi.ng.dstruct.core :as d]
   [thi.ng.xerror.core :as err]
   [thi.ng.math.core :as m :refer [PI HALF_PI THIRD SQRT3 *eps*]]
   #?(:clj [thi.ng.math.macros :as mm]))
   #?(:clj (:import [thi.ng.geom.types Tetrahedron])))

(defn orient-tetra
  "Takes a seq of 4 3D points, returns them as vector in the order so
  that the last point is on the opposite side of the plane defined by
  the first three points."
  [[a b c d :as t]]
  (let [dp (-> d (m/- a) (m/normalize) (m/dot (gu/ortho-normal a b c)))]
    (if (neg? dp) [a b c d] [a c b d])))

(defn tetrahedron
  ([points]
   (Tetrahedron.
    (orient-tetra (mapv vec3 points))))
  ([a b c d] (tetrahedron [a b c d])))

(extend-type Tetrahedron

  g/IArea
  (area
    [_] (transduce (map #(m/abs* (apply gu/tri-area3 %))) + (g/faces _)))

  ;; TODO use classify-point on all faces to check for containment
  ;; (must be neg for all)

  g/IClassify
  (classify-point [_ p] nil) ; TODO

  g/IProximity
  (closest-point [_ p] nil) ; TODO

  g/IBoundary
  (contains-point? [_ p] nil) ; TODO

  g/IBounds
  (bounds [_] (gu/bounding-box (g/vertices _)))
  (width [_] (gu/axis-range 0 (g/vertices _)))
  (height [_] (gu/axis-range 1 (g/vertices _)))
  (depth [_] (gu/axis-range 2 (g/vertices _)))

  g/IBoundingSphere
  (bounding-sphere
    [_] (gu/bounding-sphere (g/centroid _) (g/vertices _)))

  g/ICenter
  (center
    ([_] (Tetrahedron. (gu/center v/V3 (get _ :points))))
    ([_ o] (Tetrahedron. (gu/center o (get _ :points)))))
  (centroid [_] (gu/centroid (get _ :points)))

  g/IFlip
  (flip
    [{[a b c d] :points}] (Tetrahedron. [b a c d]))

  g/IVertexAccess
  (vertices
    [_] (get _ :points))

  g/IEdgeAccess
  (edges
    [{[a b c d] :points}]
    [[a b] [b c] [c a] [a d] [b d] [c d]])

  g/IFaceAccess
  (faces
    ([t _] (g/faces t))
    ([{[a b c d] :points}]
     [[[a b c]] [[a d b]] [[b d c]] [[c d a]]]))

  g/IGraph
  (vertex-neighbors
    [{[a b c d] :points} v]
    (condp = v
      a [c b d]
      b [a c d]
      c [b a d]
      d [a b c]
      nil))
  (vertex-valence
    [_ v] (if ((set (get _ :points)) v) 3 0))

  g/IIntersect
  (intersect-shape
    [_ s]
    (cond
      (instance? Tetrahedron s)
      (isec/intersect-tetrahedra?
       (orient-tetra (g/vertices _)) (orient-tetra (g/vertices s)))
      (and (sequential? s) (= 4 (count s)))
      (isec/intersect-tetrahedra? (g/vertices _) (orient-tetra s))
      :else (err/type-error! "Tetrahedron" s)))
  (intersect-line
    [_ l] (err/unsupported!))
  (intersect-ray
    [_ {p :p dir :dir}] (err/unsupported!))

  g/IMeshConvert
  (as-mesh
    ([_] (g/as-mesh _ {}))
    ([_ opts]
     (let [[a b c d] (orient-tetra (get _ :points))]
       (g/into (or (get opts :mesh) (bm/basic-mesh)) (g/faces _)))))

  g/ISample
  (point-at [_ t] nil) ; TODO
  (random-point
    [_] (g/point-at _ (m/random)))
  (random-point-inside
    [_] (gu/from-barycentric (g/vertices _) (m/normdist-weights 4)))

  g/ISlice
  (slice-with
    ([_ e] nil)
    ([_ e classifier] nil))

  g/ISubdivide
  (subdivide
    [_]
    (let [cp (gu/centroid (get _ :points))]
      (map #(tetrahedron (conj % cp)) (g/faces _))))

  g/ITessellate
  (tessellate
    [_] (g/faces _))

  g/IRotate
  (rotate [_ theta] (g/rotate-z _ theta))

  g/IRotate3D
  (rotate-x
    [_ theta] (Tetrahedron. (mapv #(g/rotate-x % theta) (get _ :points))))
  (rotate-y
    [_ theta] (Tetrahedron. (mapv #(g/rotate-y % theta) (get _ :points))))
  (rotate-z
    [_ theta] (Tetrahedron. (mapv #(g/rotate-z % theta) (get _ :points))))
  (rotate-around-axis
    [_ axis theta]
    (Tetrahedron.
     (mapv #(g/rotate-around-axis % axis theta) (get _ :points))))

  g/IScale
  (scale
    [_ s] (Tetrahedron. (mapv #(m/* % s) (get _ :points))))
  (scale-size
    [_ s] (Tetrahedron. (gu/scale-size s (get _ :points))))

  g/ITranslate
  (translate
    [_ t] (Tetrahedron. (mapv #(m/+ % t) (get _ :points))))

  g/ITransform
  (transform
    [_ m] (Tetrahedron. (mapv #(g/transform-vector m %) (get _ :points))))

  ;; http://mathcentral.uregina.ca/QQ/database/QQ.09.03/peter2.html

  g/IVolume
  (volume
    [{[a b c d] :points}]
    (let [n (gu/ortho-normal a b c)]
      (Math/abs (* (* m/THIRD (gu/tri-area3 a b c)) (- (m/dot n d) (m/dot n a)))))))
