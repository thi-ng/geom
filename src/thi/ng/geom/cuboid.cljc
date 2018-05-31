(ns thi.ng.geom.cuboid
  #?(:cljs
     (:require-macros
      [thi.ng.math.macros :as mm]))
  (:require
   [thi.ng.geom.core :as g]
   [thi.ng.geom.utils :as gu]
   [thi.ng.geom.vector :as v :refer [vec3]]
   [thi.ng.geom.attribs :as attr]
   [thi.ng.geom.basicmesh :as bm]
   [thi.ng.geom.plane :as p]
   [thi.ng.geom.tetrahedron :as tetra]
   #?(:clj [thi.ng.geom.types] :cljs [thi.ng.geom.types :refer [Cuboid]])
   [thi.ng.dstruct.core :as d]
   [thi.ng.xerror.core :as err]
   [thi.ng.math.core :as m :refer [*eps*]]
   #?(:clj [thi.ng.math.macros :as mm]))
  #?(:clj (:import [thi.ng.geom.types Cuboid])))

;; Unlike an axis-aligned bounding box (AABB), this type specifies a
;; freeform box/cuboid, defined by its 8 vertices and which can be
;; freely rotated & transformed. An AABB can be considered a logical
;; subtype of this type and an AABB can be converted into a cuboid via
;; the `cuboid` constructor function defined in this namespace.

(defn cuboid
  ([] (cuboid 1.0))
  ([a b c d e f g h]
   (Cuboid. (mapv vec3 [a b c d e f g h])))
  ([x]
   (cond
     (number? x)     (cuboid (vec3) x)
     (map? x)        (cuboid (get x :p) (get x :size))
     (sequential? x) (if (== 8 (count x))
                       (Cuboid. (mapv vec3 x))
                       (err/illegal-arg!
                        "Wrong number of points, expected 8 but got"
                        (count x)))
     :else           (err/illegal-arg! x)))
  ([o size]
   (let [[x1 y1 z1 :as a] (vec3 o)
         [x2 y2 z2 :as g] (m/+ a size)
         b (vec3 x1 y1 z2) c (vec3 x2 y1 z2)
         d (vec3 x2 y1 z1) e (vec3 x1 y2 z1)
         f (vec3 x1 y2 z2) h (vec3 x2 y2 z1)]
     (Cuboid. [a b c d e f g h]))))

(extend-type Cuboid

  g/IArea
  (area
    [_] (transduce (map #(apply gu/tri-area3 %)) + (g/tessellate _)))

  g/IBounds
  (bounds [_] (gu/bounding-box (get _ :points)))
  (width [_] (gu/axis-range 0 (get _ :points)))
  (height [_] (gu/axis-range 1 (get _ :points)))
  (depth [_] (gu/axis-range 2 (get _ :points)))

  g/IBoundingSphere
  (bounding-sphere
    [_] (gu/bounding-sphere (g/centroid _) (get _ :points)))

  g/IBoundary
  (contains-point?
    [_ p] (>= (g/classify-point _ p) 0))

  g/ICenter
  (center
    ([_] (Cuboid. (gu/center (vec3) (get _ :points))))
    ([_ o] (Cuboid. (gu/center (g/centroid _) (vec3 o) (get _ :points)))))
  (centroid
    [_] (gu/centroid (get _ :points)))

  ;; In order to determine if a given point lies within or outside the
  ;; cuboid, we compute the classifier for each of the 6 sides /
  ;; facets / plane and use their maximum as compound result. Since
  ;; the faces are all pointing outwards (away from the center),
  ;; `classify-point` for each face will return a positive value if
  ;; the point is in the positive half space of that plane. This
  ;; means, in order for a point to be inside the cuboid, all plane
  ;; classifiers need to be negative (or zero if the point is on the
  ;; surface. The search is terminated early as soon as one facet
  ;; produces a positive classifier.
  ;;
  ;; *Note:* Because of its reliance on plane normals, this
  ;; implementation only works for fully planar cuboids.

  g/IClassify
  (classify-point
    [_ p]
    (- (reduce
        (fn [c f]
          (let [c' (g/classify-point (p/plane-from-points f) p)]
            (if (pos? c') (reduced c') (max c c'))))
        -1.0 (g/faces _))))

  ;; e +----+ h
  ;;   |\   :\
  ;;   |f+----+ g
  ;;   | |  : |
  ;; a +-|--+d|
  ;;    \|   \|
  ;;   b +----+ c
  ;;
  ;; Faces are always returned in this order: east, west, north,
  ;; south, front, back - assuming the vertex order given in the
  ;; diagram.

  g/IVertexAccess
  (vertices
    [_] (get _ :points))

  g/IEdgeAccess
  (edges
    [_]
    (let [[a b c d e f g h] (get _ :points)]
      [[a b] [b c] [c d] [d a] ;; bottom
       [e f] [f g] [g h] [h e] ;; top
       [a e] [b f]             ;; left
       [c g] [d h]             ;; right
       ]))

  g/IFaceAccess
  (faces
    [_]
    (let [[a b c d e f g h] (get _ :points)]
      [[c d h g]
       [a b f e]
       [f g h e]
       [a d c b]
       [b c g f]
       [d a e h]]))

  g/IIntersect
  (intersect-shape
    [_ s] (err/unsupported!))

  g/IMeshConvert
  (as-mesh
    ([_] (g/as-mesh _ {}))
    ([_ {:keys [mesh flags attribs] :or {flags "nsewfb"}}]
     (let [[a b c d e f g h] (g/vertices _)
           [north south east west front back] (d/demunge-flags-seq flags "nsewfb")]
       (->> [(if east (attr/generate-face-attribs [c d h g] 0 attribs nil))
             (if west (attr/generate-face-attribs [a b f e] 1 attribs nil))
             (if north (attr/generate-face-attribs [f g h e] 2 attribs nil))
             (if south (attr/generate-face-attribs [a d c b] 3 attribs nil))
             (if front (attr/generate-face-attribs [b c g f] 4 attribs nil))
             (if back (attr/generate-face-attribs [d a e h] 5 attribs nil))]
            (sequence (filter identity))
            (g/into (or mesh (bm/basic-mesh)))))))

  ;; - `map-point` still missing
  ;; - `unmap-point` takes a seq of 8 points defining a cuboid and vector
  ;;   of normalized UVW coordinates. Applies trilinear interpolation to
  ;;   compute point within the cuboid:
  ;;   - U is coord along AD/BC edge
  ;;   - V along AE/BF
  ;;   - W along AB/DC
  ;;
  ;; Also see: `map-trilinear`

  g/IPointMap
  (map-point
    [_ p] (err/unsupported!))
  (unmap-point
    [_ p]
    (gu/map-trilinear (get _ :points) p))

  ;; FIXME either calc barycentric coords for quad or use triangles
  ;; (already implemented) Do not use, work in progress...

  g/IProximity
  (closest-point
    [_ p]
    (->> (g/faces _)
         (reduce
          (fn [acc f]
            (let [p' (g/closest-point (p/plane-from-points f) p)
                  d (g/dist-squared p p')]
              (if (< d (acc 1)) [p' d] acc)))
          [nil 1e+100])
         (first)))

  g/ISample
  (random-point-inside
    [{p :p size :size}]
    (let [[x1 y1 z1] p
          [x2 y2 z2] (m/+ p size)]
      (vec3 (m/random x1 x2) (m/random y1 y2) (m/random z1 z2))))
  (random-point
    [{p :p size :size}]
    (let [[x1 y1 z1] p
          [x2 y2 z2] (m/+ p size)
          id (int (m/random 6))]
      (cond
        (< id 2) (vec3 (if (zero? id) x1 x2) (m/random y1 y2) (m/random z1 z2))
        (< id 4) (vec3 (m/random x1 x2) (if (= 2 id) y1 y2) (m/random z1 z2))
        :else    (vec3 (m/random x1 x2) (m/random y1 y2) (if (= 4 id) z1 z2)))))

  ;; An Cuboid can be subdivided into smaller ones, i.e. to create a
  ;; seq of uniform grid cells. The following options can be given as
  ;; a 2nd argument map:
  ;;
  ;; | Key       | Description                                                           | Default |
  ;; |-----------+-----------------------------------------------------------------------+---------|
  ;; | `:num`    | number of cols/rows/slices the form will be uniformly subdivided into |       1 |
  ;; | `:cols`   | number of times the form will be subdivided along the X-axis          |       1 |
  ;; | `:rows`   | number of times the form will be subdivided along the Y-axis          |       1 |
  ;; | `:slices` | number of times the form will be subdivided along the Z-axis          |       1 |
  ;;
  ;; When `:num` is given, the resulting cuboids will retain the
  ;; aspect ratio of the original. If specified, `:cols`, `:rows` and
  ;; `:slices` will take precedence over `:num`, but the latter will
  ;; be used as default for missing args. Cuboids are returned as a
  ;; lazyseq starting from the first vertex of the original with inner
  ;; sorting over XYZ.

  g/ISubdivide
  (subdivide
    ([_] (g/subdivide _ {}))
    ([{points :points} {:keys [num cols rows slices] :or {num 1}}]
     (let [ru    (partition 2 1 (m/norm-range (or cols num)))
           rv    (partition 2 1 (m/norm-range (or rows num)))
           rw    (partition 2 1 (m/norm-range (or slices num)))
           map-p (fn [p]
                   (->> (gu/map-trilinear points p)
                        (mapv #(m/roundto % *eps*))
                        (vec3)))]
       (for [[w1 w2] rw, [v1 v2] rv, [u1 u2] ru]
         (Cuboid.
          (mapv map-p [[u1 v1 w1] [u1 v1 w2] [u2 v1 w2] [u2 v1 w1]
                       [u1 v2 w1] [u1 v2 w2] [u2 v2 w2] [u2 v2 w1]]))))))

  g/ITessellate
  (tessellate
    ([_] (g/tessellate _ {}))
    ([_ {f :fn :or {f gu/tessellate-3} :as opts}]
     (->> (if (some #{:num :cols :rows :slices} (keys opts))
            (g/subdivide _ opts)
            [_])
          (sequence
           (comp
            (mapcat g/faces)
            (mapcat f))))))

  g/IRotate
  (rotate
    [_ theta] (g/rotate-z _ theta))

  g/IRotate3D
  (rotate-x
    [_ theta] (Cuboid. (mapv #(g/rotate-x % theta) (get _ :points))))
  (rotate-y
    [_ theta] (Cuboid. (mapv #(g/rotate-y % theta) (get _ :points))))
  (rotate-z
    [_ theta] (Cuboid. (mapv #(g/rotate-z % theta) (get _ :points))))
  (rotate-around-axis
    [_ axis theta]
    (Cuboid.
     (mapv #(g/rotate-around-axis % axis theta) (get _ :points))))

  g/IScale
  (scale
    [_ s] (Cuboid. (mapv #(m/* % s) (get _ :points))))
  (scale-size
    [_ s] (Cuboid. (gu/scale-size s (get _ :points))))

  g/ITranslate
  (translate
    [_ t] (Cuboid. (mapv #(m/+ % t) (get _ :points))))

  g/ITransform
  (transform
    [_ m] (Cuboid. (mapv #(g/transform-vector m %) (get _ :points))))

  ;; https://www.math.ucdavis.edu/~deloera/CURRENT_INTERESTS/small.cubes.ps
  ;; http://www.ics.uci.edu/~eppstein/projects/tetra/

  g/IVolume
  (volume
    [_]
    (let [[a b c d e f g h] (get _ :points)]
      (transduce
       (map #(-> % tetra/tetrahedron g/volume m/abs*))
       +
       [[a b d e]
        [b c d g]
        [d e g h]
        [b d e g]
        [b f g e]]))))
