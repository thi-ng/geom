(ns thi.ng.geom.aabb
  #?(:cljs
     (:require-macros
      [thi.ng.math.macros :as mm]))
  (:require
   [thi.ng.geom.core :as g]
   [thi.ng.geom.utils :as gu]
   [thi.ng.geom.utils.intersect :as isec]
   [thi.ng.geom.vector :as v :refer [vec3]]
   [thi.ng.geom.matrix :refer [M44]]
   [thi.ng.geom.cuboid :as cu]
   [thi.ng.geom.basicmesh :as bm]
   [thi.ng.geom.attribs :as attr]
   [thi.ng.dstruct.core :as d]
   [thi.ng.xerror.core :as err]
   [thi.ng.math.core :as m :refer [*eps*]]
   #?(:clj [thi.ng.geom.types] :cljs [thi.ng.geom.types :refer [AABB Sphere]])
   #?(:clj [thi.ng.math.macros :as mm]))
   #?(:clj (:import [thi.ng.geom.types AABB Sphere])))

(defn aabb
  "Creates a new axis-aligned bounding box."
  ([] (AABB. (vec3 0.0) (vec3 1.0)))
  ([size] (AABB. (vec3 0.0) (vec3 size)))
  ([o size] (AABB. (vec3 o) (vec3 size)))
  ([sx sy sz] (AABB. (vec3 0.0) (vec3 sx sy sz))))

(defn aabb-from-minmax
  [p q]
  (let [p (vec3 p)
        q (vec3 q)
        p (m/min p q)]
    (AABB. p (m/- (m/max p q) p))))

(extend-type AABB

  g/IArea
  (area [{[w h d] :size}] (* 2.0 (mm/madd w h d h w d)))

  g/IBounds
  (bounds [_] _)
  (width  [_] (nth (get _ :size) 0))
  (height [_] (nth (get _ :size) 1))
  (depth  [_] (nth (get _ :size) 2))

  g/IBoundingSphere
  (bounding-sphere
    [_]
    (let [ext (m/* (get _ :size) 0.5)]
      (gu/bounding-sphere (m/+ (get _ :p) ext) (m/mag ext))))

  g/IBoundary
  (contains-point?
    [{[px py pz] :p s :size} q]
    (and (m/in-range? px (+ px (nth s 0)) (nth q 0))
         (m/in-range? py (+ py (nth s 1)) (nth q 1))
         (m/in-range? pz (+ pz (nth s 2)) (nth q 2))))
  g/ICenter
  (center
    ([{size :size}]
     (AABB. (m/* size -0.5) size))
    ([{size :size} q]
     (AABB. (m/madd size -0.5 q) size)))
  (centroid
    [_] (m/madd (get _ :size) 0.5 (get _ :p)))

  g/IClassify
  (classify-point
    [_ [x y z]]
    (let [[x1 y1 z1 :as p] (get _ :p)
          [x2 y2 z2] (m/+ p (get _ :size))
          on-plane? (fn [[minp maxp p min1 max1 min2 max2 c1 c2]]
                      (and (or (m/delta= minp p *eps*)
                               (m/delta= maxp p *eps*))
                           (m/in-range? min1 max1 c1)
                           (m/in-range? min2 max2 c2)))]
      (if (some on-plane?
                [[x1 x2 x y1 y2 z1 z2 y z]
                 [y1 y2 y x1 x2 z1 z2 x z]
                 [z1 z2 z x1 x2 y1 y2 x y]])
        0
        (if (and (m/in-range? x1 x2 x)
                 (m/in-range? y1 y2 y)
                 (m/in-range? z1 z2 z))
          1 -1))))

  ;; e +----+ h
  ;;   |\   :\
  ;;   |f+----+ g
  ;;   | |  : |
  ;; a +-|--+d|
  ;;    \|   \|
  ;;   b +----+ c
  ;;
  ;; Faces are always returned in this order: east, west, north, south,
  ;; front, back - assuming the vertex order given in the diagram.

  g/IVertexAccess
  (vertices
    [_]
    (let [[x1 y1 z1 :as a] (get _ :p)
          [x2 y2 z2 :as g] (m/+ a (get _ :size))]
      [a (vec3 x1 y1 z2) (vec3 x2 y1 z2) (vec3 x2 y1 z1)
       (vec3 x1 y2 z1) (vec3 x1 y2 z2) g (vec3 x2 y2 z1)]))

  g/IEdgeAccess
  (edges
    [_]
    (let [[a b c d e f g h] (g/vertices _)]
      [[a b] [b c] [c d] [d a] ;; bottom
       [e f] [f g] [g h] [h e] ;; top
       [a e] [b f]             ;; left
       [c g] [d h]             ;; right
       ]))

  g/IFaceAccess
  (faces
    [_]
    (let [[a b c d e f g h] (g/vertices _)]
      [[c d h g]
       [a b f e]
       [f g h e]
       [a d c b]
       [b c g f]
       [d a e h]]))

  g/IIntersect
  (intersect-shape
    [_ s]
    (cond
      (instance? AABB s)
      (isec/intersect-aabb-aabb? _ s)
      (instance? Sphere s)
      (isec/intersect-aabb-sphere? _ s)
      :else (err/type-error! "AABB" s)))

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
            (eduction (filter identity))
            (g/into (or mesh (bm/basic-mesh)))))))

  ;; `map-point` takes an AABB and 3D point, returns the point in normalized UVW
  ;; coords in local box space, where [0,0,0] is equivalent to the
  ;; AABB's min point and [1,1,1] the point diagonally opposite. If
  ;; any of the resulting coordinates is outside the 0.0 .. 1.0
  ;; interval, the orginal point was outside the box.
  ;;
  ;; `unmap-point` is the inverse operation of =map-point=. It maps a
  ;; point in local UVW coordinates to its world position.

  g/IPointMap
  (map-point
    [{:keys [p size]} q]
    (vec3
     (mm/subdiv (nth q 0) (nth p 0) (nth size 0))
     (mm/subdiv (nth q 1) (nth p 1) (nth size 1))
     (mm/subdiv (nth q 2) (nth p 2) (nth size 2))))
  (unmap-point
    [_ q] (m/madd q (get _ :size) (get _ :p)))

  g/IProximity
  (closest-point
    [{[px py pz] :p size :size} q]
    (vec3
     (m/clamp (nth q 0) px (+ px (nth size 0)))
     (m/clamp (nth q 1) py (+ py (nth size 1)))
     (m/clamp (nth q 2) pz (+ pz (nth size 2)))))

  g/ISample
  (random-point-inside
    [_]
    (let [[x1 y1 z1 :as p] (get _ :p)
          [x2 y2 z2]       (m/+ p (get _ :size))]
      (vec3 (m/random x1 x2) (m/random y1 y2) (m/random z1 z2))))
  (random-point
    [_]
    (let [[x1 y1 z1 :as p] (get _ :p)
          [x2 y2 z2]       (m/+ p (get _ :size))
          id               (int (m/random 6))]
      (cond
        (< id 2) (vec3 (if (zero? id) x1 x2) (m/random y1 y2) (m/random z1 z2))
        (< id 4) (vec3 (m/random x1 x2) (if (= 2 id) y1 y2) (m/random z1 z2))
        :else    (vec3 (m/random x1 x2) (m/random y1 y2) (if (= 4 id) z1 z2)))))

  ;; Maybe `difference` isn't suitable here and should only implement
  ;;       ops which retain AABB type. Difference already implemented in
  ;;       csg ns
  ;;
  ;; The implementations for this protocol are only intended to work with
  ;; other AABBs and no type checking is done.
  ;;
  ;; - `union` returns the union box of both args
  ;; - `intersection` returns the AABB of the intersection volume of both
  ;;   args or `nil` if there's no overlap. Intersection is defined also
  ;;   for touching boxes or if overlap is only a single point (in which
  ;;   case the size of the resulting AABB will be zero)
  ;; - `difference` (still missing) will return a mesh of the remaining
  ;;   volume after the 2nd box has been subtracted from the first (or
  ;;   `nil` in case of no overlap)

  m/ISetOps
  (union
    [_ b]
    (let [pa (get _ :p)
          pb (get b :p)
          p  (m/min pa pb)]
      (AABB.
       p (m/- (m/max (m/+ pa (get _ :size)) (m/+ pb (get b :size))) p))))
  (intersection
    [_ b]
    (let [pa (get _ :p)
          qa (m/+ pa (get _ :size))
          pb (get b :p)
          qb (m/+ pb (get b :size))
          p' (m/max pa pb)
          q' (m/min qa qb)
          s' (m/- q' p')]
      (if (every? #(>= % 0.0) s')
        (AABB. p' s'))))

  ;; An AABB can be subdivided into smaller ones, i.e. to create a seq of
  ;; uniform grid cells. The following options can be given as a 2nd
  ;; argument map:
  ;;
  ;; | Key       | Description                                                          | Default |
  ;; |-----------+----------------------------------------------------------------------+---------|
  ;; | `:num`    | number of cols/rows/slices the box will be uniformly subdivided into |       1 |
  ;; | `:cols`   | number of times the box will be subdivided along the X-axis          |       1 |
  ;; | `:rows`   | number of times the box will be subdivided along the Y-axis          |       1 |
  ;; | `:slices` | number of times the box will be subdivided along the Z-axis          |       1 |
  ;;
  ;; When `:num` is given, the resulting AABBs will retain the aspect ratio
  ;; of the original. If specified, `:cols`, `:rows` and `:slices` will
  ;; take precedence over `:num`, but the latter will be used as default
  ;; for missing args. AABBs are returned as a lazyseq starting from `:p`
  ;; of the original with inner sorting over XYZ.

  g/ISubdivide
  (subdivide
    ([_] (g/subdivide _ {}))
    ([_ {:keys [num cols rows slices] :or {num 1}}]
     (let [sx   (or cols num)
           sy   (or rows num)
           sz   (or slices num)
           p    (get _ :p)
           size (get _ :size)
           s    (m/div size sx sy sz)]
       (for [z (butlast (m/norm-range sz))
             y (butlast (m/norm-range sy))
             x (butlast (m/norm-range sx))]
         (AABB. (m/madd (vec3 x y z) size p) s)))))

  ;; TODO Only keep faces on the surface of the original box (no inside walls)
  ;; could use Quad3 face tessellation, but would require moving Q3's
  ;; subdivision into utils ns to avoid circular dependency.

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
    [_ theta] (g/rotate-z (cu/cuboid (get _ :p) (get _ :size)) theta))

  g/IRotate3D
  (rotate-x
    [_ theta] (g/rotate-x (cu/cuboid (get _ :p) (get _ :size)) theta))
  (rotate-y
    [_ theta] (g/rotate-y (cu/cuboid (get _ :p) (get _ :size)) theta))
  (rotate-z
    [_ theta] (g/rotate-z (cu/cuboid (get _ :p) (get _ :size)) theta))
  (rotate-around-axis
    [_ axis theta] (g/rotate-around-axis (cu/cuboid (get _ :p) (get _ :size)) axis theta))

  g/IScale
  (scale
    [_ s] (AABB. (m/* (get _ :p) s) (m/* (get _ :size) s)))
  (scale-size
    [_ s]
    (let [s' (m/* (get _ :size) s)]
      (AABB. (m/madd (m/- s' (get _ :size)) -0.5 (get _ :p)) s')))

  g/ITranslate
  (translate
    [_ t] (AABB. (m/+ (get _ :p) t) (get _ :size)))

  g/ITransform
  (transform
    [_ m] (g/transform (cu/cuboid (get _ :p) (get _ :size)) m))

  g/IVolume
  (volume [{[w h d] :size}] (mm/mul w h d)))
