(ns thi.ng.geom.rect
  #?(:cljs (:require-macros [thi.ng.math.macros :as mm]))
  (:require
   [thi.ng.geom.core :as g]
   [thi.ng.geom.utils :as gu]
   [thi.ng.geom.utils.intersect :as isec]
   [thi.ng.geom.vector :as v :refer [vec2 vec3]]
   [thi.ng.geom.aabb :as a]
   [thi.ng.geom.attribs :as attr]
   [thi.ng.geom.basicmesh :as bm]
   #?(:clj [thi.ng.geom.types] :cljs [thi.ng.geom.types :refer [AABB Circle2 Line2 Polygon2 Rect2 Triangle2]])
   [thi.ng.dstruct.core :as d]
   [thi.ng.xerror.core :as err]
   [thi.ng.math.core :as m :refer [*eps*]]
   #?(:clj [thi.ng.math.macros :as mm]))
  #?(:clj
     (:import
      [thi.ng.geom.types AABB Circle2 Line2 Polygon2 Rect2 Triangle2])))

(defn rect
  ([] (Rect2. (vec2) (vec2 1.0)))
  ([w] (Rect2. (vec2) (vec2 w)))
  ([p q]
   (if (coll? p)
     (if (coll? q)
       (let [p (vec2 p) q (vec2 q)
             [p q] [(m/min p q) (m/max p q)]]
         (Rect2. p (m/- q p)))
       (Rect2. (vec2 p) (vec2 q)))
     (Rect2. (vec2) (vec2 p q))))
  ([x y w]
   (if (number? x)
     (Rect2. (vec2 x y) (vec2 w))
     (Rect2. (vec2 x) (vec2 y w))))
  ([x y w h] (Rect2. (vec2 x y) (vec2 w h))))

(defn union
  [{p :p [w h] :size} {q :p [qw qh] :size}]
  (let [[x1 y1 :as p'] (m/min p q)
        x2 (max (+ (p 0) w) (+ (q 0) qw))
        y2 (max (+ (p 1) h) (+ (q 1) qh))
        w (- x2 x1)
        h (- y2 y1)]
    (Rect2. p' (vec2 w h))))

(defn left [r] (v/x (get r :p)))

(defn right [r] (+ (v/x (get r :p)) (v/x (get r :size))))

(defn bottom [r] (v/y (get r :p)))

(defn bottom-left [r] (get r :p))

(defn top [r] (+ (v/y (get r :p)) (v/y (get r :size))))

(defn top-right [r] (m/+ (get r :p) (get r :size)))

(extend-type Rect2

  g/IArea
  (area [_] (reduce * (get _ :size)))

  g/IBoundary
  (contains-point?
    [{[px py] :p [w h] :size} [x y]]
    (and (m/in-range? 0.0 w (- x px)) (m/in-range? 0.0 h (- y py))))

  g/IBounds
  (bounds[_] _)
  (width  [_] ((get _ :size) 0))
  (height [_] ((get _ :size) 1))
  (depth  [_] 0)

  g/IBoundingCircle
  (bounding-circle
    [_]
    (let [c (g/centroid _)]
      (gu/bounding-circle c (g/dist c (get _ :p)))))

  g/ICenter
  (center
    ([{s :size}]
     (Rect2. (m/* s -0.5) s))
    ([{s :size} o]
     (Rect2. (m/madd s -0.5 o) s)))
  (centroid
    [_] (m/madd (get _ :size) 0.5 (get _ :p)))

  g/ICircumference
  (circumference [_] (* 2.0 (reduce + (get _ :size))))

  g/IClassify
  (classify-point
    [_ q]
    (transduce
     (map #(m/signum (apply gu/closest-point-coeff q %) *eps*))
     min
     (g/edges _)))

  ;; Extruding a rectangle along the Z-axis results in a 3D mesh
  ;; implementation. The `extrude` function supports the following
  ;; options, given as parameter map:
  ;;
  ;; | Key       | Description                             | Default |
  ;; |-----------+-----------------------------------------+---------|
  ;; | `:depth`  | extrusion depth along positive Z        |     1.0 |
  ;; | `:scale`  | scale factor of rect at extrusion depth |     1.0 |
  ;; | `:offset` | extrusion vector (overrides :depth)     |     nil |
  ;;
  ;; The process results in bevelled forms for `:scale` values other
  ;; than 1.0.
  ;;
  ;; *Note:* Due to Clojure's non-circular namespace dependency
  ;; policy, users must manually `require` the following namespaces
  ;; before using this method for rects:
  ;;
  ;; - `thi.ng.geom.aabb`
  ;; - `thi.ng.geom.polygon`

  g/IExtrude
  (extrude
    [_ {:keys [depth scale offset] :or {depth 1.0 scale 1.0} :as opts}]
    (if (and (== scale 1.0) (nil? offset))
      (g/as-mesh
       (AABB. (vec3 (get _ :p)) (vec3 (get _ :size) depth)) opts)
      (g/extrude (g/as-polygon _) opts)))
  (extrude-shell
    [_ opts] (g/extrude-shell (g/as-polygon _) opts))

  g/IVertexAccess
  (vertices
    [_]
    (let [a (get _ :p)
          c (m/+ a (get _ :size))]
      [a (vec2 (c 0) (a 1)) c (vec2 (a 0) (c 1))]))

  g/IEdgeAccess
  (edges
    [_] (let [a (get _ :p)
              c (m/+ a (get _ :size))
              b (vec2 (c 0) (a 1))
              d (vec2 (a 0) (c 1))]
          [[a b] [b c] [c d] [d a]]))

  g/IGraph
  (vertex-neighbors
    [_ v] (d/neighbors (vec2 v) (g/vertices _)))
  (vertex-valence
    [_ v] (if ((set (g/vertices _)) (vec2 v)) 2 0))

  g/IIntersect
  (intersect-line
    [_ {[p q] :points}]
    (if (and (g/contains-point? _ p) (g/contains-point? _ q))
      {:type :coincident}
      (isec/intersect-line2-edges? p q (g/edges _))))
  (intersect-ray
    ([_ ray]
     (let [[p dir] (if (map? ray) [(get ray :p) (get ray :dir)] ray)]
       (isec/intersect-ray2-edges? p dir (g/edges _))))
    ([_ p dir]
     (isec/intersect-ray2-edges? p dir (g/edges _))))
  (intersect-shape
    [_ s]
    (cond
      (instance? Line2 s) (g/intersect-line _ s)
      (instance? Rect2 s) (isec/intersect-rect-rect? _ s)
      (instance? Circle2 s) (isec/intersect-aabb-sphere? _ s)
      :default (err/unsupported! (str "can't intersect w/ " s))))

  g/IMeshConvert
  (as-mesh
    ([_] (g/as-mesh _ {}))
    ([_ opts]
     (g/add-face
      (or (get opts :mesh) (bm/basic-mesh))
      (attr/generate-face-attribs
       (mapv vec3 (g/vertices _)) 0 (get opts :attribs)
       (merge {:uv attr/uv-default-rect} opts)))))

  ;; - `map-point` takes a rect and 2D point, returns the point in
  ;;   normalized UV coords in local rect space, where [0,0] is
  ;;   equivalent to the rect's min point and [1,1] the point
  ;;   diagonally opposite. If any of the resulting coordinates is
  ;;   outside the 0.0 .. 1.0 interval, the orginal point was outside
  ;;   the rectangle.
  ;;
  ;; - `unmap-point` is the inverse operation of `map-point`. It maps
  ;;   a point in local UV coordinates to its world position.

  g/IPointMap
  (map-point
    [{:keys [p size]} q]
    (vec2
     (mm/subdiv (v/x q) (v/x p) (v/x size))
     (mm/subdiv (v/y q) (v/y p) (v/y size))))
  (unmap-point
    [_ q] (m/madd q (get _ :size) (get _ :p)))

  g/IPolygonConvert
  (as-polygon
    [_] (Polygon2. (g/vertices _)))

  g/IProximity
  (closest-point
    [_ p]
    (first (gu/closest-point-on-segments p (g/edges _))))

  g/ISample
  (point-at
    [_ t] (gu/point-at t (conj (g/vertices _) (get _ :p))))
  (random-point
    [_] (g/point-at _ (m/random)))
  (random-point-inside
    [_] (m/+ (get _ :p) (m/random ((get _ :size) 0)) (m/random ((get _ :size) 1))))
  (sample-uniform
    [_ udist include-last?]
    (gu/sample-uniform udist include-last? (conj (g/vertices _) (get _ :p))))

  ;; TODO same as for `ISetOps` impl for AABB: consider only impls for
  ;; ops retaining type
  ;;
  ;; The implementations for this protocol are only intended to work
  ;; with other rects and no type checking is done.
  ;;
  ;; - `union` returns the union rect of both args
  ;; - `intersection` (still missing) returns the rect of the intersection
  ;;   area of both args or `nil` if there's no overlap. Intersection is
  ;;   defined also for touching rects or if overlap is only a single point
  ;;   (in which case the size of the resulting rect will be zero)
  ;; - `difference` (still missing) will return a polygon of the remaining
  ;;   area after the 2nd rect has been subtracted from the first (or
  ;;   `nil` in case of no overlap)

  m/ISetOps
  (union
    [{pa :p sa :size} {pb :p sb :size}]
    (let [p (m/min pa pb)]
      (Rect2. p (m/- (m/max (m/+ pa sa) (m/+ pb sb)) p))))
  (intersection
    [_ r]
    (let [pa (get _ :p) qa (m/+ pa (get _ :size))
          pb (get r :p) qb (m/+ pb (get r :size))
          p' (m/max pa pb)
          q' (m/min qa qb)
          s  (m/- q' p')]
      (if (every? #(>= % 0) s)
        (Rect2. p' s))))

  ;; A rectangle can be subdivided into smaller ones, i.e. to create a
  ;; list of uniform grid cells. The following options can be given as
  ;; a 2nd argument map:
  ;;
  ;; | Key     | Description                                                    | Default |
  ;; |---------+----------------------------------------------------------------+---------|
  ;; | `:num`  | number of cols/rows the rect will be uniformly subdivided into |       2 |
  ;; | `:cols` | number of times the rect will be subdivided along the X-axis   |       2 |
  ;; | `:rows` | number of times the rect will be subdivided along the Y-axis   |       2 |
  ;;
  ;; When `:num` is given, the resulting rects will retain the aspect
  ;; ratio of the original rect. If specified, `:cols` and `:rows`
  ;; will take precedence over `:num`, but the latter will be used as
  ;; default for missing args. Rects are returned as a lazyseq with
  ;; top-left to bottom-right ordering and inner sorting along X.

  g/ISubdivide
  (subdivide
    ([_] (g/subdivide _ {}))
    ([_ {:keys [num cols rows] :or {num 2}}]
     (let [dx (/ 1.0 (or cols num))
           dy (/ 1.0 (or rows num))
           rx (range 0.0 1.0 dx)
           ry (range 0.0 1.0 dy)
           s (m/* (get _ :size) dx dy)]
       (for [y ry x rx
             :let [[px py] (g/unmap-point _ (vec2 x y))
                   px (m/roundto px *eps*)
                   py (m/roundto py *eps*)]]
         (Rect2. (vec2 px py) s)))))

  ;; A rectangle can be tessellated into a number of triangles. When
  ;; called without options map as 2nd argument, the rect will be
  ;; split into 2 triangles, each in anti-clockwise orientation: `[tl
  ;; br tr]` & `[tl bl br]`. When called *with* options, then these
  ;; are used for a call to `subdivide` first and the function returns
  ;; a lazyseq of triangles of the subdivided rects.

  g/ITessellate
  (tessellate
    ([_]
     (let [[a b c d] (g/vertices _)]
       [(Triangle2. [a b c])
        (Triangle2. [a c d])]))
    ([_ {tess-fn :fn :or {tess-fn gu/tessellate-3} :as opts}]
     (->> (g/subdivide _ opts)
          (sequence
           (comp
            (mapcat #(tess-fn (g/vertices %)))
            (map #(Triangle2. %)))))))

  g/IRotate
  (rotate
    [_ m] (g/rotate (g/as-polygon _) m))

  g/IScale
  (scale
    [_ s] (Rect2. (m/* (get _ :p) s) (m/* (get _ :size) s)))
  (scale-size
    [_ s]
    (let [s' (m/* (get _ :size) s)]
      (Rect2.
       (m/madd s' -0.5 (g/centroid _)) s')))

  g/ITranslate
  (translate
    [_ t] (Rect2. (m/+ (get _ :p) t) (get _ :size)))

  g/ITransform
  (transform
    [_ m] (g/transform (g/as-polygon _) m))

  g/IVolume
  (volume [_] 0.0))
