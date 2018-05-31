(ns thi.ng.geom.quad
  (:require
   [thi.ng.geom.core :as g]
   [thi.ng.geom.utils :as gu]
   [thi.ng.geom.vector :as v :refer [vec2 vec3]]
   [thi.ng.geom.attribs :as attr]
   [thi.ng.geom.basicmesh :as bm]
   [thi.ng.geom.line :as l]
   [thi.ng.geom.triangle :as t]
   #?(:clj [thi.ng.geom.types] :cljs [thi.ng.geom.types :refer [Quad3 Triangle3 Line3]])
   [thi.ng.dstruct.core :as d]
   [thi.ng.xerror.core :as err]
   [thi.ng.math.core :as m :refer [*eps*]])
  #?(:clj (:import [thi.ng.geom.types Quad3 Triangle3 Line3])))

(defn quad3
  ([] (quad3 1.0))
  ([w] (cond
         (and (sequential? w) (= 4 (count w))) (Quad3. (mapv vec3 w))
         (number? w) (Quad3. [(vec3) (vec3 w 0.0 0.0) (vec3 w w 0.0) (vec3 0.0 w 0.0)])
         :default (err/illegal-arg! w)))
  ([a b c d] (Quad3. [(vec3 a) (vec3 b) (vec3 c) (vec3 d)])))

;; The following functions can be used to inset the edges of a convex
;; quad.

(defn inset-corner
  "Takes the end points of two lines and an inset vector for each line.
  Computes the shortest line segment between the two lines and returns
  the end point lying on AB (the first line given). Returns nil if there
  is no intersection (i.e. the given lines are parallel or zero length)."
  [a b c d n1 n2]
  (get (gu/closest-line-between (m/+ a n1) (m/+ b n1) (m/+ c n2) (m/+ d n2)) :a))

(defn inset-quad
  "Takes a vector of four points defining a convex quad and an inset
  distance (use negative value for offsetting). Returns vector of edge
  intersections (the new corner points)."
  [[a b c d] inset]
  (let [nu (m/normalize (m/- (m/mix b c) (m/mix a d)) inset)
        nv (m/normalize (m/- (m/mix d c) (m/mix a b)) inset)
        iu (m/- nu)
        iv (m/- nv)]
    [(inset-corner a d a b nu nv)
     (inset-corner b c a b iu nv)
     (inset-corner b c c d iu iv)
     (inset-corner a d c d nu iv)]))

(extend-type Quad3

  g/IArea
  (area
    [{[a b c d] :points}]
    (+ (gu/tri-area3 a b c) (gu/tri-area3 a c d)))

  g/IBoundary
  (contains-point?
    [_ p] )

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
    ([_] (Quad3. (gu/center (vec3) (get _ :points))))
    ([_ o] (Quad3. (gu/center (g/centroid _) (vec3 o) (get _ :points)))))
  (centroid [_] (gu/centroid (get _ :points)))

  g/ICircumference
  (circumference
    [{p :points}] (gu/arc-length (conj p (first p))))

  g/IClassify
  (classify-point
    [_ p]
    (transduce (map #(g/classify-point (Line3. %) p)) min (g/edges _)))

  ;; Extruding a quad along an axis vector results in a 3D quad mesh.
  ;; The `extrude` function supports the following options, given as
  ;; parameter map:
  ;;
  ;; | Key       | Description                                | Default |
  ;; |-----------+--------------------------------------------+---------|
  ;; | `:depth`  | extrusion depth/length in normal direction | 1.0     |
  ;; | `:scale`  | scale factor of rect at extrusion depth    | 1.0     |
  ;; | `:offset` | extrusion vector (overrides :depth)        | nil     |
  ;;
  ;; If `:depth` is used for extrusion, then the quad's normal is
  ;; computed from its `a`, `b` and `c` vertices only (in that order)
  ;; and the quad is assumed to be planar.
  ;;
  ;; The process results in bevelled forms for =:scale= values other
  ;; than 1.0.

  g/IExtrude
  (extrude
    [_ {:keys [mesh depth scale offset flags]
        :or {depth 1.0 scale 1.0 flags "nsewfb"}}]
    (let [[a b c d :as v] (g/vertices _)
          norm (gu/ortho-normal a b c)
          offset (or offset (m/* norm depth))
          dp (m/dot norm (m/normalize offset))
          [a2 b2 c2 d2] (if (== 1.0 scale)
                          (mapv #(m/+ offset %) v)
                          (->> (g/scale-size _ scale)
                               (g/vertices)
                               (mapv #(m/+ offset %))))
          [n s e w f b'] (d/demunge-flags-seq flags "nsewfb")]
      (->> [(if n [d2 c2 c d]) ;; TODO attribs
            (if s [b2 a2 a b])
            (if e [c2 b2 b c])
            (if w [a2 d2 d a])
            (if b' [a2 b2 c2 d2])
            (if f [d c b a])]
           (sequence ;; FIXME eduction
            (if (neg? dp)
              (comp
               (filter identity)
               (map rseq))
              (filter identity)))
           (g/into (or mesh (bm/basic-mesh))))))
  (extrude-shell
    [_ opts] nil)

  g/IFlip
  (flip
    [_] (Quad3. (reverse (get _ :points))))

  g/IVertexAccess
  (vertices
    [_] (get _ :points))

  g/IEdgeAccess
  (edges
    [{[a b c d] :points}] [[a b] [b c] [c d] [d a]])

  g/IGraph
  (vertex-neighbors
    [_ v] (d/neighbors (vec3 v) (get _ :points)))
  (vertex-valence
    [_ v] (if ((set (get _ :points)) v) 2 0))

  g/IInset
  (inset
    [_ inset] (Quad3. (inset-quad (get _ :points) inset)))

  g/IIntersect
  (intersect-line
    [_ l])
  (intersect-ray
    [_ r])
  (intersect-shape
    [_ s])

  g/IMeshConvert
  (as-mesh
    ([_] (g/as-mesh _ {}))
    ([_ opts]
     (g/add-face
      (or (get opts :mesh) (bm/basic-mesh))
      (attr/generate-face-attribs (get _ :points) 0 (get opts :attribs) opts))))

  ;; FIXME `map-point` currently broken, based on:
  ;; https://www.garagegames.com/community/forums/viewthread/76960/1#comment-543505

  g/IPointMap
  (map-point
    [{[a b c d] :points} p]
    (let [u1 (gu/closest-point-coeff p a b)
          u2 (gu/closest-point-coeff p d c)
          v1 (gu/closest-point-coeff p a d)
          v2 (gu/closest-point-coeff p b c)
          dab (g/dist p (m/mix a b u1))
          ddc (g/dist p (m/mix d c u2))
          dad (g/dist p (m/mix a d v1))
          dbc (g/dist p (m/mix b c v2))
          u* (+ (* u1 (/ dab (+ dab ddc)))
                (* u2 (/ ddc (+ dab ddc))))
          v* (+ (* v1 (/ dad (+ dad dbc)))
                (* v2 (/ dbc (+ dad dbc))))]
      (vec2 u* v*)))
  (unmap-point
    [_ p] (gu/map-bilinear (get _ :points) p))

  g/IProximity
  (closest-point
    [_ p]
    (first (gu/closest-point-on-segments p (g/edges _))))

  g/ISample
  (point-at
    [{p :points} t] (gu/point-at t (conj p (first p))))
  (random-point
    [{p :points}] (gu/point-at (m/random) (conj p (first p))))
  (random-point-inside
    [_] (gu/map-bilinear (get _ :points) (vec2 (m/random) (m/random))))
  (sample-uniform
    [{p :points} udist include-last?]
    (gu/sample-uniform udist include-last? (conj p (first p))))

  ;; A quad can be subdivided into smaller ones, i.e. to create a list
  ;; of grid cells. The following options can be given as a 2nd
  ;; parameter map:
  ;;
  ;; | Key     | Description                                                      | Default |
  ;; |---------+------------------------------------------------------------------+---------|
  ;; | `:num`  | number of cols/rows the rect will be uniformly subdivided into   |       2 |
  ;; | `:cols` | number of times the rect will be subdivided along the AB/CD edge |       2 |
  ;; | `:rows` | number of times the rect will be subdivided along the BC/DA edge |       2 |
  ;;
  ;; The resulting quads are *not* guaranteed to retain the aspect
  ;; ratio of the original quad (only kept if all of quad's inner
  ;; angles are 90 degrees). If specified, `:cols` and `:rows` will
  ;; take precedence over `:num`, but the latter will be used as
  ;; default for missing args.

  g/ISubdivide
  (subdivide
    ([_] (g/subdivide _ {}))
    ([{:keys [points]} {:keys [num cols rows] :or {num 2}}]
     (let [ru (d/successive-nth 2 (m/norm-range (or cols num)))
           rv (d/successive-nth 2 (m/norm-range (or rows num)))
           map-p (fn [p] (->> p (gu/map-bilinear points) (mapv #(m/roundto % *eps*)) vec3))]
       (for [[v1 v2] rv, [u1 u2] ru]
         (Quad3.
          [(map-p [u1 v1]) (map-p [u2 v1]) (map-p [u2 v2]) (map-p [u1 v2])])))))

  ;; A quad can be tessellated into a number of triangles. When called
  ;; without an options map as 2nd argument, the quad will be split
  ;; into 2 triangles: `[a b c]` & `[a c d]`. When called *with*
  ;; options, then these are used for a call to `subdivide` first and
  ;; the function returns a lazyseq of triangles of the subdivided
  ;; quad.

  g/ITessellate
  (tessellate
    ([{[a b c d] :points}]
     [(Triangle3. [a b c])
      (Triangle3. [a c d])])
    ([_ {tess-fn :fn :or {tess-fn gu/tessellate-3} :as opts}]
     (->> (g/subdivide _ opts)
          (sequence
           (comp
            (mapcat #(tess-fn (get % :points)))
            (map #(Triangle3. %)))))))

  g/IRotate
  (rotate
    [_ theta] (Quad3. (mapv #(g/rotate % theta) (get _ :points))))

  g/IRotate3D
  (rotate-x
    [_ theta] (Quad3. (mapv #(g/rotate-x % theta) (get _ :points))))
  (rotate-y
    [_ theta] (Quad3. (mapv #(g/rotate-y % theta) (get _ :points))))
  (rotate-z
    [_ theta] (Quad3. (mapv #(g/rotate-z % theta) (get _ :points))))
  (rotate-around-axis
    [_ axis theta]
    (Quad3.
     (mapv #(g/rotate-around-axis % axis theta) (get _ :points))))

  g/IScale
  (scale
    [_ s] (Quad3. (mapv #(m/* % s) (get _ :points))))
  (scale-size
    [_ s] (Quad3. (gu/scale-size s (get _ :points))))

  g/ITranslate
  (translate
    [_ t] (Quad3. (mapv #(m/+ % t) (get _ :points))))

  g/ITransform
  (transform
    [_ m] (Quad3. (mapv #(g/transform-vector m %) (get _ :points))))

  g/IVolume
  (volume [_] 0.0))
