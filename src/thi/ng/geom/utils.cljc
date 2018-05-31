(ns thi.ng.geom.utils
  #?(:cljs
     (:require-macros
      [thi.ng.math.macros :as mm]))
  (:require
   [thi.ng.geom.core :as g]
   [thi.ng.geom.vector :as v :refer [vec2 vec3 V2 V3]]
   #?(:clj [thi.ng.geom.types] :cljs [thi.ng.geom.types :refer [AABB Circle2 Rect2 Sphere]])
   [thi.ng.dstruct.core :as d]
   [thi.ng.math.core :as m :refer [*eps*]]
   #?(:clj [thi.ng.math.macros :as mm]))
   #?(:clj (:import [thi.ng.geom.types AABB Circle2 Rect2 Sphere])))

(declare tri-area3)

;; Point collection functions
;; Distance & length

(defn closest-point-coeff
  [p a b]
  (let [d (m/- b a)]
    (/ (m/dot (m/- p a) d) (m/mag-squared d))))

(defn closest-point-on-line
  [p a b]
  (m/mix a b (closest-point-coeff p a b)))

(defn closest-point-on-segment
  [p a b]
  (let [t (closest-point-coeff p a b)]
    (if (<= t 0.0) a (if (>= t 1.0) b (m/mix a b t)))))

(defn closest-point-on-segments
  [p segments]
  (transduce
   (map #(apply closest-point-on-segment p %))
   (completing
    (fn [a q]
      (let [d' (g/dist-squared p q)]
        (if (< d' (a 1)) [q d'] a))))
   [nil m/INF+]
   segments))

(defn closest-line-between
  [a1 b1 a2 b2]
  (let [p43 (m/- b2 a2)]
    (if-not (m/delta= V3 p43 *eps*)
      (let [p21 (m/- b1 a1)]
        (if-not (m/delta= V3 p21 *eps*)
          (let [p13   (m/- a1 a2)
                d1343 (m/dot p13 p43)
                d4321 (m/dot p43 p21)
                d1321 (m/dot p13 p21)
                d4343 (m/dot p43 p43)
                d2121 (m/dot p21 p21)
                d     (mm/msub d2121 d4343 d4321 d4321)]
            (if (m/delta= 0.0 d *eps*)
              {:type :parallel}
              (let [n  (mm/msub d1343 d4321 d1321 d4343)
                    ua (/ n d)
                    ub (/ (mm/madd d4321 ua d1343) d4343)]
                {:type :intersect
                 :a    (m/madd p21 ua a1)
                 :b    (m/madd p43 ub a2)
                 :ua   ua
                 :ub   ub}))))))))

(defn dist*
  [rf]
  (fn [c points]
    (->> points
         (transduce (map #(g/dist-squared c %)) rf 0)
         (Math/sqrt))))

(def min-dist (dist* min))
(def max-dist (dist* max))

(defn arc-length-index
  [points]
  (->> points
       (partition 2 1)
       (transduce
        (map #(g/dist (nth % 0) (nth % 1)))
        (completing (fn [a d] (conj a (+ (peek a) d))))
        [0])))

(defn arc-length
  [points]
  (d/reduce-pairs + g/dist points))

;; Centroid & centering

(defn centroid
  [coll]
  (case (count coll)
    0 nil
    1 (first coll)
    2 (m/mix (first coll) (nth coll 1))
    (let [s (/ 1.0 (count coll))
          f (fn [x _] (* x s))]
      (g/reduce-vector (first coll) + f (rest coll)))))

(defn center
  ([c' coll]
   (center (centroid coll) c' coll))
  ([c c' coll]
   (let [d (m/- c' c)] (mapv #(m/+ % d) coll))))

(defn scale-size
  ([s coll] (scale-size (centroid coll) s coll))
  ([c s coll] (mapv #(m/madd (m/- % c) s c) coll)))

;; Bounds

(defn bounds*
  [zero coll]
  (let [c (count coll)]
    (cond
      (> c 1) (let [[x & xs] coll
                    p (g/reduce-vector x min xs)
                    q (g/reduce-vector x max xs)]
                [p (m/- q p)])
      (== c 1) [(first coll) zero]
      :default nil)))

(defn bounding-rect-raw
  [points]
  (bounds* V2 points))

(defn bounding-box-raw
  [points]
  (bounds* V3 points))

(defn radial-bounds
  [ctor c r-or-points]
  [(ctor c)
   (if (coll? r-or-points)
     (max-dist c r-or-points)
     r-or-points)])

(defn axis-bounds
  [axis coll]
  (let [xs (mapv #(nth % axis) coll)]
    [(reduce min xs) (reduce max xs)]))

(defn axis-range
  [axis coll]
  (- (apply - (axis-bounds axis coll))))

(defn delta-contains
  [points p eps]
  (some #(m/delta= p % eps) points))

;; Sampling

(defn from-barycentric
  [points weights]
  (reduce m/+ (map m/* points weights)))

(defn point-at-index
  "Takes a seq of points and normalized index (0.0 .. 1.0), returns
  interpolated point at position t."
  [points t]
  (let [n  (dec (count points))
        t' (* t n)
        ti (int t')]
    (if (< ti n)
      (m/mix (nth points ti) (nth points (inc ti)) (- t' ti))
      (nth points (if (neg? ti) 0 n)))))

(defn point-at*
  [points idx total t i]
  (let [ct (* t total)
        i (int (loop [i i] (if (>= ct (idx i)) (recur (inc i)) i)))
        i1 (dec i)
        pi (idx i1)]
    [(m/mix (nth points i1) (nth points i)
            (mm/subdiv ct pi (idx i) pi))
     i]))

(defn point-at
  ([t points] (point-at t points nil))
  ([t points idx]
   (when (m/in-range? 0.0 1.0 t)
     (let [n (count points)
           t (double t)]
       (cond
         (== 0 n) nil
         (== 1 n) (first points)
         (== 1.0 t) (last points)
         :default
         (let [idx (or idx (arc-length-index points))]
           (first (point-at* points idx (peek idx) t 1))))))))

(defn sample-uniform*
  [udist include-last? points]
  (let [idx (arc-length-index points)
        total (peek idx)
        delta (/ udist total)
        samples (loop [t 0.0, i 1, acc []]
                  (if (< t 1.0)
                    (let [[p i] (point-at* points idx total t i)]
                      (recur (+ t delta) (int i) (conj acc p)))
                    acc))]
    (if include-last?
      (conj samples (last points))
      samples)))

(defn sample-uniform
  [udist include-last? points]
  (let [n (count points)]
    (cond
      (== 0 n) nil
      (== 1 n) [(first points)]
      :default (sample-uniform* udist include-last? points))))

(defn sample-segment-with-res
  [a b res include-b?]
  (let [ls (for [t (m/norm-range res)] (m/mix a b t))]
    (if include-b? ls (butlast ls))))

;; Interpolation
;;
;; d +----+ c
;;   |    |
;;   |    |
;; a +----+ b

(defn map-bilinear
  "Takes a seq of 4 points in ccw order and 2D vector of normalized UV
  coordinates. Applies bilinear interpolation to compute point within
  the rect: U is coord along AB/DC edge, V along BC/AD"
  [[a b c d] [u v]]
  (m/mix a b d c u v))

;; e +----+ h
;;   |\   :\
;;   |f+----+ g
;;   | |  : |
;; a +-|--+d|
;;    \|   \|
;;   b +----+ c

(defn map-trilinear
  "Takes a seq of 8 points defining a cuboid and vector of normalized
  UVW coordinates. Applies trilinear interpolation to compute point
  within the cuboid: U is coord along AD/BC edge, V along AE/BF, W
  along AB/DC (see above diagram)"
  [[a b c d e f g h] [u v w]]
  (m/mix (m/mix a d e h u v) (m/mix b c f g u v) w))

;; Tessellation

(defn tessellate-with-point
  ([points] (tessellate-with-point (centroid points) points))
  ([c points]
   (->> [(first points)]
        (concat points)
        (partition 2 1)
        (mapv #(vector c (first %) (nth % 1))))))

(defn tessellate-with-first
  [points]
  (if (> (count points) 3)
    (let [v0 (first points)]
      (mapv (fn [[a b]] [v0 a b]) (partition 2 1 (rest points))))
    [points]))

(defn tessellate-tri-with-midpoints
  [[a b c]]
  (let [ab (m/mix a b)
        bc (m/mix b c)
        ca (m/mix c a)]
    [[a ab ca] [ab b bc] [bc c ca] [ab bc ca]]))

(defn tessellate-3
  [points]
  (condp == (count points)
    3 [points]
    4 (let [[a b c d] points] [[a b c] [a c d]])
    (tessellate-with-point points)))

(defn tessellate-max4
  [points]
  (if (<= (count points) 4)
    [points]
    (tessellate-with-point points)))

(defn tessellate-face
  [tess-fn]
  (fn [[verts attribs]]
    (let [verts (tess-fn verts)
          attribs (reduce-kv
                   (fn [acc k v] (assoc acc k (tess-fn v)))
                   {} attribs)]
      (map-indexed
       (fn [i verts]
         [verts (reduce-kv (fn [acc k v] (assoc acc k (nth v i))) {} attribs)])
       verts))))

;; Normals

(defn ortho-normal
  ([[a b c]] (m/normalize (m/cross (m/- b a) (m/- c a))))
  ([a b] (m/normalize (m/cross a b)))
  ([a b c] (m/normalize (m/cross (m/- b a) (m/- c a)))))

;; Triangles

(defn norm-sign2
  [[ax ay] [bx by] [cx cy]]
  (- (mm/subm bx ax cy ay) (mm/subm cx ax by ay)))

(defn norm-sign3
  [a b c] (m/mag (m/cross (m/- b a) (m/- c a))))

(defn tri-area2
  [a b c] (* 0.5 (norm-sign2 a b c)))

(defn tri-area3
  [a b c] (* 0.5 (norm-sign3 a b c)))

(defn clockwise2?
  [a b c] (neg? (norm-sign2 a b c)))

(defn clockwise3?
  [a b c n] (pos? (m/dot (m/cross (m/- b a) (m/- c a)) n)))

(defn triangle-barycentric-coords
  ([[a b c] p]
   (triangle-barycentric-coords a b c p (m/- b a) (m/- c a)))
  ([a b c p]
   (triangle-barycentric-coords a b c p (m/- b a) (m/- c a)))
  ([a b c p u v]
   (let [w (m/- p a)
         uu (m/mag-squared u)
         vv (m/mag-squared v)
         uv (m/dot u v)
         wu (m/dot w u)
         wv (m/dot w v)
         denom (/ 1.0 (mm/msub uv uv uu vv))
         s (* denom (mm/msub uv wv vv wu))
         t (* denom (mm/msub uv wu uu wv))]
     [(- 1.0 (+ s t)) s t])))

(defn point-in-triangle2?
  [p a b c]
  (if (clockwise2? a b c)
    (and (>= (norm-sign2 a c p) 0.0)
         (>= (norm-sign2 b a p) 0.0)
         (>= (norm-sign2 c b p) 0.0))
    (and (>= (norm-sign2 b c p) 0.0)
         (>= (norm-sign2 a b p) 0.0)
         (>= (norm-sign2 c a p) 0.0))))

(defn point-in-triangle3?
  [p a b c]
  (let [u (m/- b a)
        v (m/- c a)
        n (ortho-normal u v)
        cl (- (m/dot n p) (m/dot n a))]
    (if (m/delta= 0.0 cl)
      (let [[u v w] (triangle-barycentric-coords a b c p u v)]
        (and (>= u 0.0) (>= w 0.0) (m/in-range? 0.0 1.0 v))))))

;; Area & volume
;; Volume calculation is using signed volume implementation of Triangle3

(defn- tessellating-transducer
  [f]
  (comp
   (mapcat tessellate-with-first)
   (map f)))

(def ^:private area-xf
  (tessellating-transducer #(->> % (apply tri-area3) m/abs*)))

(def ^:private volume-xf
  (tessellating-transducer #(m/dot (% 0) (m/cross (% 1) (% 2)))))

(defn total-area-3d
  ([faces] (transduce area-xf + faces))
  ([xf faces] (transduce (comp xf area-xf) + faces)))

(defn total-volume
  ([faces] (/ (transduce volume-xf + faces) 6.0))
  ([xf faces] (/ (transduce (comp xf volume-xf) + faces) 6.0)))

;; Bounds (w/ types)

(defn bounding-rect
  [points]
  (let [[p size] (bounding-rect-raw points)]
    (if p (Rect2. p size))))

(defn bounding-box
  [points]
  (let [[p size] (bounding-box-raw points)]
    (if p (AABB. p size))))

(defn bounding-circle
  ([points]
   (bounding-circle (centroid points) points))
  ([c r-or-points]
   (let [[c r] (radial-bounds vec2 c r-or-points)]
     (Circle2. c r))))

(defn bounding-sphere
  ([points]
   (bounding-sphere (centroid points) points))
  ([c r-or-points]
   (let [[c r] (radial-bounds vec3 c r-or-points)]
     (Sphere. c r))))

(defn coll-bounds
  "Takes a seq of shape entities, calls g/bounds on each and returns
  union bounds. Does not support collections of mixed 2d/3d entities."
  [coll]
  (reduce m/union (mapv g/bounds coll)))

(defn fit-all-into-bounds
  "Takes an AABB or rect and seq of shapes, proportionally scales and
  repositions all items to fit into given bounds. Returns lazyseq of
  transformed entities. Does not support collections of mixed 2D/3D
  entities. Use rects as target bounds for 2D colls."
  [bounds coll]
  (let [b (coll-bounds coll)
        s (reduce min (m/div (get bounds :size) (get b :size)))
        b' (g/center (g/scale b s) (g/centroid bounds))]
    (map
     #(-> %
          (g/center (g/unmap-point b' (g/map-point b (g/centroid %))))
          (g/scale-size s))
     coll)))

;; Meshes

(defn into-mesh
  "Takes a target mesh, its add-face fn and a source mesh or face
  sequence. Adds faces to target mesh and returns it."
  [mesh add-face mesh-or-faces]
  (reduce
   add-face
   mesh
   (if (satisfies? g/IFaceAccess mesh-or-faces)
     (g/faces mesh-or-faces true)
     mesh-or-faces)))

(defn map-mesh
  "Applies f to all faces of given mesh and adds resulting faces to
  new mesh. Thus f should return a seq of raw faces (i.e. each face as
  2-elem vector of [verts attribs]. Returns new mesh."
  [f mesh] (g/into (g/clear* mesh) (mapcat f (g/faces mesh true))))

;; TODO how to use this w/ IndexedMesh? Faces only store vertex IDs
(defn transform-mesh
  "Takes a mesh, its add-face fn and a vertex transformation fn.
  Transforms all mesh vertices and returns new mesh."
  [mesh add-face tx]
  (let [tx     (if (satisfies? g/IVectorTransform tx)
                 (fn [v] (g/transform-vector tx v))
                 tx)
        verts' (->> (g/vertices mesh)
                    (reduce
                     (fn [acc v] (assoc! acc v (tx v)))
                     (transient (hash-map)))
                    (persistent!))]
    (reduce
     (fn [mesh [fv fa]] (add-face mesh [(mapv verts' fv) fa]))
     (g/clear* mesh) (g/faces mesh true))))
