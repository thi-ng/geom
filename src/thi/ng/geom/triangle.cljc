(ns thi.ng.geom.triangle
  #?(:cljs (:require-macros [thi.ng.math.macros :as mm]))
  (:require
   [thi.ng.geom.core :as g]
   [thi.ng.geom.utils :as gu]
   [thi.ng.geom.utils.intersect :as isec]
   [thi.ng.geom.vector :as v :refer [vec2 vec3 V3Z]]
   [thi.ng.geom.attribs :as attr]
   [thi.ng.geom.basicmesh :as bm]
   #?(:clj [thi.ng.geom.types] :cljs [thi.ng.geom.types :refer [Circle2 Polygon2 Triangle2 Triangle3]])
   [thi.ng.dstruct.core :as d]
   [thi.ng.math.core :as m :refer [PI HALF_PI THIRD SQRT3 *eps*]]
   [thi.ng.xerror.core :as err]
   #?(:clj [thi.ng.math.macros :as mm]))
  #?(:clj (:import [thi.ng.geom.types Circle2 Polygon2 Triangle2 Triangle3])))

(defn triangle2
  ([t]
   (cond
     (map? t)        (Triangle2.
                      [(vec2 (get t :a)) (vec2 (get t :b)) (vec2 (get t :c))])
     (sequential? t) (Triangle2.
                      [(vec2 (first t)) (vec2 (nth t 1)) (vec2 (nth t 2))])
     :default (err/illegal-arg! t)))
  ([a b c] (Triangle2. [(vec2 a) (vec2 b) (vec2 c)])))

(defn triangle3
  ([t]
   (cond
     (map? t)        (Triangle3.
                      [(vec3 (get t :a)) (vec3 (get t :b)) (vec3 (get t :c))])
     (sequential? t) (Triangle3.
                      [(vec3 (first t)) (vec3 (nth t 1)) (vec3 (nth t 2))])
     :default (err/illegal-arg! t)))
  ([a b c] (Triangle3. [(vec3 a) (vec3 b) (vec3 c)])))

(defn equilateral2
  ([l]
   (cond
     (map? l) (equilateral2 (get l :p) (get l :q))
     (sequential? l) (equilateral2 (first l) (nth l 1))
     :default (err/illegal-arg! l)))
  ([a b]
   (let [a (vec2 a) b (vec2 b)
         dir (m/- a b)
         n (g/normal dir)
         c (-> n (m/normalize (mm/mul (m/mag dir) SQRT3 0.5)) (m/+ (m/mix a b)))]
     (triangle2 a b c)))
  ([x1 y1 x2 y2]
   (equilateral2 (vec2 x1 y1) (vec2 x2 y2))))

(defn equilateral3
  [a b n]
  (let [a (vec3 a) b (vec3 b)
        dir (m/- b a)
        n (m/normalize (m/cross dir n))
        c (-> n (m/normalize (mm/mul (m/mag dir) SQRT3 0.5)) (m/+ (m/mix a b)))]
    (Triangle3. [a b c])))

(defn other-point-in-tri
  [[ta tb tc] a b]
  (if (= a ta)
    (if (= b tb) tc tb)
    (if (= a tb)
      (if (= b ta) tc ta)
      (if (= b ta) tb ta))))

(defn altitude
  ([[a b c] id]
   (case id
     :a (altitude b c a)
     :b (altitude a c b)
     :c (altitude a b c)))
  ([a b c]
   [(m/mix a b (gu/closest-point-coeff c a b)) c]))

(defn norm-altitude
  ([points id]
   (let [[a b] (altitude points id)]
     (m/normalize (m/- b a))))
  ([a b c]
   (m/normalize (m/- c (m/mix a b (gu/closest-point-coeff c a b))))))

(defn centroid
  ([a b c] (m/* (m/+ a b c) THIRD))
  ([[a b c]] (m/* (m/+ a b c) THIRD)))

(defn check-edge
  [splits classifier e p q add-p? add-q?]
  (let [pc (classifier e p)
        qc (classifier e q)
        splits (if add-p? (conj splits [p pc]) splits)]
    (if (neg? (* pc qc))
      (let [ip (get (g/intersect-line e p q) :p)]
        (if add-q?
          (-> splits (conj [ip 0]) (conj [q qc]))
          (conj splits [ip 0])))
      (if add-q?
        (conj splits [q qc])
        splits))))

(defn slice-with*
  ([t e] (slice-with* t e g/classify-point))
  ([[a b c] e classifier] (slice-with* a b c e classifier))
  ([a b c e classifier]
   (let [verts (-> []
                   (check-edge classifier e a b true true)
                   (check-edge classifier e b c false true)
                   (check-edge classifier e c a false false))
         cmap (fn [ids]
                (->> ids
                     (map (fn [[a b c]] [(verts a) (verts b) (verts c)])) ;; TODO transducer
                     (reduce
                      (fn [m [a b c]]
                        (update-in m [(a 1)] conj [(a 0) (b 0) (c 0)]))
                      {-1 [] 1 []})))]
     (condp = (count verts)
       4 (let [triverts #{a b c}
               d (loop [i 3]
                   (if-let [vc (verts i)]
                     (if (and (zero? (vc 1)) (triverts (vc 0)))
                       i (recur (dec i)))))]
           (cmap [[(m/wrap-range (inc d) 4) (m/wrap-range (+ d 2) 4) d]
                  [(m/wrap-range (dec d) 4) d (m/wrap-range (+ d 2) 4)]]))
       5 (if (zero? (get-in verts [1 1]))
           (if (zero? (get-in verts [3 1]))
             (cmap [[0 1 3] [0 3 4] [2 3 1]])
             (cmap [[0 1 4] [2 4 1] [2 3 4]]))
           (cmap [[0 1 2] [0 2 4] [3 4 2]]))
       nil))))

;; http://astronomy.swin.edu.au/~pbourke/modelling/triangulate/
(defn circumcircle-raw
  [[ax ay :as a] [bx by :as b] [cx cy :as c]]
  (let [eq-ab? (m/delta= ay by *eps*)
        eq-bc? (m/delta= by cy *eps*)]
    (when-not (and eq-ab? eq-bc?)
      (let [o (cond
                eq-ab? (let [cx (mm/addm ax bx 0.5)]
                         (vec2 cx (mm/submadd
                                   cx (mm/addm bx cx 0.5)
                                   (- (mm/subdiv cx bx cy by))
                                   (mm/addm by cy 0.5))))
                eq-bc? (let [cx (mm/addm bx cx 0.5)]
                         (vec2 cx (mm/submadd
                                   cx (mm/addm ax bx 0.5)
                                   (- (mm/subdiv bx ax by ay))
                                   (mm/addm ay by 0.5))))
                :default (let [m1 (- (mm/subdiv bx ax by ay))
                               m2 (- (mm/subdiv cx bx cy by))
                               mx1 (mm/addm ax bx 0.5)
                               my1 (mm/addm ay by 0.5)
                               cx (/ (mm/add
                                      (mm/msub m1 mx1 m2 (mm/addm bx cx 0.5))
                                      (mm/addm by cy 0.5)
                                      (- my1))
                                     (- m1 m2))]
                           (vec2 cx (mm/submadd cx mx1 m1 my1))))]
        [o (g/dist o b)]))))

(defn circumcircle
  ([t] (circumcircle (get t :a) (get t :b) (get t :c)))
  ([a b c]
   (let [[o r] (circumcircle-raw a b c)]
     (Circle2. o r))))

(extend-type Triangle2

  g/IArea
  (area [_] (apply gu/tri-area2 (get _ :points)))

  g/IClassify
  (classify-point
    [_ p] (->> (g/edges _)
               (map #(m/signum (apply gu/closest-point-coeff p %) *eps*))
               (reduce min)))

  g/IBoundary
  (contains-point?
    [_ p] (apply gu/point-in-triangle2? p (get _ :points)))

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
    ([_] (Triangle2. (gu/center (vec2) (get _ :points))))
    ([_ o] (Triangle2. (gu/center (g/centroid _) (vec2 o) (get _ :points)))))
  (centroid [_] (centroid (get _ :points)))

  g/ICircumference
  (circumference
    [{[a b c] :points}] (mm/add (g/dist a b) (g/dist b c) (g/dist c a)))

  g/IExtrude
  (extrude [_ opts] (g/extrude (g/as-polygon _) opts))
  (extrude-shell [_ opts] (g/extrude-shell (g/as-polygon _) opts))

  g/IFlip
  (flip
    [_] (Triangle2. (reverse (get _ :points))))

  g/IVertexAccess
  (vertices
    [_] (get _ :points))

  g/IEdgeAccess
  (edges
    [{[a b c] :points}] [[a b] [b c] [c a]])

  g/IGraph
  (vertex-neighbors
    [{[a b c] :points} v] (condp = v, a [c b], b [a c], c [b a], nil))
  (vertex-valence
    [_ v] (if ((set (get _ :points)) v) 2 0))

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

  g/IMeshConvert
  (as-mesh
    ([_] (g/as-mesh _ {}))
    ([_ opts]
     (g/add-face
      (or (get opts :mesh) (bm/basic-mesh))
      (attr/generate-face-attribs (mapv vec3 (get _ :points)) 0 (get opts :attribs) opts))))

  g/IPolygonConvert
  (as-polygon
    [_] (Polygon2. (get _ :points)))

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
    [_] (gu/from-barycentric (get _ :points) (m/normdist-weights 3)))
  (sample-uniform
    [{p :points} udist include-last?]
    (gu/sample-uniform udist include-last? (conj p (first p))))

  g/ISlice
  (slice-with
    ([_ e]
     (slice-with* (get _ :points) e g/classify-point))
    ([_ e classifier]
     (slice-with* (get _ :points) e classifier)))

  g/ISubdivide
  (subdivide
    [_] (->> (get _ :points)
             (gu/tessellate-with-point)
             (map #(Triangle2. %))))

  g/ITessellate
  (tessellate [_] [_])

  g/IRotate
  (rotate
    [_ theta] (Triangle2. (mapv #(g/rotate % theta) (get _ :points))))

  g/IScale
  (scale
    [_ s] (Triangle2. (mapv #(m/* % s) (get _ :points))))
  (scale-size
    [_ s] (Triangle2. (gu/scale-size s (get _ :points))))

  g/ITranslate
  (translate
    [_ t] (Triangle2. (mapv #(m/+ % t) (get _ :points))))

  g/ITransform
  (transform
    [_ m] (Triangle2. (mapv #(g/transform-vector m %) (get _ :points))))

  g/IVolume
  (volume [_] 0.0))

(extend-type Triangle3

  g/IArea
  (area [_] (apply gu/tri-area3 (get _ :points)))

  g/IBoundary
  (contains-point? [_ p] (apply gu/point-in-triangle3? p (get _ :points)))

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
    ([_] (Triangle3. (gu/center (vec3) (get _ :points))))
    ([_ o] (Triangle3. (gu/center (g/centroid _) (vec3 o) (get _ :points)))))
  (centroid [_] (centroid (get _ :points)))

  g/ICircumference
  (circumference
    [{[a b c] :points}] (mm/add (g/dist a b) (g/dist b c) (g/dist c a)))

  g/IClassify
  (classify-point
    [_ p]
    (let [a (first (get _ :points))
          n (gu/ortho-normal (get _ :points))
          w (- (m/dot n a))]
      (-> n (m/dot p) (+ w) (m/signum *eps*))))

  g/IExtrude
  (extrude [_ opts] (err/unsupported!)) ; TODO
  (extrude-shell [_ opts] (err/unsupported!)) ; TODO

  g/IFlip
  (flip
    [_] (Triangle3. (reverse (get _ :points))))

  g/IVertexAccess
  (vertices
    [_] (get _ :points))

  g/IEdgeAccess
  (edges
    [{[a b c] :points}] [[a b] [b c] [c a]])

  g/IGraph
  (vertex-neighbors
    [{[a b c] :points} v] (condp = v, a [c b], b [a c], c [b a], nil))
  (vertex-valence
    [_ v] (if ((set (get _ :points)) v) 2 0))

  g/IIntersect
  (intersect-ray
    ([{[a b c] :points} ray]
     (let [[p dir] (if (map? ray) [(get ray :p) (get ray :dir)] ray)]
       (isec/intersect-ray-triangle3? p dir a b c)))
    ([{[a b c] :points} p dir]
     (isec/intersect-ray-triangle3? p dir a b c)))

  g/IMeshConvert
  (as-mesh
    ([_] (g/as-mesh _ {}))
    ([_ opts]
     (g/add-face
      (or (get opts :mesh) (bm/basic-mesh))
      (attr/generate-face-attribs (get _ :points) 0 (get opts :attribs) opts))))

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
    [_] (gu/from-barycentric (get _ :points) (m/normdist-weights 3)))
  (sample-uniform
    [{p :points} udist include-last?]
    (gu/sample-uniform udist include-last? (conj p (first p))))

  g/ISlice
  (slice-with
    ([_ e]
     (slice-with* (get _ :points) e g/classify-point))
    ([_ e classifier]
     (slice-with* (get _ :points) e classifier)))
  g/ISubdivide
  (subdivide
    [_] (->> (get _ :points)
             (gu/tessellate-with-point)
             (map #(Triangle3. %))))

  g/ITessellate
  (tessellate [_] [_])

  g/IRotate
  (rotate
    [_ theta] (Triangle3. (mapv #(g/rotate % theta) (get _ :points))))

  g/IRotate3D
  (rotate-x
    [_ theta] (Triangle3. (mapv #(g/rotate-x % theta) (get _ :points))))
  (rotate-y
    [_ theta] (Triangle3. (mapv #(g/rotate-y % theta) (get _ :points))))
  (rotate-z
    [_ theta] (Triangle3. (mapv #(g/rotate-z % theta) (get _ :points))))
  (rotate-around-axis
    [_ axis theta]
    (Triangle3.
     (mapv #(g/rotate-around-axis % axis theta) (get _ :points))))

  g/IScale
  (scale
    [_ s] (Triangle3. (mapv #(m/* % s) (get _ :points))))
  (scale-size
    [_ s] (Triangle3. (gu/scale-size s (get _ :points))))

  g/ITranslate
  (translate
    [_ t] (Triangle3. (mapv #(m/+ % t) (get _ :points))))

  g/ITransform
  (transform
    [_ m] (Triangle3. (mapv #(g/transform-vector m %) (get _ :points))))

  ;; Signed volume impl based on:
  ;; https://web.archive.org/web/20090320013931/http://amp.ece.cmu.edu/Publication/Cha/icip01_Cha.pdf

  g/IVolume
  (volume [_] 0.0)
  (signed-volume
    [{[a b c] :points}] (/ (m/dot a (m/cross b c)) 6.0)))
