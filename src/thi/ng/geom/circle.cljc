(ns thi.ng.geom.circle
  (:require
   [thi.ng.geom.core :as g :refer [*resolution*]]
   [thi.ng.geom.vector :as v :refer [vec2 vec3]]
   [thi.ng.geom.attribs :as attr]
   [thi.ng.geom.utils :as gu]
   [thi.ng.geom.utils.intersect :as isec]
   [thi.ng.geom.basicmesh :as bm]
   #?(:clj [thi.ng.geom.types] :cljs [thi.ng.geom.types :refer [Circle2 Polygon2 Rect2 Triangle2]])
   [thi.ng.xerror.core :as err]
   [thi.ng.math.core :as m :refer [PI TWO_PI *eps*]])
  #?(:clj
     (:import
      [thi.ng.geom.types Circle2 Polygon2 Rect2 Triangle2])))

(defn circle
  ([] (Circle2. (vec2) 1.0))
  ([r] (Circle2. (vec2) r))
  ([p r] (Circle2. (vec2 p) r))
  ([x y r] (Circle2. (vec2 x y) r)))

(defn tangent-points
  [{p :p :as _} q]
  (let [m (m/mix p q)]
    (isec/intersect-circle-circle? _ (circle m (g/dist m p)))))

;; https://stats.stackexchange.com/questions/481543/generating-random-points-uniformly-on-a-disk
(defn random-point-in-circle
  "Randomly generate a point inside of circle with uniform distribution."
  [{p :p r :r}]
  (-> (vec2 (* r (Math/sqrt (m/random)))
            (* m/TWO_PI (m/random)))
      g/as-cartesian
      (m/+ p)))

;; Even though a circle is a specialization of an ellipse, we define
;; an extra type for performance reasons.

(extend-type Circle2

  g/IArea
  (area [{r :r}] (* PI (* r r)))

  g/IBounds
  (bounds
    [{p :p r :r}] (Rect2. (m/- p r) (vec2 (* 2.0 r))))
  (width  [_] (* 2.0 (get _ :r)))
  (height [_] (* 2.0 (get _ :r)))
  (depth  [_] 0)

  g/IBoundingCircle
  (bounding-circle [_] _)

  g/IBoundary
  (contains-point?
    [{p :p r :r} q]
    (<= (g/dist-squared p q) (* r r)))

  g/ICenter
  (center
    ([_] (Circle2. (vec2) (get _ :r)))
    ([_ p] (Circle2. (vec2 p) (get _ :r))))
  (centroid [_] (get _ :p))

  g/ICircumference
  (circumference [_] (* TWO_PI (get _ :r)))

  g/IClassify
  (classify-point
    [_ q]
    (m/signum (- (get _ :r) (g/dist (get _ :p) q)) *eps*))

  g/IExtrude
  (extrude
    [{p :p :as _} {:keys [mesh res depth offset scale top? bottom?]
                   :or {res *resolution*, depth 1.0, scale 1.0, top? true, bottom? true}}]
    (let [points   (g/vertices _ res)
          tpoints  (if (== 1.0 scale)
                     points
                     (g/vertices (circle p (* scale (get _ :r))) res))
          off      (or offset (vec3 0 0 depth))
          points3  (mapv vec3 points)
          tpoints3 (mapv #(m/+ off %) tpoints)]
      (g/into
       (or mesh (bm/basic-mesh))
       (concat
        (when bottom?
          (->> points
               (gu/tessellate-with-point p)
               (mapv (fn [[a b c]] [[(vec3 b) (vec3 a) (vec3 c)]])))) ;; TODO attribs support
        (map
         (fn [[a1 b1] [a2 b2]] [[a1 b1 b2 a2]])
         (partition 2 1 (conj points3 (nth points3 0)))
         (partition 2 1 (conj tpoints3 (nth tpoints3 0))))
        (when top?
          (->> tpoints3
               (gu/tessellate-with-point (m/+ off p))
               (mapv vector)))))))
  (extrude-shell
    [_ opts] (g/extrude-shell (g/as-polygon _) opts))

  g/IVertexAccess
  (vertices
    ([_] (g/vertices _ *resolution*))
    ([{:keys [p r]} res]
     (->> (m/norm-range res)
          butlast
          (mapv #(m/+ p (g/as-cartesian (vec2 r (* % TWO_PI))))))))

  g/IEdgeAccess
  (edges
    ([_] (g/edges _ *resolution*))
    ([_ res]
     (let [verts (g/vertices _ res)]
       (partition 2 1 (conj verts (first verts))))))

  g/IIntersect
  (intersect-shape
    [_ s]
    (cond
      (instance? Circle2 s) (isec/intersect-circle-circle? _ s)
      (instance? Rect2 s)   (isec/intersect-rect-circle? s _)
      :else                                   (err/type-error! "Circle2" s)))

  g/IMeshConvert
  (as-mesh
    ([_] (g/as-mesh _ {:res *resolution*}))
    ([_ {:keys [mesh res attribs]}]
     (let [ires (/ TWO_PI res)
           aopts {:delta ires :r 0.5}]
       (->> res
            (g/vertices _)
            (map vec3)
            (gu/tessellate-with-point (vec3 (get _ :p)))
            (map-indexed
             (fn [i verts]
               (attr/generate-face-attribs
                verts i attribs (assoc aopts :theta (* i ires)))))
            (g/into (or mesh (bm/basic-mesh)))))))

  g/IPolygonConvert
  (as-polygon
    ([_] (g/as-polygon _ *resolution*))
    ([_ res] (Polygon2. (vec (g/vertices _ res)))))

  g/IProximity
  (closest-point
    [{p :p :as _} q]
    (m/+! (m/normalize (m/- q p) (get _ :r)) p))

  g/ISample
  (point-at
    [_ t]
    (m/+ (get _ :p) (g/as-cartesian (vec2 (get _ :r) (* t TWO_PI)))))
  (random-point
    [_] (g/point-at _ (m/random)))
  (random-point-inside
    [_]
    (m/+! (v/randvec2 (m/random (get _ :r))) (get _ :p)))
  (sample-uniform
    [_ udist include-last?]
    (let [points (g/vertices _)]
      (gu/sample-uniform udist include-last? (conj (vec points) (first points)))))

  ;; A circle can be tessellated into a list of uniform triangles,
  ;; each sharing the circle's centroid. If called without 2nd
  ;; argument the default =*resolution*= (a dynamic var defined
  ;; in `thi.ng.geom.core`) will be used.

  g/ITessellate
  (tessellate
    ([_] (g/tessellate _ *resolution*))
    ([{p :p :as _} res]
     (->> res
          (g/vertices _)
          (gu/tessellate-with-point p)
          (map #(Triangle2. %)))))

  ;; TODO scale with non-uniform values should return Ellipse
  ;; Since transforming a circle with a matrix can produce non-circular
  ;; results, the `transform` implementation returns a polygon.

  g/IRotate
  (rotate [_ theta] (Circle2. (g/rotate (get _ :p) theta) (get _ :r)))

  g/IScale
  (scale [_ s] (Circle2. (m/* (get _ :p) s) (* (get _ :r) s)))
  (scale-size [_ s] (Circle2. (get _ :p) (* (get _ :r) s)))

  g/ITranslate
  (translate [_ t] (Circle2. (m/+ (get _ :p) t) (get _ :r)))

  g/ITransform
  (transform [_ m] (g/transform (g/as-polygon _) m))

  g/IVolume
  (volume [_] 0))
