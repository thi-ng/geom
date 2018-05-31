(ns thi.ng.geom.plane
  #?(:cljs (:require-macros [thi.ng.math.macros :as mm]))
  (:require
   [thi.ng.geom.core :as g]
   [thi.ng.geom.utils :as gu]
   [thi.ng.geom.utils.intersect :as isec]
   [thi.ng.geom.vector :as v :refer [vec3]]
   [thi.ng.geom.quaternion :as q]
   [thi.ng.geom.basicmesh :as bm]
   [thi.ng.geom.attribs :as attr]
   #?(:clj [thi.ng.geom.types] :cljs [thi.ng.geom.types :refer [AABB Plane Sphere]])
   [thi.ng.xerror.core :as err]
   [thi.ng.math.core :as m :refer [*eps* INF+]]
   #?(:clj [thi.ng.math.macros :as mm]))
   #?(:clj (:import [thi.ng.geom.types AABB Plane Sphere])))

;; A plane in cartesian 3D space can be defined as a point `p` lying
;; on the plane and normal vector `n` standing perpendicular on the
;; plane. The latter defines the plane's orientation in space.

(defn plane
  [n w] (Plane. (m/normalize (vec3 n)) w))

(defn plane-with-point
  [p n]
  (let [n (m/normalize (vec3 n))]
    (Plane. n (- (m/dot n p)))))

(defn plane-from-points
  ([[a b c]] (plane-from-points a b c))
  ([a b c]
   (let [n (gu/ortho-normal a b c)]
     (Plane. n (- (m/dot n a))))))

(extend-type Plane

  g/IArea
  (area [_] INF+)

  g/IBounds
  (bounds
    [_]
    (let [s (vec3 (g/width _) (g/height _) (g/depth _))]
      (AABB. (m/madd s -0.5 (g/centroid _)) s)))
  (width
    [_] (if (m/delta= (m/abs (get _ :n)) v/V3X *eps*) 0.0 INF+))
  (height
    [_] (if (m/delta= (m/abs (get _ :n)) v/V3Y *eps*) 0.0 INF+))
  (depth
    [_] (if (m/delta= (m/abs (get _ :n)) v/V3Z *eps*) 0.0 INF+))

  g/IBoundingSphere
  (bounding-sphere
    [_] (Sphere. (g/centroid _) INF+))

  g/ICenter
  (center
    ([_] (Plane. (get _ :n) 0))
    ([_ o] (plane-with-point o (get _ :n))))
  (centroid
    ([_] (m/* (get _ :n) (- (get _ :w)))))

  g/IClassify
  (classify-point
    [_ p]
    (-> (get _ :n) (m/dot p) (+ (get _ :w)) (m/signum *eps*)))

  g/IDistance
  (dist
    [_ p] (+ (m/dot (get _ :n) p) (get _ :w)))
  (dist-squared
    [_ p] (let [d (g/dist _ p)] (* d d)))

  g/IExtrude
  (extrude [_ {}] (err/unsupported!))

  g/IFlip
  (flip
    [_] (Plane. (m/- (get _ :n)) (- (get _ :w))))

  g/IIntersect
  (intersect-line
    ([_ l]
     (let [[p q] (if (map? l) (get l :points) l)]
       (isec/intersect-ray-plane? p (m/- q p) (get _ :n) (get _ :w))))
    ([_ p q]
     (isec/intersect-ray-plane? p (m/- q p) (get _ :n) (get _ :w))))
  (intersect-ray
    ([_ ray]
     (let [[p dir] (if (map? ray) [(get ray :p) (get ray :dir)] ray)]
       (isec/intersect-ray-plane? p dir (get _ :n) (get _ :w))))
    ([_ p dir]
     (isec/intersect-ray-plane? p dir (get _ :n) (get _ :w))))
  (intersect-shape
    [_ s]
    (cond
      (instance? Plane s)
      (isec/intersect-plane-plane? (get _ :n) (get _ :w) (get s :n) (get s :w))
      (instance? Sphere s)
      (isec/intersect-plane-sphere? (get _ :n) (get _ :w) (get s :p) (get s :r))
      :default (err/illegal-arg! s)))

  g/IMeshConvert
  (as-mesh
    ([_] (g/as-mesh _ {}))
    ([_ {:keys [p width height size mesh attribs] :or {size 1.0}}]
     (let [w     (* (or width size) 0.5)
           h     (* (or height size) 0.5)
           flip? (m/delta= -1.0 (m/dot (get _ :n) v/V3Z))
           p     (g/closest-point _ (or p (vec3)))
           q     (if flip?
                   (q/quat 0 0 0 1)
                   (q/alignment-quat v/V3Z (get _ :n)))
           face  (mapv
                  #(m/+ p (g/transform-vector q %))
                  [(vec3 (- w) (- h) 0) (vec3 (- w) h 0)
                   (vec3 w h 0) (vec3 w (- h) 0)])
           face  (attr/generate-face-attribs (if flip? face (rseq face)) 0 attribs nil)]
       (g/add-face (or mesh (bm/basic-mesh)) face))))

  g/IProximity
  (closest-point
    [{:keys [n] :as _} p]
    (->> p
         (m/dot n)
         (+ (get _ :w))
         (m/normalize n)
         (m/- p)))

  g/IScale
  (scale
    [_ s] (plane-with-point (m/* (g/centroid _) s) (get _ :n)))
  (scale-size
    ([_ s] _))

  g/ITranslate
  (translate
    [_ t] (plane-with-point (m/+ (g/centroid _) t) (get _ :n)))

  g/ITransform
  (transform
    [_ m] (plane-with-point
           (g/transform-vector m (g/centroid _))
           (g/transform-vector m (get _ :n))))

  g/IVolume
  (volume [_] 0.0))
