(ns thi.ng.geom.sphere
  #?(:cljs
     (:require-macros [thi.ng.math.macros :as mm]))
  (:require
   [thi.ng.geom.core :as g :refer [*resolution*]]
   [thi.ng.geom.utils :as gu]
   [thi.ng.geom.vector :as v :refer [vec2 vec3]]
   [thi.ng.geom.utils.intersect :as isec]
   [thi.ng.geom.basicmesh :as bm]
   [thi.ng.geom.attribs :as attr]
   [thi.ng.geom.types :as types]
   [thi.ng.xerror.core :as err]
   [thi.ng.math.core :as m :refer [TWO_PI PI *eps*]]
   #?(:clj [thi.ng.math.macros :as mm]
      :cljs [thi.ng.typedarrays.core :as ta]))
  #?(:clj
     (:import
      [thi.ng.geom.types AABB Sphere])))

(defn sphere
  ([] (thi.ng.geom.types.Sphere. (vec3) 1.0))
  ([r] (thi.ng.geom.types.Sphere. (vec3) #?(:clj (double r) :cljs r)))
  ([p r] (thi.ng.geom.types.Sphere. (vec3 p) #?(:clj (double r) :cljs r))))

(extend-type thi.ng.geom.types.Sphere

  g/IArea
  (area
    [{r :r}] (* 4.0 PI r r))

  g/IBoundary
  (contains-point?
    [{p :p r :r} q] (<= (g/dist-squared p q) (* r r)))

  g/IBounds
  (bounds
    [_] (thi.ng.geom.types.AABB. (m/- (get _ :p) (get _ :r)) (vec3 (* 2 (get _ :r)))))
  (width  [_] (* 2.0 (get _ :r)))
  (height [_] (* 2.0 (get _ :r)))
  (depth  [_] (* 2.0 (get _ :r)))

  g/IBoundingSphere
  (bounding-sphere [_] _)

  g/ICenter
  (center
    ([_] (thi.ng.geom.types.Sphere. (vec3) (get _ :r)))
    ([_ p] (thi.ng.geom.types.Sphere. (vec3 p) (get _ :r))))
  (centroid [_] (get _ :p))

  g/IClassify
  (classify-point
    [{p :p r :r} q]
    (m/signum (- (* r r) (g/dist-squared p q)) *eps*))

  ;; If ray intersects sphere, the function will return a 2-element
  ;; vector of distances from the ray's start point to the sphere
  ;; surface. If either of those distances is negative the
  ;; intersection point lies in the opposite ray direction. The actual
  ;; intersection points can be computed by passing the distances into
  ;; the following form:
  ;;
  ;; ```
  ;; ;; dir = ray direction
  ;; ;; p = ray start point
  ;; ;; t = distance
  ;; (m/madd dir t p)
  ;; ```
  ;;
  ;; If there's no intersection, the function returns `nil`.

  g/IIntersect
  (intersect-ray
    [{p :p r :r} ray]
    (let [[rp dir] (if (map? ray) [(get ray :p) (get ray :dir)] ray)]
      (isec/intersect-ray-sphere? rp dir p r)))
  (intersect-shape
    [_ s]
    (cond
      (instance? thi.ng.geom.types.AABB s)
      (isec/intersect-aabb-sphere? s _)
      (instance? thi.ng.geom.types.Sphere s)
      (isec/intersect-sphere-sphere? _ s)
      (instance? thi.ng.geom.types.Plane s)
      (isec/intersect-plane-sphere? (get s :n) (get s :w) (get _ :p) (get _ :r))
      :default (err/type-error! "Sphere" s)))

  g/IMeshConvert
  (as-mesh
    ([_] (g/as-mesh _ {}))
    ([{[x y z] :p r :r}
      {:keys [mesh res slices stacks attribs] :as opts}]
     (let [res     (or res *resolution*)
           slices  (or slices res)
           stacks  (or stacks res)
           rsl     (m/norm-range slices)
           rst     (m/norm-range stacks)
           st      (#?(:clj double-array :cljs ta/float32) (map #(Math/sin (* TWO_PI %)) rsl))
           ct      (#?(:clj double-array :cljs ta/float32) (map #(Math/cos (* TWO_PI %)) rsl))
           sp      (#?(:clj double-array :cljs ta/float32) (map #(Math/sin (* PI %)) rst))
           cp      (#?(:clj double-array :cljs ta/float32) (map #(Math/cos (* PI %)) rst))
           iu      (/ 1.0 slices)
           iv      (/ 1.0 stacks)
           stacks' (dec stacks)
           fgen    (if attribs
                     (fn [fverts verts fid]
                       (attr/generate-face-attribs
                        fverts fid attribs
                        (merge {:uv (mapv #(vec2 (* iu (first %)) (* iv (nth % 1))) verts)} opts)))
                     (fn [fverts _ _] [fverts]))]
       (loop [acc (transient []), fid 0, i 0, j 0]
         (if (< i slices)
           (let [ii     (inc i)
                 jj     (inc j)
                 verts  (if (pos? j)
                          (if (< j stacks')
                            [[i j] [ii j] [ii jj] [i jj]]
                            [[i j] [ii j] [i jj]])
                          [[i j] [ii jj] [i jj]])
                 fverts (mapv
                         (fn [[u v]]
                           (vec3
                            (+ (mm/mul (aget ct u) (aget sp v) r) x)
                            (mm/madd (aget cp v) r y)
                            (+ (mm/mul (aget st u) (aget sp v) r) z)))
                         verts)]
             (recur (conj! acc (fgen fverts verts fid)) (inc fid) ii j))
           (if (< j stacks')
             (recur acc fid 0 (inc j))
             (g/into (or mesh (bm/basic-mesh)) (persistent! acc))))))))

  g/IProximity
  (closest-point
    [{p :p r :r} q]
    (m/+! (m/normalize (m/- q p) r) p))

  g/ISample
  (random-point-inside
    [_]
    (m/+ (get _ :p) (v/randvec3 (m/random (get _ :r)))))
  (random-point
    [_]
    (m/+ (get _ :p) (v/randvec3 (get _ :r))))

  g/ITessellate
  (tessellate
    ([_] (g/tessellate _ {}))
    ([_ opts] (g/tessellate (g/as-mesh _ opts))))

  g/IRotate
  (rotate
    [_ theta] (thi.ng.geom.types.Sphere. (g/rotate-z (get _ :p) theta) (get _ :r)))

  g/IRotate3D
  (rotate-x
    [_ theta] (thi.ng.geom.types.Sphere. (g/rotate-x (get _ :p) theta) (get _ :r)))
  (rotate-y
    [_ theta] (thi.ng.geom.types.Sphere. (g/rotate-y (get _ :p) theta) (get _ :r)))
  (rotate-z
    [_ theta] (thi.ng.geom.types.Sphere. (g/rotate-z (get _ :p) theta) (get _ :r)))
  (rotate-around-axis
    [_ axis theta]
    (thi.ng.geom.types.Sphere.
     (g/rotate-around-axis (get _ :p) axis theta) (get _ :r)))

  ;; FIXME scale with non-uniform values should return Ellipsoid

  g/IScale
  (scale [_ s] (thi.ng.geom.types.Sphere. (m/* (get _ :p) s) (* (get _ :r) s)))
  (scale-size [_ s] (thi.ng.geom.types.Sphere. (get _ :p) (* (get _ :r) s)))

  g/ITranslate
  (translate [_ t] (thi.ng.geom.types.Sphere. (m/+ (get _ :p) t) (get _ :r)))

  g/IVolume
  (volume [{r :r}] (mm/mul (/ 4.0 3.0) PI r r r)))
