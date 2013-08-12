(ns thi.ng.geom.sphere
  (:require
    [thi.ng.math.core :as m]
    [thi.ng.geom.core :as g]
    [thi.ng.geom.types :as t]))

(extend-type t/Sphere
  g/IShape
    (bounds [this] (t/AABB. (:p this) (g/vec3 (:r this))))
    (center
      ([this] (t/Sphere. [0.0 0.0 0.0] (:r this)))
      ([this p] (t/Sphere. p (:r this))))
    (centroid [this] (:p this))
    (contains-point? [this q] (<= (g/dist3 (:p this) q) (:r this)))
    (classify-point [{p :p r :r} q]
      (m/signum (- (* r r) (g/dist3-squared p q)) m/*eps*))
    (random-point [this]
      (g/add3 (:p this) (g/randvec3 (m/random (:r this)))))
    (random-boundary-point [this]
      (g/add3 (:p this) (g/randvec3 (:r this))))
  g/IShape3
    (as-mesh
      ([this] (g/as-mesh this g/*resolution*))
      ([this res] nil))
    (bounding-sphere [this] this))

(defn sphere
  ([] (t/Sphere. [0.0 0.0 0.0] 1.0))
  ([r] (t/Sphere. [0.0 0.0 0.0] r)))
