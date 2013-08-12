(ns thi.ng.geom.plane
  (:require
   [thi.ng.math.core :as m]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.mesh :as mesh]
   [thi.ng.geom.types :as t]
   ))

(extend-type t/Plane
  g/IShape
  (classify-point [{p :p n :n} q]
    (-> (g/sub3 q p) (g/normalize3) (g/dot3 n) (m/signum m/*eps*)))
  (closest-point [{p :p n :n} q]
    (let [sn (- (g/dot3 n (g/sub3 q p)))]
      (g/add3 q (g/normalize3 n sn))))
  g/IIntersectable
  (intersect-line [{p :p n :n} {lp :p lq :q}]
    (let [u (g/sub3 lq lp)
          w (g/sub3 lp p)
          d (g/dot3 n u)
          dn (- (g/dot3 n w))]
      (if (m/delta= d 0.0 m/*eps*)
        {:type (if (zero? dn) :on-plane :parallel)}
        (let [du (/ dn d)
              i (g/add3 lp (g/scale3 u du))]
          (if (m/in-range? 0.0 1.0 du)
            {:type :intersect :p i}
            {:type :intersect-outside :p i}))))))

(defn plane
  [p n] (t/Plane. (g/vec3 p) (g/normalize3 (g/vec3 n))))

(defn intersect-plane
  [{a :p na :n} {b :p nb :n}]
  (let [da (g/dot3 a na) db (g/dot3 b nb)]
    (when-not (or (m/delta= na nb) (m/delta= da db))
      (let [od (g/dot3 na nb)
            det (/ 1.0 (- 1 (* od od)))
            ta (* det (- da (* db od)))
            tb (* det (- db (* da od)))]
        [(g/fma3 na ta (g/scale3 nb tb)) (g/cross3 na nb)]))))
