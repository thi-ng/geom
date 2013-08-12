(ns thi.ng.geom.circle
  (:require
    [thi.ng.data.core :as d]
    [thi.ng.math.core :as m]
    [thi.ng.geom.core :as g]
    [thi.ng.geom.types :as t]))

(extend-type t/Circle
  g/IShape
    (bounds [{p :p r :r}]
      (let [d (* 2 r)] (g/bounding-rect* (g/sub2 p [r r]) d d)))
    (center
      ([this] (t/Circle. [0.0 0.0] (:r this)))
      ([this o] (t/Circle. o (:r this))))
    (centroid [this] (:p this))
    (classify-point [{p :p r :r} q]
      (m/signum (- r (g/dist2 p q)) m/*eps*))
    (closest-point [{p :p r :r} q]
      (g/add2 p (g/normalize2 (g/sub2 q p) r)))
    (contains-point? [{p :p r :r} q]
      (<= (g/mag2-squared (g/sub2 p q)) (* r r)))
    (point-at [{p :p r :r} t]
      (g/add2 p (g/cartesian2 [r (* t m/TWO_PI)])))
    (random-point [{p :p r :r}]
      (g/add2 p (g/scale2 (g/randvec2) (m/random r))))
    (random-boundary-point [this] (g/point-at this (m/random)))
  g/IShape2
    (area [{r :r}] (* (* m/PI r) r))
    (bounding-circle [this] this)
    (circumference [{r :r}] (* m/TWO_PI r))
    (edges [this] (g/edges this g/*resolution*))
    (edges [this res]
      (let [verts (map (partial g/point-at this)
                    (range 0.0 m/TWO_PI (/ m/TWO_PI res)))]
        (map (fn [[p q]] (t/Line2. p q))
          (d/successive-nth 2 (concat verts [(first verts)])))))
    (as-polygon
      ([this] (g/as-polygon this g/*resolution*))
      ([{p :p r :r} res]
        (t/Polygon. (vec (map (fn [x] (g/add2 p (g/cartesian2 [r x])))
          (range 0 m/TWO_PI (/ m/TWO_PI res))))))))

(defn circle
  ([] (t/Circle. [0.0 0.0] 1.0))
  ([r] (t/Circle. [0.0 0.0] r))
  ([p r] (t/Circle. (g/vec2 p) r))
  ([x y r] (t/Circle. (g/vec2 x y) r)))

(defn ellipse
  ([] {:p [0.0 0.0] :r [1.0 1.0]})
  ([r] {:p [0.0 0.0] :r (g/vec2 r)})
  ([p r] {:p (g/vec2 p) :r (g/vec2 r)})
  ([x y r] {:p (g/vec2 x y) :r (g/vec2 r)})
  ([x y rx ry] {:p (g/vec2 x y) :r (g/vec2 rx ry)}))

(defn intersect-circle
  [{p :p r1 :r} {q :p r2 :r}]
  (let [delta (g/sub2 q p)
        d (g/mag2 delta)]
    (if (and (<= d (+ r1 r2)) (>= d (m/abs (- r1 r2))))
      (let [a (/ (+ (- (* r1 r1) (* r2 r2)) (* d d)) (* 2 d))
            invd (/ 1.0 d)
            p (g/add2 p (g/scale2 delta (* a invd)))
            h (Math/sqrt (- (* r1 r1) (* a a)))
            perp (g/scale2 (g/perpendicular2 delta) (* h invd))]
        [(g/add2 p perp) (g/sub2 p perp)]))))

(defn tangent-points
  [{p :p :as c} q]
  (let [m (g/mix2 p q 0.5)]
    (intersect-circle c (circle m (g/dist2 m p)))))
