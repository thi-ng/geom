(ns thi.ng.geom.aabb
  (:require
    [thi.ng.math.core :as m]
    [thi.ng.geom.core :as g]
    [thi.ng.geom.mesh :as mesh]
    [thi.ng.geom.types :as t]))

(extend-type t/AABB
  g/IShape
    (bounds [this] this)
    (center
      ([{p :p size :size}] (t/AABB. (g/sub3 p (g/scale3 size 0.5)) size))
      ([{size :size} q] (t/AABB. (g/sub3 q (g/scale3 size 0.5)) size)))
    (centroid [{p :p size :size}] (g/add3 p (g/scale3 size 0.5)))
    (classify-point [{p :p size :size} [x y z :as q]]
      (let [[x1 y1 z1] p
            [x2 y2 z2] (g/add3 p size)
            on-plane? (fn [& [minp maxp p min1 max1 min2 max2 c1 c2]]
                        (and (or (m/delta= minp p m/*eps*)
                                 (m/delta= maxp p m/*eps*))
                             (m/in-range? min1 max1 c1)
                             (m/in-range? min2 max2 c2)))]
        (if (some #(apply on-plane? %)
              [[x1 x2 x y1 y2 z1 z2 y z]
               [y1 y2 y x1 x2 z1 z2 x z]
               [z1 z2 z x1 x2 y1 y2 x y]]) 0
          (if (and (m/in-range? x1 x2 x)
                   (m/in-range? y1 y2 y)
                   (m/in-range? z1 z2 z))
            1 -1))))
    (contains-point? [{p :p size :size} [qx qy qz]]
      (let [[x1 y1 z1] p
            [x2 y2 z2] (g/add3 p size)]
        (and (m/in-range? x1 x2 qx)
             (m/in-range? y1 y2 qy)
             (m/in-range? z1 z2 qz))))
    (random-point [{p :p size :size}]
      (let [[x1 y1 z1] p
            [x2 y2 z2] (g/add3 p size)]
        [(m/random x1 x2) (m/random y1 y2) (m/random z1 z2)]))
    (random-boundary-point [{p :p size :size}]
      (let [[x1 y1 z1] p
            [x2 y2 z2] (m/add3 p size)
            id (int (m/random 6))]
        (cond
          (< id 2) (g/vec3 (if (zero? id) x1 x2) (m/random y1 y2) (m/random z1 z2))
          (< id 4) (g/vec3 (m/random x1 x2) (if (= 2 id) y1 y2) (m/random z1 z2))
          :default (g/vec3 (m/random x1 x2) (m/random y1 y2) (if (= 4 id) z1 z2)))))
  g/IShape3
    (as-mesh [{p :p size :size}]
      (let [[x1 y1 z1 :as a] p
            [x2 y2 z2 :as g] (g/add3 p size)
            b [x1 y2 z1] c [x1 y2 z2]
            d [x1 y1 z2] e [x2 y1 z1]
            f [x2 y2 z1] i [x2 y1 z2]]
        (mesh/mesh3
          [a c b] [a d c] ;left
          [d g c] [d i g] ;front
          [i f g] [i e f] ;right
          [e b f] [e a b] ;back
          [c f b] [c g f] ;top
          [e i d] [e d a]))) ; bottom
    (bounding-sphere [{p :p size :size}]
      (let [e (g/scale3 size 0.5)]
        (g/bounding-sphere* (g/add3 p e) (g/mag3 e))))
    (volume [{[w h d] :size}] (* (* w h) d)))

(defn aabb
  ([] (t/AABB. (g/vec3 0.0) (g/vec3 1.0)))
  ([size] (t/AABB. (g/vec3 0.0) (g/vec3 size)))
  ([o size] (t/AABB. (g/vec3 o) (g/vec3 size))))

(defn intersects-aabb?
  "Returns true if the two given boxes intersect/overlap."
  [{pa :p sa :size} {pb :p sb :size}]
  (every? #(and (<= (pa %) (+ (pb %) (sb %)))
                (<= (pb %) (+ (pa %) (sa %))))
          [0 1 2]))
