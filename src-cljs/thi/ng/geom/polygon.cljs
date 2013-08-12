(ns thi.ng.geom.polygon
  (:require
    [thi.ng.data.core :as d]
    [thi.ng.math.core :as m]
    [thi.ng.geom.core :as g]
    [thi.ng.geom.line :as l]
    [thi.ng.geom.triangulate :as tri]
    [thi.ng.geom.triangle :as t]
    [thi.ng.geom.circle :as c]
    [thi.ng.geom.rect :as r]
    [thi.ng.geom.mesh :as mesh]
    [thi.ng.geom.types :as gt]))

(extend-type gt/Polygon
  g/IShape
    (bounds [this] (g/bounding-rect* (:points this)))
    (center
      ([this] (g/center this [0.0 0.0]))
      ([this o]
        (gt/Polygon. (vec (g/translate2 (g/sub2 o (g/centroid this)) (:points this))))))
    (centroid [{points :points :as this}]
      (let [p (first points)
            [c] (reduce
                  (fn [[c p] v]
                    (let [x (g/cross2 p v)]
                      [(g/add2 c (g/scale2 (g/add2 p v) x)) v]))
                  [[0.0 0.0] p] (conj (subvec points 1 (count points)) p))]
        (g/scale2 c (/ 1.0 (* 6 (g/area this))))))
    (classify-point [this p] nil) ; TODO
    (closest-point [this p] (g/closest-point* g/dist2-squared (g/edges this) p))
    (contains-point? [{points :points} p]
      (if (some #{p} points) true
        (let [[x y] p]
          (first
            (reduce
              (fn [[in [px py]] [vx vy]]
                (if (and (or (and (< vy y) (>= py y)) (and (< py y) (>= vy y)))
                             (< (+ vx (* (/ (- y vy) (- py vy)) (- px vx))) x))
                  [(not in) [vx vy]] [in [vx vy]]))
              [false (last points)] points)))))
    (point-at [this t] nil) ; TODO
    (random-point [this] nil) ; TODO
    (random-boundary-point [this] (g/point-at this (m/random)))
  g/IShape2
    (area [{points :points}]
      (let [p (first points)
            [area] (reduce (fn [[a p] v] [(+ a (g/cross2 p v)) v])
                     [0 p] (concat (drop 1 points) [p]))]
      (* 0.5 area)))
    (bounding-circle [this] (g/bounding-circle* (g/centroid this) (:points this)))
    (circumference [{points :points}]
      (d/apply-to-pairs + g/dist2 (concat points [(first points)])))
    (edges [{points :points}]
      (map (fn [[p q]] (gt/Line2. p q))
        (d/successive-nth 2 (concat points [(first points)]))))
    (as-polygon [this] this))

(defn polygon
  ([points] (gt/Polygon. (vec (map g/vec2 points))))
  ([p & more] (gt/Polygon. (vec (map g/vec2 (cons p more))))))

(defn clip-convex
  [poly bounds]
  (let[verts (if (map? poly) (:points poly) (vec poly))
       verts (conj verts (first verts))
       bc (g/centroid bounds)
       ec-pos (fn [e p q] (:p (g/intersect-line e (gt/Line2. p q))))]
    (loop [cedges (g/edges bounds) points verts clipped []]
      (if-let [ce (first cedges)]
        (let [sign (g/classify-point ce bc)
              clipped (reduce
                        (fn [clipped [p q]]
                          (if (= sign (g/classify-point ce p))
                            (if (= sign (g/classify-point ce q))
                              (conj clipped q)
                              (conj clipped (ec-pos ce p q)))
                            (if (= sign (g/classify-point ce q))
                              (conj clipped (ec-pos ce p q) q)
                              clipped)))
                        [] (d/successive-nth 2 points))
              clipped (if (and (pos? (count clipped))
                               (not (m/delta= (first clipped) (peek clipped))))
                        (conj clipped (first clipped))
                        clipped)]
          (recur (rest cedges) clipped points))
        (polygon (butlast points))))))

(defn delta-contains
  [points p eps]
  (some #(m/delta= p % eps) points))

(defn crossed-edge?
  [e a b]
  (let [{t :type ua :ua} (g/intersect-line e {:p a :q b})]
    (and (= :intersect t) (m/in-range? 0.01 0.99 ua))))

(defn tesselate
  [poly]
  (let [m (apply mesh/mesh2 (tri/triangulate (:points poly)))
        assoc-bounds #(assoc! % %2 (g/bounding-rect* %2))
        fbounds (reduce assoc-bounds (transient {}) (:faces m))
        [m] (reduce
          (fn [[m fbounds] e]
            (let [eb (g/bounds e)
                  faces (filter
                    (fn [[a b c :as f]]
                      (and (r/rects-intersect? eb (get fbounds f))
                           (or (crossed-edge? e a b)
                               (crossed-edge? e b c)
                               (crossed-edge? e c a))))
                    (:faces m))
                  [m nf] (mesh/slice-with m e faces)
                  fbounds (reduce assoc-bounds fbounds nf)]
              [m fbounds]))
          [m fbounds] (g/edges poly))]
    (mesh/keep-faces m #(g/contains-point? poly (t/centroid2* %)))))

(defn randompoly
  [n r]
  (let [points (:points (g/as-polygon (c/circle r) n))]
    (polygon (map #(g/scale2 % (m/random 0.5 1.5)) points))))

(defn h-segment
  [verts [px py :as p] pred theta ps]
  (let [[q] (reduce (fn [state [qx qy :as q]]
              (if (pred qy py)
                (let [d (m/abs-diff theta (g/heading2 [(- qx px) (- qy py)]))]
                  (if (< d (state 1)) [q d] state))
                state))
            [nil m/HALF_PI] ps)]
    (if q (recur (conj verts q) q pred theta (d/all-after q ps)) verts)))

(defn convex-hull
  [points]
  (let [[p & more :as ps] (sort-by first (if (map? points) (:points points) points))
        rps (reverse ps)]
    (polygon
      (butlast
        (reduce
          (fn[v [pred t ps]] (h-segment v (peek v) pred t (d/all-after (peek v) ps)))
          [p] [[<= m/THREE_HALVES_PI more] [>= 0.0 more]
               [>= m/HALF_PI rps] [<= m/PI rps]])))))
