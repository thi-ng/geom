(ns thi.ng.geom.line
  (:require
   [thi.ng.math.core :as m]
   [thi.ng.geom.core :as g]
   [thi.ng.data.core :as d]
   [thi.ng.geom.types :as t]))

(defn closest-point-coeff
  [fsub fdot fmag p a b]
  (let [d (fsub b a)] (/ (fdot (fsub p a) d) (fmag d))))

(extend-type t/Line2
  g/IShape
  (bounds [{p :p q :q}] (g/bounding-rect* [p q]))
  (center
    ([{p :p q :q}]
       (let [c (g/mix2 p q 0.5)] (t/Line2. (g/sub2 p c) (g/sub2 q c))))
    ([{p :p q :q} o]
       (let [c (g/sub2 o (g/mix2 p q 0.5))]
         (t/Line2. (g/add2 p c) (g/add2 q c)))))
  (centroid [{p :p q :q}] (g/mix2 p q 0.5))
  (classify-point [{p :p q :q} v]
    (m/signum (g/dot2 (g/sub2 v p) (g/perpendicular2 (g/sub2 q p))) m/*eps*))
  (closest-point [{p :p q :q} a]
    (let [t (closest-point-coeff g/sub2 g/dot2 g/mag2-squared a p q)]
      (cond (neg? t) p (> t 1.0) q :default (g/mix2 p q t))))
  (contains-point? [this p] (= (g/closest-point this p) p))
  (point-at [{p :p q :q} t] (g/mix2 p q t))
  (random-point [{p :p q :q}] (g/mix2 p q (m/random)))
  (random-boundary-point [{p :p q :q}] (g/mix2 p q (m/random)))
  g/IShape2
  (area [this] 0)
  (bounding-circle [{p :p q :q}]
    (g/bounding-circle* (g/mix2 p q 0.5) (* 0.5 (g/dist2 p q))))
  (circumference [{p :p q :q}] (* 2 (g/dist2 p q)))
  (edges [this] [this])
  (as-polygon [this] nil)
  g/IIntersectable
  (intersect-line [{[px1 py1 :as p] :p [qx1 qy1 :as q] :q :as this}
                   {[px2 py2 :as lp] :p [qx2 qy2 :as lq] :q}]
    (let [denom (- (* (- qy2 py2) (- qx1 px1)) (* (- qx2 px2) (- qy1 py1)))
          na (- (* (- qx2 px2) (- py1 py2)) (* (- qy2 py2) (- px1 px2)))
          nb (- (* (- qx1 px1) (- py1 py2)) (* (- qy1 py1) (- px1 px2)))]
      (if-not (zero? denom)
        (let [ua (/ na denom) ub (/ nb denom) ipos (g/mix2 p q ua)]
          (if (and (>= ua 0.0) (<= ua 1.0) (>= ub 0.0) (<= ub 1.0))
            {:type :intersect :p ipos :ua ua :ub ub}
            {:type :intersect-outside :p ipos :ua ua :ub ub}))
        (if (and (zero? na) (zero? nb))
          (let [ip (g/closest-point this lp)
                iq (g/closest-point this lq)]
            (if (or (m/delta= ip lp) (m/delta= iq lq))
              {:type :coincident :p ip :q iq}
              {:type :coincident-no-intersect :p ip :q iq}))
          {:type :parallel})))))

(extend-type t/Line3
  g/IShape
  (bounds [{p :p q :q}] (g/bounding-box* [p q]))
  (center
    ([{p :p q :q}]
       (let [c (g/mix3 p q 0.5)] (t/Line3. (g/sub3 p c) (g/sub3 q c))))
    ([{p :p q :q} o]
       (let [c (g/sub3 o (g/mix3 p q 0.5))]
         (t/Line3. (g/add3 p c) (g/add3 q c)))))
  (centroid [{p :p q :q}] (g/mix3 p q 0.5))
  (classify-point [{p :p q :q} p] nil) ; TODO
  (closest-point [{p :p q :q} a]
    (let [t (closest-point-coeff g/sub3 g/dot3 g/mag3-squared a p q)]
      (cond (neg? t) p (> t 1.0) q :default (g/mix3 p q t))))
  (contains-point? [{p :p q :q} p] nil) ; TODO
  (point-at [{p :p q :q} t] (g/mix3 p q t))
  (random-point [{p :p q :q}] (g/mix3 p q (m/random)))
  (random-boundary-point [{p :p q :q}] (g/mix3 p q (m/random)))
  (as-polygon [this] nil)
  g/IShape3
  (bounding-sphere [{p :p q :q}]
    (g/bounding-sphere* (g/mix3 p q 0.5) (* 0.5 (g/dist3 p q)))))

(defn line2
  ([p q] (t/Line2. (g/vec2 p) (g/vec2 q)))
  ([px py qx qy] (t/Line2. (g/vec2 px py) (g/vec2 qx qy))))

(defn line3
  ([p q] (t/Line3. (g/vec3 p) (g/vec3 q)))
  ([px py qx qy] (t/Line3. (g/vec3 px py) (g/vec3 qx qy)))
  ([px py pz qx qy qz] (t/Line3. (g/vec3 px py pz) (g/vec3 qx qy qz))))

(defn as-linestrip
  [mix a b res include-b?]
  (let [ls (for [t (range 0 1.0 (/ 1.0 res))] (mix a b t))]
    (if include-b? (concat ls [b]) ls)))

(defn arc-length
  [mag points]
  (->> points
       (d/successive-nth 2)
       (map #(mag (% 0) (% 1)))
       (reductions + 0)
       (vec)))

(defn sample-uniform
  [mix mag step include-last? points]
  (let [idx (arc-length mag points)
        total (peek idx)
        delta (/ step total)
        samples (loop [t 0.0 i 1 acc []]
                  (if (< t 1.0)
                    (let [ct (* t total)
                          i (loop [i i] (if (>= ct (idx i)) (recur (inc i)) i))
                          p (nth points (dec i))
                          q (nth points i)
                          pi (idx (dec i))
                          frac (/ (- ct pi) (- (idx i) pi))]
                      (recur (+ t delta) i (conj acc (mix p q frac))))
                    acc))]
    (if include-last?
      (conj samples (last points))
      samples)))
