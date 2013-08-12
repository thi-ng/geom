(ns thi.ng.geom.triangle
  (:require
   [thi.ng.math.core :as m]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.types :as t]))

(defn centroid2*
  ([a b c] (g/scale2 (g/add2 a b c) m/THIRD))
  ([[a b c]] (g/scale2 (g/add2 a b c) m/THIRD)))

(defn centroid3*
  ([a b c] (g/scale3 (g/add3 a b c) m/THIRD))
  ([[a b c]] (g/scale3 (g/add3 a b c) m/THIRD)))

(defn normal3*
  ([a b c] (g/normalize3 (g/cross3 (g/sub3 b a) (g/sub3 c a))))
  ([[a b c]] (g/normalize3 (g/cross3 (g/sub3 b a) (g/sub3 c a)))))

(defn barycentric*
  [fsub fdot fmag a b c p]
  (let [e0 (fsub c a)
        e1 (fsub b a)
        e2 (fsub p a)
        d00 (fmag e0)
        d01 (fdot e0 e1)
        d02 (fdot e0 e2)
        d11 (fmag e1)
        d12 (fdot e1 e2)
        denom (/ 1.0 (- (* d00 d11) (* d01 d01)))
        u (* denom (- (* d11 d02) (* d01 d12)))
        v (* denom (- (* d00 d12) (* d01 d02)))]
    [(- 1.0 (+ u v)) v u]))

(defn contains-point*
  [fsub fdot fmag a b c p]
  (let [[u v w] (barycentric* fsub fdot fmag a b c p)]
    (and (>= u 0.0) (>= v 0.0) (<= w 1.0))))

(extend-type t/Triangle2
  g/IShape
  (bounds [t] (g/bounding-rect* [(:a t) (:b t) (:c t)]))
  (center
    ([t] (let [ct (g/centroid t)]
           (t/Triangle2.
            (g/sub2 (:a t) ct) (g/sub2 (:b t) ct) (g/sub2 (:c t) ct))))
    ([t o] (let [ct (g/sub2 o (g/centroid t))]
             (t/Triangle2.
              (g/add2 (:a t) ct) (g/add2 (:b t) ct) (g/add2 (:c t) ct)))))
  (centroid [t] (centroid2* (:a t) (:b t) (:c t)))
                                        ; add clockwise? check, currently assumes clockwise ordering
  (classify-point [this p]
    (reduce min (map #(g/classify-point % p) (g/edges this))))
  (closest-point [this p]
    (g/closest-point* g/dist2-squared (g/edges this) p))
  (contains-point? [{a :a b :b c :c} p] (contains-point* g/sub2 g/dot2 g/mag2-squared a b c p))
  (point-at [this t] nil) ; TODO
  (random-point [t]
    (let [b1 (m/random)
          b2 (m/random (- 1.0 b1))
          b3 (- 1.0 (+ b1 b2))]
      (g/from-barycentric*
       g/vec2 g/scale2 g/add2
       [(:a t) (:b t) (:c t)] (shuffle [b1 b2 b3]))))
  (random-boundary-point [this] (g/point-at this (m/random)))
  g/IShape2
  (area [t] (* 0.5 (g/cross2 (g/sub2 (:b t) (:a t)) (g/sub2 (:c t) (:a t)))))
  (bounding-circle [t]
    (g/bounding-circle* (g/centroid t) [(:a t) (:b t) (:c t)]))
  (circumference [{a :a b :b c :c}] (+ (+ (g/dist2 a b) (g/dist2 b c)) (g/dist2 c a)))
  (edges [{a :a b :b c :c}] [(t/Line2. a b) (t/Line2. b c) (t/Line2. c a)])
  (as-polygon [t] (t/Polygon. [(:a t) (:b t) (:c t)])))

(extend-type t/Triangle3
  g/IShape
  (bounds [t] (g/bounding-box* [(:a t) (:b t) (:c t)]))
  (center
    ([t] (let [ct (g/centroid t)]
           (t/Triangle3. (g/sub3 (:a t) ct) (g/sub3 (:b t) ct) (g/sub3 (:c t) ct))))
    ([t o] (let [ct (g/sub3 o (g/centroid t))]
             (t/Triangle3.
              (g/add3 (:a t) ct) (g/add3 (:b t) ct) (g/add3 (:c t) ct)))))
  (centroid [t] (centroid3* (:a t) (:b t) (:c t)))
  (classify-point [this p] nil) ; TODO
  (closest-point [this p] nil) ; TODO
  (contains-point? [{a :a b :b c :c} p] (contains-point* g/sub3 g/dot3 g/mag3-squared a b c p))
  (point-at [this t] nil) ; TODO
  (random-point [t]
    (let [b1 (m/random)
          b2 (m/random (- 1.0 b1))
          b3 (- 1.0 (+ b1 b2))]
      (g/from-barycentric*
       g/vec3 g/scale3 g/add3
       [(:a t) (:b t) (:c t)] (shuffle [b1 b2 b3]))))
  (random-boundary-point [this] (g/point-at this (m/random)))
  g/IShape3
  (bounding-sphere [t] (g/bounding-sphere* (g/centroid t) [(:a t) (:b t) (:c t)]))
  (as-mesh [this] nil))

(defn triangle2
  [a b c] (t/Triangle2. (g/vec2 a) (g/vec2 b) (g/vec2 c)))

(defn triangle3
  [a b c] (t/Triangle3. (g/vec3 a) (g/vec3 b) (g/vec3 c)))

(defn equilateral2
  ([l]
     (cond
      (map? l) (equilateral2 (:a l) (:b l))
      (sequential? l) (let[[a b] l] (equilateral2 a b))
      :default (throw (js/Error. "invalid argument, only map or sequential supported"))))
  ([a b]
     (let[a (g/vec2 a) b (g/vec2 b)
          dir (g/sub2 a b)
          n (g/perpendicular2 dir)
          c (-> n (g/normalize2 (* (g/mag2 dir) (* m/SQRT3 0.5))) (g/add2 (g/mid2 a b)))]
       (triangle2 a b c)))
  ([x1 y1 x2 y2]
     (equilateral2 [x1 y1] [x2 y2])))

                                        ; FIXME
(defn equilateral3
  ([l n]
     {:pre [(or (map? l) (sequential? l)) (g/vec3? n)]}
     (cond
      (map? l) (equilateral3 (:a l) (:b l) n)
      (sequential? l) (let[[a b] l] (equilateral3 a b n))
      :default (throw (js/Error. "invalid argument, only map or sequential supported"))))
  ([a b n]
     (let[a (g/vec3 a) b (g/vec3 b)
          dir (g/sub3 a b)
          n (g/cross3 dir (g/add3 n b))
          c (-> n (g/normalize3 (* (g/mag3 dir) (* m/SQRT3 0.5))) (g/add3 (g/mid3 a b)))]
       (triangle3 a b c)))
  ([x1 y1 z1 x2 y2 z2] (equilateral3 [x1 y1 z1] [x2 y2 z2])))

(defn circumcircle*
  [[ax ay :as a] [bx by :as b] [cx cy :as c]]
  (let [eq-ab? (m/delta= ay by m/*eps*)
        eq-bc? (m/delta= by cy m/*eps*)]
    (when-not (and eq-ab? eq-bc?)
      (let [[ox oy :as o]
            (cond
             eq-ab? (let [m2 (- (/ (- cx bx) (- cy by)))
                          mx1 (* 0.5 (+ ax bx))
                          [mx2 my2] (g/mid2 b c)]
                      [mx1 (+ (* m2 (- mx1 mx2)) my2)])
             eq-bc? (let [m1 (- (/ (- bx ax) (- by ay)))
                          mx2 (* 0.5 (+ bx cx))
                          [mx1 my1] (g/mid2 a b)]
                      [mx2 (+ (* m1 (- mx2 mx1)) my1)])
             :default (let [m1 (- (/ (- bx ax) (- by ay)))
                            m2 (- (/ (- cx bx) (- cy by)))
                            [mx1 my1] (g/mid2 a b)
                            [mx2 my2] (g/mid2 b c)
                            xx (-> (* m1 mx1) (- (* m2 mx2))
                                   (+ my2) (- my1) (/ (- m1 m2)))]
                        [xx (+ (* m1 (- xx mx1)) my1)]))]
        [o (m/hypot (- bx ox) (- by oy))]))))

(defn circumcircle2
  ([t] (circumcircle2 (:a t) (:b t) (:c t)))
  ([a b c]
     (let [[o r] (circumcircle* a b c)] (t/Circle. o r))))

(defn- subdivide*
  [fctor fmid {:keys [a b c] :as tri}]
  (let [ab (fmid a b)
        bc (fmid b c)
        ca (fmid c a)
        ct (g/centroid tri)]
    [(fctor a ab ca) (fctor bc ab b)
     (fctor c ca bc) (fctor ca ab bc)]))

(defn subdiv2
  [tri] (subdivide* (fn[a b c] (t/Triangle2. a b c)) g/mid2 tri))

(defn subdiv3
  [tri] (subdivide* (fn[a b c] (t/Triangle3. a b c)) g/mid3 tri))

(defn check-edge
  [splits classifier e p q add-p? add-q?]
  (let [pc (classifier e p) qc (classifier e q)
        splits (if add-p? (conj splits [p pc]) splits)]
    (if (neg? (* pc qc))
      (let [{ip :p ub :ub} (g/intersect-line e {:p p :q q})]
        (if add-q?
          (conj (conj splits [ip 0]) [q qc])
          (conj splits [ip 0])))
      (if add-q? (conj splits [q qc]) splits))))

(defn slice-with
  ([t e] (slice-with t e g/classify-point))
  ([[a b c] e classifier]
     (let [verts (-> []
                     (check-edge classifier e a b true true)
                     (check-edge classifier e b c false true)
                     (check-edge classifier e c a false false))
           cmap (fn[ids]
                  (reduce
                   (fn[m [[a cl] b c]]
                     (update-in m [cl] conj [a (b 0) (c 0)]))
                   {-1 [] 1 []}
                   (map (fn[[a b c]] [(verts a) (verts b) (verts c)]) ids)))
           corner-index (fn[]
                          (let [triverts #{a b c}]
                            (loop [i (dec (count verts))]
                              (when-let [[v c] (verts i)]
                                (if (and (zero? c) (triverts v)) i (recur (dec i)))))))]
       (condp = (count verts)
         4 (let [d (corner-index)]
             (cmap [[(d/wrap-range (inc d) 4) (d/wrap-range (+ d 2) 4) d]
                    [(d/wrap-range (dec d) 4) d (d/wrap-range (+ d 2) 4)]]))
         5 (if (zero? (get-in verts [1 1]))
             (if (zero? (get-in verts [3 1]))
               (cmap [[0 1 3] [0 3 4] [2 3 1]])
               (cmap [[0 1 4] [2 4 1] [2 3 4]]))
             (cmap [[0 1 2] [0 2 4] [3 4 2]]))
         nil))))

(defn intersect-ray3
  [[a b c] p d]
  (let [n (normal3* a b c)
        nd (g/dot3 n d)]
    (if (neg? nd)
      (let [t (/ (- (g/dot3 n (g/sub3 p a))) nd)]
        (if (>= t m/*eps*)
          (let [ip (g/add3 p (g/scale3 d t))]
            (if (contains-point* g/sub3 g/dot3 g/mag3-squared a b c ip)
              {:p ip :n n :dist t :dir (g/normalize3 (g/sub3 ip p))})))))))
