(ns thi.ng.geom.bezier
  (:require
   [thi.ng.geom.core :as g :refer [*resolution*]]
   [thi.ng.geom.vector :as v :refer [vec2 vec3]]
   [thi.ng.geom.utils :as gu]
   #?(:clj [thi.ng.geom.types] :cljs [thi.ng.geom.types :refer [Bezier2 Bezier3]])
   [thi.ng.geom.ptf :as ptf]
   [thi.ng.dstruct.core :as d]
   [thi.ng.math.core :as m])
   #?(:clj (:import [thi.ng.geom.types Bezier2 Bezier3])))

(defn bernstein
  [t]
  (let [it (- 1.0 t) it2 (* it it) t2 (* t t)]
    [(* it it2) (* 3 (* t it2)) (* 3 (* it t2)) (* t t2)]))

(defn interpolate
  [[a b c d] t]
  (let [it (- 1.0 t), it2 (* it it), t2 (* t t)]
    (->> (m/* a (* it it2))
         (m/madd b (* 3 (* t it2)))
         (m/madd c (* 3 (* it t2)))
         (m/madd d (* t t2)))))

(defn sample-segment
  [seg res]
  (map #(interpolate seg %) (butlast (m/norm-range res))))

(defn sample-with-res
  [res include-last? points]
  (let [ls (->> points
                (d/successive-nth 4)
                (take-nth 3)
                (mapcat #(sample-segment % res)))]
    (if include-last?
      (concat ls [(last points)])
      ls)))

;; *** Automatic curve generation
;;
;; The following two functions allow us to compute a bezier curve which
;; passes through all given points and automatically computes the
;; required control points. This only works for non-closed curves, though.

(defn- find-cpoints*
  [ctor tight points]
  (let [np (count points)
        invt (/ 1.0 tight)
        points (vec points)
        c1 (m/subm (points 2) (first points) tight)
        [bi coeff] (reduce
                    (fn [[bi coeff] i]
                      (let [b (/ -1.0 (+ invt (peek bi)))
                            c (peek coeff)
                            p (get points (dec i))
                            q (get points (inc i))]
                        [(conj bi b)
                         (conj coeff (m/* (m/- q p c) (- b)))]))
                    [[0 (- tight)] [(ctor) c1]]
                    (range 2 (dec np)))]
    (reduce
     (fn [delta i]
       (assoc delta i (m/madd (delta (inc i)) (bi i) (coeff i))))
     (vec (repeatedly np ctor))
     (range (- np 2) 0 -1))))

(defn auto-spline*
  [points cpoints]
  (concat
   (->> cpoints
        (d/successive-nth 2)
        (interleave (d/successive-nth 2 points))
        (partition 2)
        (mapcat (fn [[[p q] [dp dq]]] [p (m/+ p dp) (m/- q dq)])))
   [(last points)]))

;; ** Constructors

(defn bezier2
  [points] (Bezier2. (mapv vec2 points)))

(defn auto-spline2
  ([points]
   (->> points
        (find-cpoints* vec2 0.25)
        (auto-spline* points)
        (Bezier2.)))
  ([points closed?]
   (auto-spline2
    (if closed?
      (conj (vec points) (first points))
      points))))

(defn bezier3
  [points] (Bezier3. (mapv vec3 points)))

(defn auto-spline3
  ([points]
   (->> points
        (find-cpoints* vec3 0.25)
        (auto-spline* points)
        (Bezier3.)))
  ([points closed?]
   (auto-spline3
    (if closed?
      (conj (vec points) (first points))
      points))))

;; ** Type implementations

(extend-type Bezier2
  g/IFlip
  (flip [_]
    (Bezier2. (reverse (get _ :points))))

  g/IVertexAccess
  (vertices
    ([_] (g/vertices _ *resolution*))
    ([_ res] (sample-with-res res true (get _ :points))))

  g/IEdgeAccess
  (edges
    ([_] (d/successive-nth 2 (g/vertices _ *resolution*)))
    ([_ res] (d/successive-nth 2 (g/vertices _ res))))

  g/IGraph
  (vertex-neighbors
    [_ v] (d/neighbors v (g/vertices _)))
  (vertex-valence
    [_ v]
    (let [points (g/vertices _)]
      (if-let [p (d/neighbors v points)]
        (if (or (m/delta= p (first points)) (m/delta= p (peek points)))
          1 2)
        0)))

  g/IMeshConvert
  (as-mesh
    [_ {:keys [res dist profile] :as opts}]
    (let [points (if dist
                   (g/sample-uniform _ dist true)
                   (g/vertices _ (or res *resolution*)))]
      (ptf/sweep-mesh (map vec3 points) profile opts)))

  g/IProximity
  (closest-point
    [_ p]
    (first (gu/closest-point-on-segments p (g/edges _))))

  g/ISample
  (point-at
    [_ t] (gu/point-at t (get _ :points) nil))
  (random-point
    [_] (gu/point-at (m/random) (get _ :points) nil))
  (random-point-inside
    [_] (g/random-point _))
  (sample-uniform
    [_ udist include-last?]
    (gu/sample-uniform udist include-last? (g/vertices _))))

(extend-type Bezier3
  g/IFlip
  (flip [_]
    (Bezier3. (reverse (get _ :points))))

  g/IVertexAccess
  (vertices
    ([_] (g/vertices _ *resolution*))
    ([_ res] (sample-with-res res true (get _ :points))))

  g/IEdgeAccess
  (edges
    ([_] (d/successive-nth 2 (g/vertices _ *resolution*)))
    ([_ res] (d/successive-nth 2 (g/vertices _ res))))

  g/IGraph
  (vertex-neighbors
    [_ v] (d/neighbors v (g/vertices _)))
  (vertex-valence
    [_ v]
    (let [points (g/vertices _)]
      (if-let [p (d/neighbors v points)]
        (if (or (m/delta= p (first points)) (m/delta= p (peek points)))
          1 2)
        0)))

  g/IMeshConvert
  (as-mesh
    [_ {:keys [res dist profile] :as opts}]
    (let [points (if dist
                   (g/sample-uniform _ dist true)
                   (g/vertices _ (or res *resolution*)))]
      (ptf/sweep-mesh points profile opts)))

  g/IProximity
  (closest-point
    [_ p]
    (first (gu/closest-point-on-segments p (g/edges _))))

  g/ISample
  (point-at
    [_ t] (gu/point-at t (get _ :points) nil))
  (random-point
    [_] (gu/point-at (m/random) (get _ :points) nil))
  (random-point-inside
    [_] (g/random-point _))
  (sample-uniform
    [_ udist include-last?]
    (gu/sample-uniform udist include-last? (g/vertices _))))
