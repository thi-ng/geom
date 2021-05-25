(ns thi.ng.geom.polygon
  (:require
   [thi.ng.geom.core :as g]
   [thi.ng.geom.utils :as gu]
   [thi.ng.geom.utils.intersect :as isec]
   [thi.ng.geom.utils.probability :as prob]
   [thi.ng.geom.vector :as v :refer [vec2 vec3]]
   [thi.ng.geom.line :as l]
   [thi.ng.geom.triangle :as t]
   [thi.ng.geom.basicmesh :as bm]
   [thi.ng.geom.attribs :as attr]
   #?(:clj [thi.ng.geom.types] :cljs [thi.ng.geom.types :refer [Circle2 Polygon2]])
   [thi.ng.dstruct.core :as d]
   [thi.ng.math.core :as m :refer [PI HALF_PI THREE_HALVES_PI *eps*]])
  #?(:clj (:import [thi.ng.geom.types Circle2 Polygon2])))

(defn polygon2
  ([points] (Polygon2. (mapv vec2 points)))
  ([p q & more] (Polygon2. (mapv vec2 (cons p (cons q more))))))

(defn cog
  [radius teeth profile]
  (-> (Circle2. (vec2) radius)
      (g/vertices (* teeth (count profile)))
      (->> (mapv (fn [p v] (m/* v p)) (cycle profile))
           (Polygon2.))))

;; Sutherland-Hodgeman convex clipping
;; http://en.wikipedia.org/wiki/Sutherland-Hodgman_algorithm

(defn clip-convex*
  [verts bounds]
  (let [verts (conj verts (first verts))
        bc (g/centroid bounds)
        ec-pos (fn [e p q] (get (g/intersect-line e [p q]) :p))]
    (loop [cedges (mapv l/line2 (g/edges bounds)) points verts clipped []]
      (if cedges
        (let [ce (first cedges)
              sign (g/classify-point ce bc)
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
          (recur (next cedges) clipped points))
        (distinct (butlast points))))))

;; Convex hull

(defn- h-segment
  [verts [px py :as p] pred theta ps]
  (let [[q] (reduce
             (fn [state [qx qy :as q]]
               (if (pred qy py)
                 (let [d (m/abs-diff theta (g/heading-xy (vec2 (- qx px) (- qy py))))]
                   (if (< d (state 1)) [q d] state))
                 state))
             [nil HALF_PI] ps)]
    (if q
      (recur (conj verts q) q pred theta (d/all-after q ps))
      verts)))

(defn convex-hull*
  [points]
  (let [[p & more :as ps] (sort-by first points)
        rps (reverse ps)]
    (butlast
     (reduce
      (fn [v [pred t ps]] (h-segment v (peek v) pred t (d/all-after (peek v) ps)))
      [p] [[<= THREE_HALVES_PI more] [>= 0.0 more]
           [>= HALF_PI rps] [<= PI rps]]))))

;; Tesselation

(defn- snip
  [points a b c nv verts]
  (let [[ax ay] a [bx by] b [cx cy] c
        cp (- (* (- bx ax) (- cy ay)) (* (- by ay) (- cx ax)))]
    (when (< m/*eps* cp)
      (not (some #(gu/point-in-triangle2? % a b c)
                 (disj (set (map points (subvec verts 0 nv))) a b c)))))) ;; TODO transducer

(defn tessellate*
  [p]
  (let [[points area] (if (instance? Polygon2 p)
                        [(get p :points) (g/area p)]
                        [(vec p) (g/area (polygon2 p))])
        nv (count points)
        verts (vec (if (pos? area) (range nv) (range (dec nv) -1 -1)))]
    (loop [result [], verts verts, v (dec nv), nv nv, cnt (dec (* 2 nv))]
      ;; (prn :cnt cnt nv verts)
      (if (= nv 2)
        result
        (when (pos? cnt)
          (let [u (if (<= nv v) 0 v)
                v (inc u) v (if (<= nv v) 0 v)
                w (inc v) w (if (<= nv w) 0 w)
                a (points (verts u))
                b (points (verts v))
                c (points (verts w))]
            ;; (prn :uvw u v w)
            (if (snip points a b c nv verts)
              (let [result (conj result [a b c])
                    verts (vec (concat (subvec verts 0 v) (subvec verts (inc v))))
                    nv (dec nv)]
                (recur result verts v nv (* 2 nv)))
              (recur result verts v nv (dec cnt)))))))))

;; http://alienryderflex.com/polygon_inset/
(defn- inset-corner
  [prev curr next d]
  (let [[dx1 dy1 :as d1] (m/- curr prev)
        [dx2 dy2 :as d2] (m/- next curr)
        d1 (m/mag d1) d2 (m/mag d2)]
    (if-not (or (m/delta= 0.0 d1) (m/delta= 0.0 d2))
      (let [i1 (m/* (m/* (vec2 dy1 (- dx1)) (/ d1)) d) ;; TODO avoid double multiply => (/ d d1)
            i2 (m/* (m/* (vec2 dy2 (- dx2)) (/ d2)) d) ;; TODO ditto => (/ d d2)
            c1 (m/+ curr i1), c2 (m/+ curr i2)
            prev (m/+ prev i1), next (m/+ next i2)]
        (if (m/delta= c1 c2)
          c1 (get (isec/intersect-line2-line2? prev c1 c2 next) :p)))
      curr)))

(defn inset-polygon
  "For CW polygons, use positive distance to inset or negative to outset.
  For CCW polygons, use opposite."
  [points d]
  (mapv
   (fn [[p c n]] (inset-corner p c n d))
   (d/successive-nth 3 (d/wrap-seq points [(peek points)] [(first points)]))))

(defn smooth
  [{points :points :as _} amp base-weight]
  (let [pc (g/centroid _)]
    (Polygon2.
     (mapv
      (fn [[p c n]]
        (let [d (m/+ (m/- p c) (m/- n c))
              d (m/madd (m/- c pc) base-weight d)]
          (m/madd d amp c)))
      (d/successive-nth 3 (d/wrap-seq points [(peek points)] [(first points)]))))))

(extend-type Polygon2

  g/IArea
  (area
    [{points :points}]
    (->> points
         (d/rotate-left 1)
         (reduce (fn [[a p] v] [(+ a (m/cross p v)) v]) [0.0 (first points)])
         first
         (* 0.5)))

  g/IBounds
  (bounds [_] (gu/bounding-rect (get _ :points)))
  (width [_] (gu/axis-range 0 (get _ :points)))
  (height [_] (gu/axis-range 1 (get _ :points)))
  (depth [_] 0)

  g/IBoundingCircle
  (bounding-circle
    [_] (gu/bounding-circle (g/centroid _) (get _ :points)))

  g/IBoundary
  (contains-point?
    [{points :points} p]
    (if (some #{p} points) true
        (let [[x y] p]
          (first
           (reduce
            (fn [[in [px py]] [vx vy]]
              (if (and (or (and (< vy y) (>= py y)) (and (< py y) (>= vy y)))
                       (< (+ vx (* (/ (- y vy) (- py vy)) (- px vx))) x))
                [(not in) [vx vy]] [in [vx vy]]))
            [false (last points)] points)))))

  g/ICenter
  (center
    ([_] (Polygon2. (gu/center (vec2) (get _ :points))))
    ([_ o] (Polygon2. (gu/center (g/centroid _) (vec2 o) (get _ :points)))))
  (centroid
    [{points :points :as _}]
    (let [c (->> points
                 (d/rotate-left 1)
                 (reduce
                  (fn [[c p] v] [(m/madd (m/+ p v) (m/cross p v) c) v])
                  [(vec2) (first points)])
                 (first))]
      (m/* c (/ 1.0 (* 6.0 (g/area _))))))

  g/ICircumference
  (circumference
    [{p :points}] (gu/arc-length (conj p (first p))))

  g/IClassify
  (classify-point
    [_ p] nil)

  g/IClip
  (clip-with
    [_ s] (polygon2 (clip-convex* (get _ :points) s)))

  g/IConvexHull
  (convex-hull
    [_] (polygon2 (convex-hull* (get _ :points))))

  g/IExtrude
  (extrude
    [{points :points :as _}
     {:keys [mesh depth offset scale top? bottom?]
      :or {depth 1.0 scale 1.0 top? true bottom? true}}]
    (let [points   (if (neg? (g/area _)) (reverse points) points)
          tpoints  (if (= 1.0 scale)
                     points
                     (get (g/scale-size (polygon2 points) scale) :points))
          off      (or offset (vec3 0 0 depth))
          points3  (mapv vec3 points)
          tpoints3 (mapv #(m/+ off %) tpoints)
          quad?    (== 4 (count points))]
      (g/into
       (or mesh (bm/basic-mesh))
       (concat
        (when bottom?
          (if quad?
            [[(rseq (mapv vec3 points))]]
            (->> points
                 (tessellate*)
                 (map (fn [[a b c]] [[(vec3 b) (vec3 a) (vec3 c)]])))))
        (mapv (fn [[a1 b1] [a2 b2]] [[a1 b1 b2 a2]])
              (d/successive-nth 2 (conj points3 (points3 0)))
              (d/successive-nth 2 (conj tpoints3 (tpoints3 0))))
        (when top?
          (if quad?
            [[tpoints3]]
            (->> tpoints
                 (tessellate*)
                 (mapv (fn [[a b c]] [[(m/+ off a) (m/+ off b) (m/+ off c)]])))))))))
  (extrude-shell
    [{points :points :as _}
     {:keys [mesh depth offset inset top? bottom? wall nump]
      :or {wall 1.0 depth 1.0 inset 0.0 top? false bottom? false}}]
    (let [points     (if (neg? (g/area _)) (reverse points) points)
          tpoints    (if (zero? inset) points (inset-polygon points (- inset)))
          ipoints    (inset-polygon points (- wall))
          itpoints   (inset-polygon points (- (- inset) wall))
          off        (or offset (vec3 0 0 depth))
          ioff       (if bottom? (m/normalize off wall) (vec3))
          itoff      (if top? (m/normalize off (- (m/mag off) wall)) off)
          maxp       (inc (count points))
          quad?      (== 4 (count points))
          nump       (if nump (m/clamp nump 2 maxp) maxp)
          np1        (dec nump)
          complete?  (= nump maxp)
          maybe-loop #(if complete? (conj % (% 0)) (take nump %))
          drop-wrap  #(conj (vec (drop np1 %)) (first %))
          quad-strip (fn [a b flip?]
                       (map
                        (fn [[a1 b1] [a2 b2]]
                          [(if flip? [a1 b1 b2 a2] [a1 a2 b2 b1])])
                        (d/successive-nth 2 a) (d/successive-nth 2 b)))
          points3    (mapv #(vec3 %) points)
          ipoints3   (mapv #(m/+ ioff %) ipoints)
          tpoints3   (mapv #(m/+ off %) tpoints)
          itpoints3  (mapv #(m/+ itoff %) itpoints)
          outsides   (quad-strip (maybe-loop points3) (maybe-loop tpoints3) true)
          insides    (quad-strip (maybe-loop ipoints3) (maybe-loop itpoints3) false)]
      (g/into
       (or mesh (bm/basic-mesh))
       (concat
        (if bottom?
          (concat
           (if quad?
             [[(rseq (mapv vec3 points))]]
             (->> points
                  (tessellate*)
                  (map (fn [[a b c]] [[(vec3 b) (vec3 a) (vec3 c)]]))))
           (if quad?
             [(mapv #(m/+ ioff %) ipoints)]
             (->> ipoints
                  (tessellate*)
                  (map (fn [[a b c]] [[(m/+ ioff a) (m/+ ioff b) (m/+ ioff c)]]))))
           (when-not complete?
             (quad-strip (drop-wrap points3) (drop-wrap ipoints3) true)))
          (quad-strip (maybe-loop points3) (maybe-loop ipoints3) false))
        outsides
        insides
        (when-not complete?
          (let [a (points3 0) b (ipoints3 0) c (itpoints3 0) d (tpoints3 0)
                e (points3 np1) f (ipoints3 np1) g (itpoints3 np1) h (tpoints3 np1)]
            [[[d c b a]] [[e f g h]]]))
        (if top?
          (concat
           (if quad?
             [[(mapv #(m/+ off %) tpoints)]]
             (->> tpoints
                  (tessellate*)
                  (map (fn [[a b c]] [[(m/+ off a) (m/+ off b) (m/+ off c)]]))))
           (if quad?
             [[(rseq (mapv #(m/+ itoff %) itpoints))]]
             (->> itpoints
                  (tessellate*)
                  (map (fn [[a b c]] [[(m/+ itoff b) (m/+ itoff a) (m/+ itoff c)]]))))
           (when-not complete? (quad-strip (drop-wrap tpoints3) (drop-wrap itpoints3) false)))
          (quad-strip (maybe-loop tpoints3) (maybe-loop itpoints3) true))))))

  g/IFlip
  (flip
    [_] (Polygon2. (vec (rseq (:points _)))))

  g/IVertexAccess
  (vertices
    [_] (get _ :points))

  g/IEdgeAccess
  (edges
    [{points :points}]
    (d/successive-nth 2 (conj points (first points))))

  g/IGraph
  (vertex-neighbors
    [_ v] (d/neighbors v (g/vertices _)))
  (vertex-valence
    [_ v] (if ((set (g/vertices _)) v) 2 0))

  g/IIntersect
  (intersect-line
    [_ l])
  (intersect-ray
    [_ r])
  (intersect-shape
    [_ s])

  g/IMeshConvert
  (as-mesh
    ([_] (g/as-mesh _ {}))
    ([_ {:keys [attribs] :as opts}]
     (let [bounds (gu/bounding-rect (get _ :points))]
       (->> (tessellate* _)
            (map-indexed
             (fn [i [a b c]]
               (attr/generate-face-attribs
                [(vec3 a) (vec3 b) (vec3 c)] i attribs
                {:uv [(g/map-point bounds a) (g/map-point bounds b) (g/map-point bounds c)]})))
            (g/into (or (get opts :mesh) (bm/basic-mesh)))))))

  g/IPolygonConvert
  (as-polygon [_] _)

  g/IProximity
  (closest-point
    [_ p]
    (first (gu/closest-point-on-segments p (g/edges _))))

  g/ISample
  (point-at
    [{points :points} t] (gu/point-at t (conj points (first points))))
  (random-point
    [_] (g/point-at _ (m/random)))
  ;; Uniformly sample point from tessellated triangles of polygon
  ;; https://blogs.sas.com/content/iml/2020/10/21/random-points-in-polygon.html
  ;; https://observablehq.com/@scarysize/finding-random-points-in-a-polygon
  (random-point-inside
    [_]
    "Uniformly sample point from inside polygon

    Tessellates into triangles, randomly selects a triangle weighted by area,
    and then samples a point inside of that triangle uniformly."
    (->> (g/tessellate _)
         (map t/triangle2)
         (prob/rand-weighted-by g/area)
         t/random-point-in-triangle2))
  (sample-uniform
    [{points :points} udist include-last?]
    (gu/sample-uniform udist include-last? (conj points (first points))))

  g/ITessellate
  (tessellate
    [_] (tessellate* _))

  g/IRotate
  (rotate
    [_ theta] (Polygon2. (mapv #(g/rotate % theta) (get _ :points))))

  g/IScale
  (scale
    [_ s] (Polygon2. (mapv #(m/* % s) (get _ :points))))
  (scale-size
    [_ s] (Polygon2. (gu/scale-size s (get _ :points))))

  g/ITranslate
  (translate
    [_ t] (Polygon2. (mapv #(m/+ % t) (get _ :points))))

  g/ITransform
  (transform
    [_ m] (Polygon2. (mapv #(g/transform-vector m %) (get _ :points))))

  g/IVolume
  (volume [_] 0.0))
