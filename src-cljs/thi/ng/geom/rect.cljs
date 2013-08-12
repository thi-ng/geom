(ns thi.ng.geom.rect
  (:require
    [thi.ng.math.core :as m]
    [thi.ng.geom.core :as g]
    [thi.ng.geom.types :as t]))

(extend-type t/Rect
  g/IShape
    (bounds [this] this)
    (center
      ([{w :w h :h}]
        (let [w2 (* w 0.5) h2 (* h 0.5)]
          (t/Rect. [(- w2) (- h2)] w h)))
      ([{w :w h :h} o]
        (let [t [(* w 0.5) (* h 0.5)]]
          (t/Rect. (g/sub2 o t) w h))))
    (centroid [{p :p w :w h :h}] (g/mid2 p (g/add2 p [w h])))
    (classify-point [this q]
      (reduce min (map #(g/classify-point % q) (g/edges this))))
    (closest-point [this q]
      (g/closest-point* g/dist2-squared (g/edges this) q))
    (contains-point? [{[px py] :p w :w h :h} [x y]]
      (and (m/in-range? 0 w (- x px)) (m/in-range? 0 h (- y py))))
    (point-at [this t] nil) ; TODO
    (random-point [{p :p w :w h :h}] (g/add2 p [(m/random w) (m/random h)]))
    (random-boundary-point [this] (g/point-at this (m/random)))
  g/IShape2
    (area [this] (* (:w this) (:h this)))
    (bounding-circle [this]
      (let[c (g/centroid this)]
        (g/bounding-circle* c (g/dist2 c (:p this)))))
    (circumference [this] (* 2 (+ (:w this) (:h this))))
    (edges [{[x y :as p] :p w :w h :h}]
      (let [r (+ x w) b (+ y h)]
        [(t/Line2. p [r y]) (t/Line2. [r y] [r b])
         (t/Line2. [r b] [x b]) (t/Line2. [x b] p)]))
    (as-polygon [{[x y :as p] :p w :w h :h}]
      (let [r (+ x w) b (+ y h)] (t/Polygon. [p [r y] [r b] [x b]]))))

(defn rect
  ([] (t/Rect. [0.0 0.0] 1.0 1.0))
  ([w] (t/Rect. [0.0 0.0] w w))
  ([p q]
    (if (vector? p)
      (if (vector? q)
        (let [p (g/vec2 p) q (g/vec2 q)
              [p q] [(g/min2 p q) (g/max2 p q)]
              [w h] (g/sub2 q p)]
          (t/Rect. p w h))
        (t/Rect. (g/vec2 p) q q))
      (t/Rect. [0.0 0.0] p q)))
  ([x y w]
    (if (number? x)
      (t/Rect. (g/vec2 x y) w w)
      (t/Rect. (g/vec2 x) y w)))
  ([x y w h] (t/Rect. (g/vec2 x y) w h)))

(defn union
  [{:keys[p w h]} {q :p qw :w qh :h}]
  (let[[x1 y1] (g/min2 p q)
       x2 (max (+ (p 0) w) (+ (q 0) qw))
       y2 (max (+ (p 1) h) (+ (q 1) qh))
       w (- x2 x1)
       h (- y2 y1)]
    (t/Rect. [x1 y1] w h)))

(defn map-uv
  [{:keys[p w h]} [qx qy]]
  [(/ (- qx (p 0)) w) (/ (- qy (p 1)) h)])

(defn unmap-uv
  [{:keys[p w h]} [qx qy]]
  [(+ (* qx w) (p 0)) (+ (* qy h) (p 1))])

(defn left [{p :p}] (p 0))
(defn right [{p :p w :w}] (+ (p 0) w))
(defn top [{p :p}] (p 1))
(defn bottom [{p :p h :h}] (+ (p 1) h))
(defn bottom-right [{p :p w :w h :h}] (g/add2 p [w h]))

(defn rects-intersect?
  [{[px py] :p w :w h :h} {[qx qy] :p w2 :w h2 :h}]
  (not (or (> px (+ qx w2)) (> qx (+ px w)) (> py (+ qy h2)) (> qy (+ py h)))))

(defn intersect-circle
  [{w :w h :h :as r} {c :p radius :r}]
  (let [p (g/centroid r)
        [dx dy :as d] (g/abs2 (g/sub2 c p))
        w (* w 0.5) h (* h 0.5)]
    (if (and (<= dx (+ w radius))
             (<= dy (+ h radius)))
      (if (or (<= dx w) (<= dy h))
        true
        (<= (g/mag2-squared (g/sub2 d [w h])) (* radius radius)))
      false)))
