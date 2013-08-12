(ns thi.ng.geom.core
  (:require
   [thi.ng.math.core :as m]
   [thi.ng.geom.types :as gt]))

(defprotocol IShape
  (bounds [this])
  (center [this] [this o])
  (centroid [this])
  (classify-point [this p])
  (closest-point [this p])
  (contains-point? [this p])
  (point-at [this t])
  (random-point [this])
  (random-boundary-point [this]))

(defprotocol IShape2
  (area [this])
  (as-polygon [this] [this res])
  (bounding-circle [this])
  (circumference [this])
  (edges [this] [this res]))

(defprotocol IShape3
  (as-mesh [this] [this res])
  (bounding-sphere [this])
  (volume [this]))

(defprotocol IIntersectable
  (intersect-line [this l])
  (intersect-ray [this r]))

(defprotocol IRotatable
  (rotate [this t])
  (rotate-x [this t])
  (rotate-y [this t])
  (rotate-z [this t])
  (rotate-axis [this a t]))

(def ^:dynamic *resolution* 20)

(def ^:dynamic *vecindex*
  {\x 0 \y 1 \z 2 \w 3
   \0 0 \1 1 \2 2 \3 3 \4 4 \5 5 \6 6 \7 7
   \8 8 \9 9 \a 10 \b 11 \c 12 \d 13 \e 14 \f 15})

(defn type-error
  [t x] (assert false (str "can't create " t " from " (type x))))

(defn >>
  ([v keys] (>> v keys {} *vecindex*))
  ([v keys defaults] (>> v keys defaults *vecindex*))
  ([v keys defaults index]
     (let [kn (name keys)
           ck (count kn)
           [v acc] (cond
                    (sequential? v)
                    [(if (vector? v) v (vec v))
                     #(or (get %1 (get index %2)) (get defaults %2 0.0))]
                    (map? v)
                    [v #(or (get %1 %2) (get defaults %2 0.0))]
                    :default
                    (throw (js/Error. (str "can't swizzle " v))))]
       (condp = ck
         1 (acc v (nth kn 0))
         2 [(acc v (nth kn 0)) (acc v (nth kn 1))]
         3 [(acc v (nth kn 0)) (acc v (nth kn 1)) (acc v (nth kn 2))]
         4 [(acc v (nth kn 0)) (acc v (nth kn 1)) (acc v (nth kn 2)) (acc v (nth kn 3))]
         (vec (map #(acc v %) kn))))))

(defn <<
  [v ks vs]
  (let [kn (name ks)]
    (reduce
     (if (map? v)
       (fn [v kv] (assoc v (keyword (str (kv 0))) (kv 1)))
       (fn [v kv] (assoc v (get *vecindex* (kv 0)) (kv 1))))
     v (zipmap kn vs))))

(defn assocv
  "Similar to assoc, but transforms keys into numeric indices by looking
  them up in `*vecindex*` first. Thus allows to refer to vector
  elements by keywords. Referring to non-existing elements will throw
  IndexOutOfBoundsException.

      (assocv [10 20] :x 23); => [23 20]
      (assocv [10 20] :x 23 :y 42); => [23 42]
      (assocv [10 20] :z 30); => error"
  ([v k nv] (assoc v (get *vecindex* (nth (name k) 0)) nv))
  ([v k1 v1 k2 v2]
     (-> v
         (assoc (get *vecindex* (nth (name k1) 0)) v1)
         (assoc (get *vecindex* (nth (name k2) 0)) v2)))
  ([v k1 v1 k2 v2 k3 v3]
     (-> v
         (assoc (get *vecindex* (nth (name k1) 0)) v1)
         (assoc (get *vecindex* (nth (name k2) 0)) v2)
         (assoc (get *vecindex* (nth (name k3) 0)) v3)))
  ([v k1 v1 k2 v2 k3 v3 & more]
     (reduce (fn [v [k nv]] (assoc! v (get *vecindex* (nth (name k) 0)) nv))
             v (apply hash-map k1 v1 k2 v2 k3 v3 more))))

(defn build-vec*
  [len defaults & args]
  (let [[v args] (if (every? number? args)
                   [(vec args) []] [[] args])]
    (loop [v v args args]
      (if-let [a (first args)]
        (cond
         (number? a) (recur (conj v a) (rest args))
         (sequential? a) (recur (vec (concat v a)) (rest args))
         :default (type-error (str "vec" len) a))
        (let [c (count v)]
          (assert (<= c len) (str "expected max. " len " coordinates, but got " c))
          (if (= c len) v (vec (concat v (drop c defaults)))))))))

(defn vec2
  ([] [0.0 0.0])
  ([x] (cond
        (sequential? x)
        (if (= 2 (count x))
          (if (vector? x) x (vec x))
          [(nth x 0 0.0) (nth x 1 0.0)])
        (map? x) [(get x :x 0.0) (get x :y 0.0)]
        (number? x) [x x]
        :default (type-error "vec2" x)))
  ([x y] [x y]))

(defn vec3
  ([] [0.0 0.0 0.0])
  ([x] (cond
        (sequential? x)
        (if (= 3 (count x))
          (if (vector? x) x (vec x))
          [(nth x 0 0.0) (nth x 1 0.0) (nth x 2 0.0)])
        (map? x) [(get x :x 0.0) (get x :y 0.0) (get x :z 0.0)]
        (number? x) [x x x]
        :default (type-error "vec3" x)))
  ([x & more] (apply build-vec* 3 [0.0 0.0 0.0] x more)))

(defn vec4
  ([] [0.0 0.0 0.0 1.0])
  ([x] (cond
        (sequential? x)
        (if (= 4 (count x))
          (if (vector? x) x (vec x))
          [(nth x 0 0.0) (nth x 1 0.0) (nth x 2 0.0) (nth x 3 1.0)])
        (map? x) [(get x :x 0.0) (get x :y 0.0) (get x :z 0.0) (get x :w 1.0)]
        (number? x) [x x x 1.0]
        :default (type-error "vec4" x)))
  ([x & more] (apply build-vec* 4 [0.0 0.0 0.0 1.0] x more)))

(defn vec2? [x] (and (sequential? x) (>= (count x) 2)))
(defn vec3? [x] (and (sequential? x) (>= (count x) 3)))
(defn vec4? [x] (and (sequential? x) (>= (count x) 4)))

(defn abs2
  [[x y]] [(m/abs x) (m/abs y)])

(defn abs3
  [[x y z]] [(m/abs x) (m/abs y) (m/abs z)])

(defn abs4
  [[x y z w]] [(m/abs x) (m/abs y) (m/abs z) (m/abs w)])

(defn add2
  ([[px py] [qx qy]] [(+ px qx) (+ py qy)])
  ([[px py] [qx qy] [rx ry]] [(+ (+ px qx) rx) (+ (+ py qy) ry)])
  ([p q r & more] (vec2 (apply map + p q r more))))

(defn add3
  ([[px py pz] [qx qy qz]]
     [(+ px qx) (+ py qy) (+ pz qz)])
  ([[px py pz] [qx qy qz] [rx ry rz]]
     [(+ (+ px qx) rx) (+ (+ py qy) ry) (+ (+ pz qz) rz)])
  ([p q r & more] (vec3 (apply map + p q r more))))

(defn add4
  ([[px py pz pw] [qx qy qz qw]]
     [(+ px qx) (+ py qy) (+ pz qz) (+ pw qw)])
  ([[px py pz pw] [qx qy qz qw] [rx ry rz rw]]
     [(+ (+ px qx) rx) (+ (+ py qy) ry) (+ (+ pz qz) rz) (+ (+ pw qw) rw)])
  ([p q r & more] (vec4 (apply map + p q r more))))

(defn sub2
  ([[px py] [qx qy]] [(- px qx) (- py qy)])
  ([[px py] [qx qy] [rx ry]] [(- (- px qx) rx) (- (- py qy) ry)])
  ([p q r & more] (vec2 (apply map - p q r more))))

(defn sub3
  ([[px py pz] [qx qy qz]]
     [(- px qx) (- py qy) (- pz qz)])
  ([[px py pz] [qx qy qz] [rx ry rz]]
     [(- (- px qx) rx) (- (- py qy) ry) (- (- pz qz) rz)])
  ([p q r & more] (vec3 (apply map - p q r more))))

(defn sub4
  ([[px py pz pw] [qx qy qz qw]]
     [(- px qx) (- py qy) (- pz qz) (- pw qw)])
  ([[px py pz pw] [qx qy qz qw] [rx ry rz rw]]
     [(- (- px qx) rx) (- (- py qy) ry) (- (- pz qz) rz) (- (- pw qw) rw)])
  ([p q r & more] (vec4 (apply map - p q r more))))

(defn scale2
  ([[px py] v] (let [[sx sy] (vec2 v)] [(* px sx) (* py sy)]))
  ([[px py] sx sy] [(* px sx) (* py sy)]))

(defn scale3
  ([[px py pz] v] (let [[sx sy sz] (vec3 v)] [(* px sx) (* py sy) (* pz sz)]))
  ([[px py pz] sx sy sz] [(* px sx) (* py sy) (* pz sz)]))

(defn scale4
  ([[px py pz pw] v] (let [[sx sy sz sw] (vec4 v)] [(* px sx) (* py sy) (* pz sz) (* pw sw)]))
  ([[px py pz pw] sx sy sz sw] [(* px sx) (* py sy) (* pz sz) (* pw sw)]))

(defn fma2 [a b c] (add2 (scale2 a b) c))

(defn fma3 [a b c] (add3 (scale3 a b) c))

(defn fma4 [a b c] (add4 (scale4 a b) c))

(defn invert2 [[x y]] [(- x) (- y)])

(defn invert3 [[x y z]] [(- x) (- y) (- z)])

(defn invert4 [[x y z w]] [(- x) (- y) (- z) (- w)])

(defn dot2
  ([[px py] [qx qy]] (+ (* px qx) (* py qy)))
  ([px py qx qy] (+ (* px qx) (* py qy))))

(defn dot3
  ([[px py pz] [qx qy qz]] (+ (+ (* px qx) (* py qy)) (* pz qz)))
  ([px py pz qx qy qz] (+ (+ (* px qx) (* py qy)) (* pz qz))))

(defn dot4
  ([[px py pz pw] [qx qy qz qw]] (+ (+ (+ (* px qx) (* py qy)) (* pz qz)) (* pw qw)))
  ([px py pz pw qx qy qz qw] (+ (+ (+ (* px qx) (* py qy)) (* pz qz)) (* pw qw))))

(defn cross2
  [[px py] [qx qy]] (- (* px qy) (* py qx)))

(defn cross3
  [[px py pz] [qx qy qz]]
  [(- (* py qz) (* qy pz)) (- (* pz qx) (* qz px)) (- (* px qy) (* qx py))])

(defn mag2-squared
  ([[x y]] (+ (* x x) (* y y)))
  ([x y] (+ (* x x) (* y y))))

(defn mag3-squared
  ([[x y z]] (+ (+ (* x x) (* y y)) (* z z)))
  ([x y z] (+ (+ (* x x) (* y y)) (* z z))))

(defn mag4-squared
  ([[x y z w]] (+ (+ (+ (* x x) (* y y)) (* z z)) (* w w)))
  ([x y z w] (+ (+ (+ (* x x) (* y y)) (* z z)) (* w w))))

(defn mag2
  ([v] (Math/sqrt (mag2-squared v)))
  ([x y] (Math/sqrt (mag2-squared x y))))

(defn mag3
  ([v] (Math/sqrt (mag3-squared v)))
  ([x y z] (Math/sqrt (mag3-squared x y z))))

(defn mag4
  ([v] (Math/sqrt (mag4-squared v)))
  ([x y z w] (Math/sqrt (mag4-squared x y z w))))

(defn normalize2
  ([[x y :as p]] (let [m (mag2 p)] (if (pos? m) [(/ x m) (/ y m)] p)))
  ([[x y :as p] n]
     (let [m (mag2 p)]
       (if (pos? m) (let [m (/ n m)] [(* x m) (* y m)]) p))))

(defn normalize3
  ([[x y z :as p]] (let [m (mag3 p) [x y z] p] (if (pos? m) [(/ x m) (/ y m) (/ z m)] p)))
  ([[x y z :as p] n]
     (let [m (mag3 p)]
       (if (pos? m) (let [m (/ n m)] [(* x m) (* y m) (* z m)]) p))))

(defn normalize4
  ([[x y z w :as p]] (let [m (mag4 p)] (if (pos? m) [(/ x m) (/ y m) (/ z m) (/ w m)] p)))
  ([[x y z w :as p] n]
     (let [m (mag4 p)]
       (if (pos? m) (let [m (/ n m)] [(* x m) (* y m) (* z m) (* w m)]) p))))

(defn dist2
  ([p q] (mag2 (sub2 p q)))
  ([px py qx qy] (mag2 [(- px qx) (- py qy)])))

(defn dist3
  ([p q] (mag3 (sub3 p q)))
  ([px py pz qx qy qz] (mag3 [(- px qx) (- py qy) (- pz qz)])))

(defn dist4
  ([p q] (mag4 (sub4 p q)))
  ([px py pz pw qx qy qz qw] (mag4 [(- px qx) (- py qy) (- pz qz) (- pw qw)])))

(defn dist2-squared
  ([p q] (mag2-squared (sub2 p q)))
  ([px py qx qy] (mag2-squared [(- px qx) (- py qy)])))

(defn dist3-squared
  ([p q] (mag3-squared (sub3 p q)))
  ([px py pz qx qy qz] (mag3-squared [(- px qx) (- py qy) (- pz qz)])))

(defn dist4-squared
  ([p q] (mag4-squared (sub4 p q)))
  ([px py pz pw qx qy qz qw] (mag4-squared [(- px qx) (- py qy) (- pz qz) (- pw qw)])))

(defn limit2
  [v len]
  (if (> (mag2-squared v) (* len len)) (normalize2 v len) v))

(defn limit3
  [v len]
  (if (> (mag3-squared v) (* len len)) (normalize3 v len) v))

(defn limit4
  [v len]
  (if (> (mag4-squared v) (* len len)) (normalize4 v len) v))

(defn randvec2
  ([] (normalize2 [(m/randnorm) (m/randnorm)]))
  ([n] (normalize2 [(m/randnorm) (m/randnorm)] n)))

(defn randvec3
  ([] (normalize3 [(m/randnorm) (m/randnorm) (m/randnorm)]))
  ([n] (normalize3 [(m/randnorm) (m/randnorm) (m/randnorm)] n)))

(defn randvec4
  ([] (normalize4 [(m/randnorm) (m/randnorm) (m/randnorm) (m/randnorm)]))
  ([n] (normalize4 [(m/randnorm) (m/randnorm) (m/randnorm) (m/randnorm)] n)))

(defn mid2
  [[px py] [qx qy]] [(* 0.5 (+ px qx)) (* 0.5 (+ py qy))])

(defn mid3
  [[px py pz] [qx qy qz]]
  [(* 0.5 (+ px qx)) (* 0.5 (+ py qy)) (* 0.5 (+ pz qz))])

(defn mid4
  [[px py pz pw] [qx qy qz qw]]
  [(* 0.5 (+ px qx)) (* 0.5 (+ py qy)) (* 0.5 (+ pz qz)) (* 0.5 (+ pw qw))])

(defn mix2
  ([[px py] [qx qy] t] [(m/mix px qx t) (m/mix py qy t)])
  ([px py qx qy t] [(m/mix px qx t) (m/mix py qy t)]))

(defn mix3
  ([[px py pz] [qx qy qz] t]
     [(m/mix px qx t) (m/mix py qy t) (m/mix pz qz t)])
  ([px py pz qx qy qz t]
     [(m/mix px qx t) (m/mix py qy t) (m/mix pz qz t)]))

(defn mix4
  ([[px py pz pw] [qx qy qz qw] t]
     [(m/mix px qx t) (m/mix py qy t) (m/mix pz qz t) (m/mix pw qw t)])
  ([px py pz pw qx qy qz qw t]
     [(m/mix px qx t) (m/mix py qy t) (m/mix pz qz t) (m/mix pw qw t)]))

(defn min2
  ([[px py] [qx qy]] [(min px qx) (min py qy)])
  ([[px py] [qx qy] [rx ry]]
     [(min (min px qx) rx) (min (min py qy) ry)])
  ([a b c & more] (vec2 (apply map min a b c more))))

(defn min3
  ([[px py pz] [qx qy qz]] [(min px qx) (min py qy) (min pz qz)])
  ([[px py pz] [qx qy qz] [rx ry rz]]
     [(min (min px qx) rx) (min (min py qy) ry) (min (min pz qz) rz)])
  ([a b c & more] (vec3 (apply map min a b c more))))

(defn min4
  ([[px py pz pw] [qx qy qz qw]] [(min px qx) (min py qy) (min pz qz) (min pw qw)])
  ([[px py pz pw] [qx qy qz qw] [rx ry rz rw]]
     [(min (min px qx) rx) (min (min py qy) ry) (min (min pz qz) rz) (min (min pw qw) rw)])
  ([a b c & more] (vec3 (apply map min a b c more))))

(defn max2
  ([[px py] [qx qy]] [(max px qx) (max py qy)])
  ([[px py] [qx qy] [rx ry]]
     [(max (max px qx) rx) (max (max py qy) ry)])
  ([a b c & more] (vec2 (apply map max a b c more))))

(defn max3
  ([[px py pz] [qx qy qz]] [(max px qx) (max py qy) (max pz qz)])
  ([[px py pz] [qx qy qz] [rx ry rz]]
     [(max (max px qx) rx) (max (max py qy) ry) (max (max pz qz) rz)])
  ([a b c & more] (vec3 (apply map max a b c more))))

(defn max4
  ([[px py pz pw] [qx qy qz qw]] [(max px qx) (max py qy) (max pz qz) (max pw qw)])
  ([[px py pz pw] [qx qy qz qw] [rx ry rz rw]]
     [(max (max px qx) rx) (max (max py qy) ry) (max (max pz qz) rz) (max (max pw qw) rw)])
  ([a b c & more] (vec4 (apply map max a b c more))))

(defn translate2
  ([t points] (let [t (vec2 t)] (map #(add2 % t) points)))
  ([t p & more] (apply translate2 t p more)))

(defn translate3
  ([t points] (let [t (vec3 t)] (map #(add3 % t) points)))
  ([t p & more] (apply translate3 t p more)))

(defn translate4
  ([t points] (let [t (vec4 t)] (map #(add4 % t) points)))
  ([t p & more] (apply translate4 t p more)))

(defn rotate2
  [[x y] theta]
  (let [c (Math/cos theta) s (Math/sin theta)]
    [(- (* x c) (* y s)) (+ (* x s) (* y c))]))

(defn perpendicular2
  [[x y]] [(- y) x])

(defn heading2
  [[x y]]
  (let [t (Math/atan2 y x)]
    (if (neg? t) (+ m/TWO_PI t) t)))

(defn slope2
  [[x y]] (/ y x))

(defn angle-between2
  [p q]
  (let [t (- (heading2 q) (heading2 p))]
    (if (neg? t) (+ m/TWO_PI t) t)))

(defn reflect2
  [[x y :as v] [rx ry :as r]]
  (let [d (* (dot2 v r) 2.0)]
    [(- (* rx d) x) (- (* ry d) y)]))

(defn reflect3
  [[x y z :as v] [rx ry rz :as r]]
  (let [d (* (dot3 v r) 2.0)]
    [(- (* rx d) x) (- (* ry d) y) (- (* rz d) z)]))

(defn polar2
  [v] [(mag2 v) (heading2 v)])

(defn cartesian2
  [[r t]] [(* r (Math/cos t)) (* r (Math/sin t))])

(defn cartesian3
  [[x y z]]
  (let [a (* x (Math/cos z))]
    [(* a (Math/cos y)) (* x (Math/sin z)) (* a (Math/sin y))]))

(defn spherical3
  [[x y z]]
  (let [xx (if (m/delta= 0.0 (m/abs x))
             (if (< x 0.0) (- m/*eps*) m/*eps*) x)
        m (mag3 [xx y z])
        yy (+ (Math/atan (/ z xx)) (if (< xx 0.0) m/PI 0))
        zz (Math/asin (/ y m))]
    [m yy zz]))

(defn bisect2
  [p q]
  (let [diff (sub2 p q)
        dotp (dot2 diff (add2 p q))]
    (>> diff :xyz {:z (* dotp -0.5)})))

(defn closest-point*
  [fdist lines q]
  (reduce (fn [a b]
            (if (< (fdist q a) (fdist q b)) a b))
          (map #(closest-point % q) lines)))

(defn from-barycentric*
  [fctor fscale fadd points weights]
  (apply fadd (map (fn [p w] (fscale (fctor p) w)) points weights)))

(defn bounding-rect*
  ([coll]
     (let [p (apply min2 coll)
           q (apply max2 coll)
           [w h] (sub2 q p)]
       (gt/Rect. p w h)))
  ([p w h] (gt/Rect. p w h)))

(defn bounding-circle*
  [c r-or-points]
  (gt/Circle. c
           (if (number? r-or-points) r-or-points
               (Math/sqrt
                (reduce max
                        (map (comp mag2-squared (partial sub2 c))
                             r-or-points))))))

(defn bounding-box*
  ([coll]
     (let [cnt (count coll)]
       (cond
        (> cnt 1)
        (let [p (apply min3 coll)
              q (apply max3 coll)]
          (gt/AABB. p (sub3 q p)))
        (= cnt 1)
        (gt/AABB. (first coll) [0 0 0])
        :default nil)))
  ([p q] (gt/AABB. p (sub3 q p))))

(defn bounding-sphere*
  [p r-or-points]
  (gt/Sphere. p
           (if (number? r-or-points) r-or-points
               (Math/sqrt
                (reduce max
                        (map (comp mag3-squared (partial sub3 p))
                             r-or-points))))))
