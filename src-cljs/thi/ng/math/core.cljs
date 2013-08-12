(ns thi.ng.math.core)

(def ^:const PI Math/PI)
(def ^:const TWO_PI (* PI 2))
(def ^:const HALF_PI (/ PI 2))
(def ^:const THIRD_PI (/ PI 3))
(def ^:const QUARTER_PI (/ PI 4))
(def ^:const SIXTH_PI (/ PI 6))
(def ^:const THREE_HALVES_PI (* PI 1.5))

(def ^:const MAX 1.7976931348623157E308)
(def ^:const MIN 4.9E-324)

(def ^:const SQRT2 (Math/sqrt 2))
(def ^:const SQRT3 (Math/sqrt 3))
(def ^:const PHI (/ (inc (Math/sqrt 5.0)) 2.0))

(def ^:const THIRD (/ 1 3.0))

(def ^:const LOG2 (Math/log 2))

(def ^:dynamic *eps* 1e-6)

(defn fma
  [a b c] (+ (* a b) c))

(defn abs
  [x] (if (neg? x) (- x) x))

(defn abs-diff
  [x y] (abs (- x y)))

(defn delta=
  "Compares the absolute difference between a and b and returns true
   if less than delta."
  ([a b] (delta= a b *eps*))
  ([a b delta]
    (if (number? a)
      (if (number? b) (<= (abs (- b a)) delta) false)
      (every? true? (map #(delta= % %2 delta) a b)))))

(defn radians
  [x] (/ (* PI x) 180.0))

(defn degrees
  [x] (/ (* 180.0 x) PI))

(defn mix
  [a b t] (+ (* (- b a) t) a))

(defn map-interval
  "Maps x from one interval into another. Intervals can be defined as vectors."
  ([x [minIn maxIn] [minOut maxOut]]
    (+ (* (- maxOut minOut) (/ (- x minIn) (- maxIn minIn))) minOut))
  ([x minIn maxIn minOut maxOut]
    (+ (* (- maxOut minOut) (/ (- x minIn) (- maxIn minIn))) minOut)))

(defn floor
  [x] (Math/floor x))

(defn ceil
  [x] (Math/ceil x))

(defn roundto
  [x prec] (* (floor (+ (/ x prec) 0.5)) prec))

(defn ceil-pow2
  [x]
  (loop [pow2 1]
    (if (>= pow2 x) pow2 (recur (* pow2 2)))))

(defn floor-pow2
  [x] (int (Math/pow 2 (int (/ (Math/log x) LOG2)))))

(defn trunc
  [x] (if (neg? x) (ceil x) (floor x)))

(defn fract
  [x] (- x (floor x)))

(defn fdim
  [x y] (if (> x y) (- x y) 0))

(defn signum
  ([x] (if (neg? x) -1 (if (zero? x) 0 1)))
  ([x delta]
    (if (delta= 0 x delta) 0
      (if (neg? x) -1 1))))

(defn in-range?
  "Returns true if x >= min and x <= max."
  ([[min max] x]
   (and (>= x min) (<= x max)))
  ([min max x]
   (and (>= x min) (<= x max))))

(defn clamp
  [x min max] (if (< x min) min (if (> x max) max x)))

(defn clamp-normalized
  [x] (clamp x -1.0 1.0))

(defn maxmag
  [x y]
  (let [ax (abs x) ay (abs y)]
    (cond
      (> ax ay) x
      (> ay ax) y
      :default (max x y))))

(defn minmag
  [x y]
  (let [ax (abs x) ay (abs y)]
    (cond
      (< ax ay) x
      (< ay ax) y
      :default (min x y))))

(defn ldexp
  [x k] (* x (Math/pow 2 (int k))))

(defn remquo
  [x y]
  (let [k (/ x y)
        f (fract k)
        k (cond
            (= f 0.5) (if (even? (int k)) (int k) (ceil k))
            (< f 0.5) (floor k)
            :default (ceil k))]
    (- x (* k y))))

(defn hypot
  [x y] (Math/sqrt (+ (* x x) (* y y))))

(defn rootn
  [x y] (Math/pow x (/ 1.0 y)))

(defn rsqrt
  [x] (/ 1.0 (Math/sqrt x)))

(defn step
  [edge x] (if (< x edge) 0.0 1.0))

(defn smoothstep
  [e0 e1 x]
  (let [t (clamp (/ (- x e0) (- e1 e0)) 0.0 1.0)]
    (* t (* t (- 3.0 (* 2.0 t))))))

(defn impulse
  [k t]
  (let [h (* k t)] (* h (Math/exp (- 1.0 h)))))

(defn random
  ([] (Math/random))
  ([max] (* (Math/random) max))
  ([min max] (+ (* (Math/random) (- max min)) min)))

(defn randnorm
  [] (dec (* (Math/random) 2.0)))

(defn wrap-range
  ([x y] (let [x (rem x y)] (if (neg? x) (+ x y) x))))
