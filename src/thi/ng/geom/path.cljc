(ns thi.ng.geom.path
  (:require
   [thi.ng.math.core :as m]
   [thi.ng.geom.core :as g :refer [*resolution*]]
   [thi.ng.geom.vector :as v :refer [vec2]]
   #?(:clj [thi.ng.geom.types] :cljs [thi.ng.geom.types :refer [Path2]])
   [thi.ng.geom.utils :as gu]
   [thi.ng.geom.bezier :as b]
   [thi.ng.xerror.core :as err]
   #?(:clj [clojure.xml :as xml]))
  #?(:clj (:import [thi.ng.geom.types Path2])))

(defn path2
  ([segments]
   (Path2. (vec segments)))
  ([s1 s2 & segments]
   (Path2. (vec (cons s1 (cons s2 segments))))))

(defmulti sample-segment (fn [s res last?] (get s :type)))

(defmethod sample-segment :line
  [{[a b] :points} res last?]
  (gu/sample-segment-with-res a b res last?))

(defmethod sample-segment :close
  [{[a b] :points} res last?]
  (gu/sample-segment-with-res a b res last?))

(defmethod sample-segment :bezier
  [{points :points} res last?]
  (b/sample-with-res res last? points))

(defn sample-segments*
  [res segments]
  (let [last (last segments)
        [paths curr] (reduce
                      (fn [[paths curr] seg]
                        (let [curr (concat curr (sample-segment seg res (= seg last)))]
                          (if (= :close (get seg :type))
                            [(conj paths curr) []]
                            [paths curr])))
                      [[] []] segments)]
    (if (seq curr)
      (conj paths curr)
      paths)))

(def coordinate-regex #"[\-\+]?[0-9]+\.?[0-9]*|\.[0-9]+")


(defn parse-svg-coords
  [coords]
  (->> coords
       (re-seq coordinate-regex)
       #?(:clj (mapv #(Double/parseDouble %)) :cljs (map js/parseFloat))))

(defn svg-coord-pairs [parsed-coords]
  (mapv vec2 (partition 2 parsed-coords)))

;; the general parsing strategy is designed to line up with the intended output:
;; a sequence of segments.
;; 1. use command regex to generate a partially parsed sequence of commands
;; 2. loop through this sequence while holding on to some "current position"
;;    state information to ensure the segments begin and end correctly



;; regex to separate by command indicators; supporting an arbitrary number of
;; coordinate pairs

;; used to generate the sequence of path commands
(def ^:private cmd-regex #"(?i)([achlmqstvz])([^achlmqstvz]*)")

(comment
  (parse-svg-coords "40,40")

  (parse-svg-path
   "M 10,80 20,20 40,40 0,10 Z")

  (re-seq cmd-regex "M 10,80 20,20 40,40 0,10 Z")

  )

(defn move-to [cmd current-pos [start-pt & line-coords]]
  ;; implicit line: return the segment and the current position
  ;; (as described by the final point in the line)
  (if line-coords
    (let [line (reduce (fn [pts nxt] (conj pts (vec2 nxt)))
                       [start-pt]
                       line-coords)]
      [{:type :line :points line}
       (peek line)])
    ;; standard move: return only the current position
    [nil start-pt]))

(defn line-to [cmd current-pos pts]
  (let [rel (= "l" cmd)]
    (if (not= 1 (count pts))
      [{:type :line-string :points (reduce conj [current-pos] pts)
        :relative? rel}
       (peek pts)]
      [{:type :line :points [current-pos (first pts)]
        :relative? rel}
       (first pts)])))

(defn h-line-to [cmd [cx cy :as current-pos] [next-x & xs]]
  (let [rel (= "h" cmd)]
    (if xs
      [{:type :line-string
        :points (reduce (fn [pts x] (conj pts (vec2 x cy)))
                        [current-pos (vec2 next-x cy)]
                        xs)
        :relative? rel}
       (vec2 (peek xs) cy)]
      [{:type :line :points [current-pos (vec2 next-x cy)]
        :relative? rel}
       (vec2 next-x cy)])))

(defn v-line-to [cmd [cx cy :as current-pos] [next-y & ys]]
  (let [rel (= "v" cmd)]
    (if ys
      [{:type :line-string
        :points (reduce (fn [pts y] (conj pts (vec2 cx y)))
                        [current-pos (vec2 cx next-y)]
                        ys)
        :relative? rel}
       (vec2 cx (peek ys))]
      [{:type :line :points [current-pos (vec2 cx next-y)]
        :relative? rel}
       (vec2 cx next-y)])))

(defn cubic-to [cmd current-pos pts]
  (let [rel (= "c" cmd)]
    [{:type :cubic :points (reduce conj [current-pos] pts)
      :relative? rel}
     (peek pts)]))

(defn cubic-chain-to [cmd current-pos pts]
  (let [rel (= "s" cmd)]
    [{:type :cubic-chain :points (reduce conj [current-pos] pts)
      :relative? rel}
     (peek pts)]))

(defn quadratic-to [cmd current-pos pts]
  (let [rel (= "q" cmd)]
    [{:type :quadratic :points (reduce conj [current-pos] pts)
      :relative? rel}
     (peek pts)]))

(defn quadratic-chain-to [cmd current-pos pts]
  (let [rel (= "t" cmd)]
    [{:type :quadratic-chain :points (reduce conj [current-pos] pts)
      :relative? rel}
     (peek pts)]))

(defn arc-to [cmd current-pos pts]
  (let [rel (= "a" cmd)]
    [{:type :arc :points (reduce conj [current-pos] [pts])
      :relative? rel}
     (peek pts)]))


(defn parse-svg-path
  ([path-str]
   (parse-svg-path
    (map (fn parse-coords [[_m cmd coord-str]]
           [cmd
            (let [parsed (parse-svg-coords coord-str)]
              ;; don't partition coordinates into pairs for 1d line commands
              (if (#{"V" "v" "H" "h"} cmd) parsed
                  (svg-coord-pairs parsed)))])
         (re-seq cmd-regex path-str))
    {:origin [0 0]
     :current [0 0]}))
  ([[[cmd coords :as seg] & more]
    {:keys [current origin]
     :as pts}]
   (if (nil? seg)
     '()
     (case cmd
       "M"
       (let [[new-segment new-pos]
             (move-to cmd current coords)]
         (if new-segment
           (lazy-seq
            (cons new-segment
                  (parse-svg-path more (assoc pts :current new-pos))))
           (recur more (assoc pts :current new-pos))))
       "m"
       (let [[line-segment new-pos]
             (move-to cmd current coords)]
         (if line-segment
           (lazy-seq
            (cons line-segment
                  (parse-svg-path
                   more
                   (assoc pts :current new-pos))))
           (recur more (assoc pts :current new-pos))))
       "L" (let [[line-segment new-pos] (line-to cmd current coords)]
             (lazy-seq
              (cons line-segment
                    (parse-svg-path more (assoc pts :current new-pos)))))
       "l" (let [[line-segment new-pos] (line-to cmd current coords)]
             (lazy-seq
              (cons line-segment
                    (parse-svg-path more (assoc pts :current new-pos)))))
       "H"  (let [[line-segment new-pos] (h-line-to cmd current coords)]
              (lazy-seq
               (cons line-segment
                     (parse-svg-path more (assoc pts :current new-pos)))))
       "h"  (let [[line-segment new-pos] (h-line-to cmd current coords)]
              (lazy-seq
               (cons line-segment
                     (parse-svg-path more (assoc pts :current new-pos)))))
       "V"  (let [[line-segment new-pos] (h-line-to cmd current coords)]
              (lazy-seq
               (cons line-segment
                     (parse-svg-path more (assoc pts :current new-pos)))))
       "v"  (let [[line-segment new-pos] (v-line-to cmd current coords)]
              (lazy-seq
               (cons line-segment
                     (parse-svg-path more (assoc pts :current new-pos)))))
       "Q" (let [[line-segment new-pos] (bezier-to cmd current coords)]
             (lazy-seq
              (cons line-segment
                    (parse-svg-path more (assoc pts :current new-pos)))))
       "q" (let [[line-segment new-pos] (bezier-to cmd current coords)]
             (lazy-seq
              (cons line-segment
                    (parse-svg-path more (assoc pts :current new-pos)))))
       "T" (let [[line-segment new-pos] (bezier-to cmd current coords)]
             (lazy-seq
              (cons line-segment
                    (parse-svg-path more (assoc pts :current new-pos)))))
       "t" (let [[line-segment new-pos] (bezier-to cmd current coords)]
             (lazy-seq
              (cons line-segment
                    (parse-svg-path more (assoc pts :current new-pos)))))
       "C" (let [[line-segment new-pos] (bezier-to cmd current coords)]
             (lazy-seq
              (cons line-segment
                    (parse-svg-path more (assoc pts :current new-pos)))))
       "c" (let [[line-segment new-pos] (bezier-to cmd current coords)]
             (lazy-seq
              (cons line-segment
                    (parse-svg-path more (assoc pts :current new-pos)))))
       "S" (let [[line-segment new-pos] (bezier-to cmd current coords)]
             (lazy-seq
              (cons line-segment
                    (parse-svg-path more (assoc pts :current new-pos)))))
       "s" (let [[line-segment new-pos] (bezier-to cmd current coords)]
             (lazy-seq
              (cons line-segment
                    (parse-svg-path more (assoc pts :current new-pos)))))
       "A" (let [[line-segment new-pos] (arc-to cmd current coords)]
             (lazy-seq
              (cons line-segment
                    (parse-svg-path more (assoc pts :current new-pos)))))
       "a" (let [[line-segment new-pos] (arc-to cmd current coords)]
             (lazy-seq
              (cons line-segment
                    (parse-svg-path more (assoc pts :current new-pos)))))
       "Z" (lazy-seq (cons {:type :close :points [current origin]}
                           (parse-svg-path more (assoc pts :current origin))))
       "z" (lazy-seq (cons {:type :close :points [current origin]}
                           (parse-svg-path more (assoc pts :current origin))))
       nil))))

(comment
  (defn parse-svg-path-old
    ([svg]
     (parse-svg-path-old
      (->> svg
           (re-seq #"([MLCZz])\s*(((([0-9\.\-]+)\,?){2}\s*){0,3})")
           (map (fn [[_ t c]]
                  [t (parse-svg-coords c)])))
      [0 0] [0 0]))
    ([[[type points :as seg] & more] p0 pc]
     (when seg
       (cond
         (= "M" type)
         (let [p (first points)] (recur more p p))

         (= "L" type)
         (let [p (first points)]
           (lazy-seq (cons {:type :line :points [pc p]}
                           (parse-svg-path-old more p0 p))))

         (= "C" type)
         (let [p (last points)]
           (lazy-seq (cons {:type :bezier :points (cons pc points)}
                           (parse-svg-path-old more p0 p))))

         (or (= "Z" type) (= "z" type))
         (lazy-seq (cons {:type :close :points [pc p0]}
                         (parse-svg-path-old more p0 p0)))

         :default
         (err/unsupported! (str "Unsupported path segment type" type))))))

  (parse-svg-path-old "M 10 10 C 20 20, 40 20, 50 10")
  (parse-svg-path-old "M 10 10 ")

  )

#?(:clj
   (defn parse-svg
     [src res udist]
     (->> src
          (xml/parse)
          (xml-seq)
          (filter #(= :path (get % :tag)))
          (mapv #(parse-svg-path (-> % (get :attrs) (get :d))))
          (map path2))))

(extend-type Path2

  g/IArea
  (area [_])

  g/IClassify
  (classify-point [_ p])

  g/IProximity
  (closest-point [_ p])

  g/IBoundary
  (contains-point? [_ p])

  g/IBounds
  (bounds [_])

  g/IBoundingCircle
  (bounding-circle [_] nil)

  g/ICenter
  (center
    ([_] nil)
    ([_ o] nil))
  (centroid [_])

  g/ICircumference
  (circumference [_] nil)

  g/IVertexAccess
  (vertices
    [_ res]
    (first (sample-segments* res (get _ :segments))))

  g/IEdgeAccess
  (edges [_])

  g/IPolygonConvert
  (as-polygon
    ([_] nil)
    ([_ res] nil))

  g/ISample
  (point-at [_ t])
  (random-point [_])
  (random-point-inside [_])
  (sample-uniform
    [_ udist include-last?]
    (->> _
         :segments
         (sample-segments* 8)
         (map #(gu/sample-uniform udist include-last? %))
         (first))) ;; TODO why first?
  )
