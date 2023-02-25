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
       #?(:clj (map #(Double/parseDouble %)) :cljs (map js/parseFloat))
       (partition 2)
       (mapv vec2)))

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

  (re-seq #"(?i)([achlmqstvz])([^achlmqstvz]+)"
          "M 10 80 10 -60 l 20 20 l -40 -30 v 10 l 5 5 5 10 20 10")

  (re-seq cmd-regex
          "M 10 80 10 -60 l 20 20 l -40 -30 v 10 l 5 5 5 10 20 10 Z")

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

(defn line-to [cmd current-pos [next-pt & pts]]
  (if pts
    [{:type :line-string :points (reduce conj [current-pos next-pt] pts)}
     (peek pts)]
    [{:type :line :points [current-pos next-pt]}
     next-pt]))

(defn parse-svg-path
  ([path-str]
   (parse-svg-path
    (map (fn parse-coords [[_m cmd coord-str]]
           [cmd (parse-svg-coords coord-str)])
         (re-seq cmd-regex path-str))
    {:origin [0 0]
     :current [0 0]}))
  ([[[cmd coords :as seg] & more]
    {:keys [current origin]
     :as pts}]
   (when seg
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
       ;; "H"  (h-line-to cmd current-pos coords)
       ;; "h"  (h-line-to cmd current-pos coords)
       ;; "V"  (v-line-to cmd current-pos coords)
       ;; "v"  (v-line-to cmd current-pos coords)
       ;; "C"  (cubic-to cmd current-pos coords)
       ;; "c"  (cubic-to cmd current-pos coords)
       ;; "S" nil
       ;; "s" nil
       "Z" (lazy-seq (cons {:type :close :points [current origin]}
                           (parse-svg-path more (assoc pts :current origin))))
       "z" (lazy-seq (cons {:type :close :points [current origin]}
                           (parse-svg-path more (assoc pts :current origin))))
       nil
       ))))

(comment
  (defn parse-svg-path
    ([svg]
     (parse-svg-path
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
                           (parse-svg-path more p0 p))))

         (= "C" type)
         (let [p (last points)]
           (lazy-seq (cons {:type :bezier :points (cons pc points)}
                           (parse-svg-path more p0 p))))

         (or (= "Z" type) (= "z" type))
         (lazy-seq (cons {:type :close :points [pc p0]}
                         (parse-svg-path more p0 p0)))

         :default
         (err/unsupported! (str "Unsupported path segment type" type)))))))

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
