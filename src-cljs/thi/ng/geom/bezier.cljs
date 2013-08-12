(ns thi.ng.geom.bezier
  (:require
   [thi.ng.geom.core :as g]
   [thi.ng.data.core :as d]))

(defn interpolate*
  [afn sfn [a b c d] t]
  (let [it (- 1.0 t) it2 (* it it) t2 (* t t)]
    (-> a
        (sfn (* it2 it))
        (afn (sfn b (* (* 3.0 t) it2)))
        (afn (sfn c (* (* 3.0 t2) it)))
        (afn (sfn d (* t2 t))))))

(defn sample-segment
  [afn sfn seg res]
  (for [t (range 0 1 (/ 1.0 res))]
    (interpolate* afn sfn seg t)))

(defn as-linestrip
  [afn sfn points res include-last?]
  (let [ls (->> points
        (d/successive-nth 4)
        (take-nth 3)
        (mapcat #(sample-segment afn sfn % res)))]
    (if include-last?
      (concat ls [(last points)])
      ls)))
