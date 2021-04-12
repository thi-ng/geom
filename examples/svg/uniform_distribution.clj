(ns thi.ng.geom.examples.svg.uniform-distribution
  (:require [thi.ng.geom.circle :as c]
            [thi.ng.geom.core :as g]
            [thi.ng.geom.polygon :as p]
            [thi.ng.geom.rect :as r]
            [thi.ng.geom.svg.adapter :as adapt]
            [thi.ng.geom.svg.core :as svg]
            [thi.ng.geom.triangle :as t]
            [thi.ng.geom.vector :as v]))

(defn sample-points
  ([shape sample-method]
   (sample-points shape sample-method 400))
  ([shape sample-method n]
   (let [centered (g/center shape)]
     (repeatedly n #(sample-method centered)))))

(defn example [pos shape points description]
  (let [shape-centered (g/center shape)]
    (svg/group {}
               (svg/text (g/translate pos (v/vec2 0 -70))
                         description
                         {:text-anchor "middle"})
               (svg/group {:fill "none" :stroke "red"} (g/translate shape-centered pos))
               (svg/group {:opacity 0.8}
                          (for [[x y] points]
                            (g/translate (c/circle x y 0.5) pos))))))

(defn scene []
  (let [circle (c/circle 0 0 50)
        ;; ellipse is not implemented?
        rectangle (r/rect 0 0 100 100)
        rotated-rectangle (g/rotate rectangle 0.25)
        triangle (t/triangle2 (v/vec2 0 0) (v/vec2 100 50) (v/vec2 25 100))
        polygon (p/polygon2 [0 0] [50 75] [100 100] [100 50] [75 25])]
    (svg/svg {:width 800 :height 600 :stroke "black"}
             (example (v/vec2 100 100) circle
                      (sample-points circle g/random-point-inside)
                      "g/random-point-inside")
             (example (v/vec2 300 100) circle
                      (sample-points circle c/random-point-in-circle)
                      "c/random-point-in-circle")
             (example (v/vec2 500 100) circle
                      (sample-points circle g/random-point 200)
                      "g/random-point")
             (example (v/vec2 700 100) circle
                      (g/sample-uniform (g/center circle) 10 true)
                      "g/sample-uniform")

             (example (v/vec2 100 250) rectangle
                      (sample-points rectangle g/random-point-inside)
                      "g/random-point-inside")
             (example (v/vec2 300 250) rotated-rectangle
                      (sample-points rotated-rectangle g/random-point-inside)
                      "g/random-point-inside (polygon)")
             (example (v/vec2 500 250) rectangle
                      (sample-points rectangle g/random-point 200)
                      "g/random-point")
             (example (v/vec2 700 250) rectangle
                      (g/sample-uniform (g/center rectangle) 10 true)
                      "g/sample-uniform")

             (example (v/vec2 100 400) triangle
                      (sample-points triangle g/random-point-inside)
                      "g/random-point-inside")
             (example (v/vec2 300 400) triangle
                      (sample-points triangle t/random-point-in-triangle2)
                      "t/random-point-in-triangle2")
             (example (v/vec2 500 400) triangle
                      (sample-points triangle g/random-point 200)
                      "g/random-point")
             (example (v/vec2 700 400) triangle
                      (g/sample-uniform (g/center triangle) 10 true)
                      "g/sample-uniform")

             (example (v/vec2 300 550) polygon
                      (sample-points polygon g/random-point-inside)
                      "g/random-point-inside")
             (example (v/vec2 500 550) polygon
                      (sample-points polygon g/random-point 200)
                      "g/random-point")
             (example (v/vec2 700 550) polygon
                      (g/sample-uniform (g/center polygon) 10 true)
                      "g/sample-uniform")
             )))

(->> (scene)
     adapt/all-as-svg
     svg/serialize
     (spit "out/uniform-distribution.svg"))
