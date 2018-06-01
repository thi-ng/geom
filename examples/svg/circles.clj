(ns thi.ng.geom.examples.svg.circles
  (:require
   [thi.ng.math.core :as m]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.circle :as c]
   [thi.ng.geom.svg.core :as svg]
   [thi.ng.geom.svg.adapter :as adapt]
   [thi.ng.color.core :as col]))

(defn labeled-dot
  [p label] (list (c/circle p 3) (svg/text (m/+ p 10 0) label)))

;; This scene defines 2 circles and their intersection points
(def scene
  (let [c1    (c/circle 50 150 50)
        c2    (c/circle 250 150 50)
        c3    (c/circle 150 150 100)
        [a b] (g/intersect-shape c1 c3)
        [c d] (g/intersect-shape c2 c3)]
    (svg/svg
     {:width 300 :height 300}
     (svg/group
      {:fill "yellow"}
      ;; these circles inherit all attributes from parent group
      c1 c2
      ;; we can use metadata to override specific attribs per shape
      ;; here we also demonstrate automatic color attrib conversion
      (with-meta c3 {:fill (col/rgba 0 1 1 0.25) :stroke (col/hsva 0 1 1)}))
     (svg/group
      {:fill "#000"
       :font-family "Arial, sans-serif"
       :font-size 10}
      (mapcat labeled-dot [a b c d] ["A" "B" "C" "D"])))))

(->> scene
     (adapt/all-as-svg)                  ;; transform all scene elements
     (svg/serialize)                     ;; serialize as SVG XML string
     (spit "out/svg-circles.svg")) ;; write to disk
