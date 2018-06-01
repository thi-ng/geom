(require '[thi.ng.geom.viz.core :as viz] :reload)
(require '[thi.ng.geom.svg.core :as svg])
(require '[thi.ng.geom.vector :as v])
(require '[thi.ng.color.core :as col])
(require '[thi.ng.math.core :as m :refer [PI TWO_PI]])
(require '[thi.ng.color.gradients :as grad])
(require '[thi.ng.geom.core :as g])
(require '[thi.ng.geom.utils :as gu])
(require '[thi.ng.math.noise :as n])

(def test-matrix
  (->> (for [y (range 10) x (range 50)] (n/noise2 (* x 0.1) (* y 0.25)))
       (viz/matrix-2d 50 10)))

(defn heatmap-spec
  [id]
  {:matrix        test-matrix
   :value-domain  (viz/value-domain-bounds test-matrix)
   :palette       (->> id grad/cosine-schemes (grad/cosine-gradient 100))
   :palette-scale viz/linear-scale
   :layout        viz/svg-heatmap})

(defn cartesian-viz
  [prefix id & [opts]]
  (->> {:x-axis (viz/linear-axis
                 {:domain [0 50]
                  :range [50 550]
                  :major 10
                  :minor 5
                  :pos 280})
        :y-axis (viz/linear-axis
                 {:domain      [0 10]
                  :range       [280 20]
                  :major       1
                  :pos         50
                  :label-dist  15
                  :label-style {:text-anchor "end"}})
        :data   [(merge (heatmap-spec id) opts)]}
       (viz/svg-plot2d-cartesian)
       (svg/svg {:width 600 :height 300})
       (svg/serialize)
       (spit (str prefix "-" (name id) ".svg"))))

(cartesian-viz "out/hm" :rainbow2)
(cartesian-viz "out/hm" :orange-blue)

(defn polar-viz
  [prefix id & [opts]]
  (->> {:x-axis (viz/linear-axis
                 {:domain [0 50]
                  :range [(* 1.1 PI) (* 1.9 PI)]
                  :major 10
                  :minor 5
                  :pos 280})
        :y-axis (viz/linear-axis
                 {:domain     [0 10]
                  :range      [90 280]
                  :major      5
                  :pos        (* 1.1 PI)
                  :major-size 10
                  :label-dist 20})
        :origin (v/vec2 300)
        :data   [(merge (heatmap-spec id) opts)]}
       (viz/svg-plot2d-polar)
       (svg/svg {:width 600 :height 320})
       (svg/serialize)
       (spit (str prefix "-" (name id) ".svg"))))

(polar-viz "out/hmp" :yellow-magenta-cyan)
(polar-viz "out/hmp" :green-magenta)

;; using custom shape function applied for each matrix cell
;; (a circle fitting within the 4 points defining a grid cell)
(cartesian-viz "out/hms" :rainbow2 {:shape viz/circle-cell})
(polar-viz "out/hmsp" :rainbow2 {:shape viz/circle-cell})
