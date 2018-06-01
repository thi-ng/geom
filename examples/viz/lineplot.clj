(require '[thi.ng.geom.viz.core :as viz] :reload)
(require '[thi.ng.geom.svg.core :as svg])
(require '[thi.ng.geom.vector :as v])
(require '[thi.ng.color.core :as col])
(require '[thi.ng.math.core :as m :refer [PI TWO_PI]])

(defn test-equation
  [t] (let [x (m/mix* (- PI) PI t)] [x (* (Math/cos (* 0.5 x)) (Math/sin (* x x x)))]))

(defn export-viz
  [viz path] (->> viz (svg/svg {:width 600 :height 320}) (svg/serialize) (spit path)))

(def viz-spec
  {:x-axis (viz/linear-axis
            {:domain [(- PI) PI]
             :range  [50 580]
             :major  (/ PI 2)
             :minor  (/ PI 4)
             :pos    250})
   :y-axis (viz/linear-axis
            {:domain      [-1 1]
             :range       [250 20]
             :major       0.2
             :minor       0.1
             :pos         50
             :label-dist  15
             :label-style {:text-anchor "end"}})
   :grid   {:attribs {:stroke "#caa"}
            :minor-y true}
   :data   [{:values  (map test-equation (m/norm-range 200))
             :attribs {:fill "none" :stroke "#0af"}
             :layout  viz/svg-line-plot}]})

(-> viz-spec
    (viz/svg-plot2d-cartesian)
    (export-viz "out/lineplot.svg"))

;; same spec, just update style attribs & layout method
(-> viz-spec
    (update-in [:data 0] merge {:attribs {:fill "#0af"} :layout viz/svg-area-plot})
    (viz/svg-plot2d-cartesian)
    (export-viz "out/areaplot.svg"))

(def viz-spec-polar
  {:x-axis (viz/linear-axis
            {:domain [(- PI) PI]
             :range  [(* 1.1 PI) (* 1.9 PI)]
             :major  (/ PI 2)
             :minor  (/ PI 16)
             :pos    280})
   :y-axis (viz/linear-axis
            {:domain [-1 1]
             :range  [60 280]
             :major  0.5
             :minor  0.25
             :pos    (* 1.1 PI)})
   :origin (v/vec2 300 310)
   :grid   {:attribs {:stroke "#caa" :fill "none"}
            :minor-x true
            :minor-y true}
   :data   [{:values  (map test-equation (m/norm-range 200))
             :attribs {:fill "none" :stroke "#0af"}
             :layout  viz/svg-line-plot}]})

(-> viz-spec-polar (viz/svg-plot2d-polar) (export-viz "out/lineplot-polar.svg"))

;; same spec, just update style attribs & layout method
(-> viz-spec-polar
    (update-in [:data 0] merge {:attribs {:fill "#0af"} :res 20 :layout viz/svg-area-plot})
    (viz/svg-plot2d-polar)
    (export-viz "out/areaplot-polar.svg"))
