(require '[thi.ng.geom.viz.core :as viz] :reload)
(require '[thi.ng.geom.svg.core :as svg])
(require '[thi.ng.geom.vector :as v])
(require '[thi.ng.color.core :as col])
(require '[thi.ng.math.core :as m :refer [PI TWO_PI]])

(->> {:x-axis (viz/linear-axis
               {:domain [-10 310]
                :range  [50 550]
                :major  100
                :minor  50
                :pos    150})
      :y-axis (viz/linear-axis
               {:domain  [0 4]
                :range   [50 150]
                :visible false})
      :data   [{:values  [[0 100] [10 90] [80 200] [250 300] [150 170] [110 120]
                          [210 280] [180 280] [160 240] [160 170]]
                :attribs {:stroke-width "10px" :stroke-linecap "round" :stroke "#0af"}
                :layout  viz/svg-stacked-interval-plot}]}
     (viz/svg-plot2d-cartesian)
     (svg/svg {:width 600 :height 200})
     (svg/serialize)
     (spit "out/intervals.svg"))
