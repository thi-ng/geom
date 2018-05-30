(require '[thi.ng.geom.viz.core :as viz] :reload)
(require '[thi.ng.geom.svg.core :as svg])
(require '[thi.ng.geom.vector :as v])
(require '[thi.ng.color.core :as col])
(require '[thi.ng.math.core :as m :refer [PI TWO_PI]])

(defn export-viz
  [viz path] (->> viz (svg/svg {:width 600 :height 320}) (svg/serialize) (spit path)))

(defn bar-spec
  [num width]
  (fn [idx col]
    {:values     (map (fn [i] [i (m/random 100)]) (range 2000 2016))
     :attribs    {:stroke       col
                  :stroke-width (str (dec width) "px")}
     :layout     viz/svg-bar-plot
     :interleave num
     :bar-width  width
     :offset     idx}))

(def viz-spec
  {:x-axis (viz/linear-axis
            {:domain [1999 2016]
             :range  [50 580]
             :major  1
             :pos    280
             :label  (viz/default-svg-label int)})
   :y-axis (viz/linear-axis
            {:domain      [0 100]
             :range       [280 20]
             :major       10
             :minor       5
             :pos         50
             :label-dist  15
             :label-style {:text-anchor "end"}})
   :grid   {:minor-y true}})

(-> viz-spec
    (assoc :data [((bar-spec 1 20) 0 "#0af")])
    (viz/svg-plot2d-cartesian)
    (export-viz "bars.svg"))

(-> viz-spec
    (assoc :data (map-indexed (bar-spec 3 6) ["#0af" "#fa0" "#f0a"]))
    (viz/svg-plot2d-cartesian)
    (export-viz "bars-interleave.svg"))
