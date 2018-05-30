(require '[thi.ng.geom.viz.core :as viz] :reload)
(require '[thi.ng.geom.svg.core :as svg])
(require '[thi.ng.geom.vector :as v])
(require '[thi.ng.color.core :as col])
(require '[thi.ng.math.core :as m :refer [PI TWO_PI]])
(require '[thi.ng.math.noise :as n])

(def viz-spec
  {:x-axis (viz/linear-axis
            {:domain [0 63]
             :range  [50 550]
             :major  8
             :minor  2
             :pos    550})
   :y-axis (viz/linear-axis
            {:domain      [0 63]
             :range       [550 50]
             :major       8
             :minor       2
             :pos         50
             :label-dist  15
             :label-style {:text-anchor "end"}})
   :data   [{:matrix       (->> (for [y (range 64) x (range 64)] (n/noise2 (* x 0.06) (* y 0.06)))
                                (viz/contour-matrix 64 64))
             :levels       (range -1 1 0.05)
             :value-domain [-1.0 1.0]
             :attribs      {:fill "none" :stroke "#0af"}
             :layout       viz/svg-contour-plot}]})

(def viz-spec-log
  (merge viz-spec
         {:x-axis (viz/log-axis
                   {:domain [0 64]
                    :range [50 550]
                    :base 2
                    :pos 555})
          :y-axis (viz/log-axis
                   {:domain      [0 64]
                    :range       [550 50]
                    :base        2
                    :pos         45
                    :label-dist  15
                    :label-style {:text-anchor "end"}})}))

(def fill-attribs {:fill (col/rgba 0.0 0.66 1.0 0.05) :stroke "#fff"})

(defn export-viz
  [viz path] (->> viz (svg/svg {:width 600 :height 600}) (svg/serialize) (spit path)))

(->> {"contours-outline.svg"     [viz-spec false]
      "contours.svg"             [viz-spec true]
      "contours-log-outline.svg" [viz-spec-log false]
      "contours-log.svg"         [viz-spec-log true]}
     (run!
      (fn [[path [spec fill?]]]
        (-> (if fill? (assoc-in spec [:data 0 :attribs] fill-attribs) spec)
            (viz/svg-plot2d-cartesian)
            (export-viz path)))))
