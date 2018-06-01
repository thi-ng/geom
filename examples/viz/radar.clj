(require '[thi.ng.geom.viz.core :as viz] :reload)
(require '[thi.ng.geom.svg.core :as svg])
(require '[thi.ng.geom.vector :as v])
(require '[thi.ng.color.core :as col])
(require '[thi.ng.math.core :as m :refer [PI TWO_PI]])

(def category->domain (zipmap [:C1 :C2 :C3 :C4 :C5 :C6] (range)))
(def domain->category (reduce-kv #(assoc % %3 %2) {} category->domain))

(defn random-radar-spec
  "Generates radar plot data spec w/ random values for each category in the form:
   {:C1 0.8 :C2 0.2 ...}"
  [color]
  {:values   (zipmap (keys category->domain) (repeatedly #(m/random 0.25 1)))
   :item-pos (fn [[k v]] [(category->domain k) v])
   :attribs  {:fill (col/rgba color)}
   :layout   viz/svg-radar-plot})

(defn random-radar-spec-minmax
  "Generates radar plot data spec w/ random value intervals for each category in the form:
   {:C1 [0.5 0.8] :C2 [0.12 0.2] ...}"
  [color]
  {:values       (zipmap
                  (keys category->domain)
                  (repeatedly #(let [x (m/random 0.5 1)] [(* x (m/random 0.25 0.75)) x])))
   :item-pos-min (fn [[k v]] [(category->domain k) (first v)])
   :item-pos-max (fn [[k v]] [(category->domain k) (peek v)])
   :attribs      {:fill (col/rgba color)}
   :layout       viz/svg-radar-plot-minmax})

(def viz-spec
  {:x-axis (viz/linear-axis
            {:domain     [0 5]
             :range      [0 (* (/ 5 6) TWO_PI)]
             :major      1
             :label-dist 20
             :pos        260
             :label      (viz/default-svg-label (comp name domain->category))})
   :y-axis (viz/linear-axis
            {:domain      [0 1.05]
             :range       [0 260]
             :major       0.5
             :minor       0.1
             :pos         (/ PI 2)
             :label-style {:text-anchor "start"}
             :label       (viz/default-svg-label viz/format-percent)})
   :grid   {:minor-x true :minor-y true}
   :origin (v/vec2 300 300)
   :circle true})

(->> (assoc viz-spec :data (mapv random-radar-spec [[0 0.66 1 0.33] [1 0.5 0 0.33] [1 0 0.8 0.33]]))
     (viz/svg-plot2d-polar)
     (svg/svg {:width 600 :height 600})
     (svg/serialize)
     (spit "out/radarplot.svg"))

(->> (assoc viz-spec :data (mapv random-radar-spec-minmax [[0 0.66 1 0.33] [1 0.5 0 0.33] [1 0 0.8 0.33]]))
     (viz/svg-plot2d-polar)
     (svg/svg {:width 600 :height 600})
     (svg/serialize)
     (spit "out/radarplot-minmax.svg"))
