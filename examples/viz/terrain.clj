(require '[thi.ng.geom.viz.core :as viz] :reload)
(require '[thi.ng.geom.svg.core :as svg])
(require '[thi.ng.geom.vector :as v])
(require '[thi.ng.color.core :as col])
(require '[thi.ng.math.core :as m :refer [PI TWO_PI]])
(require '[thi.ng.color.gradients :as grad])

(defn load-image
  [path]
  (let [img (javax.imageio.ImageIO/read (java.io.File. path))
        w   (.getWidth img)
        h   (.getHeight img)
        rgb (.getRGB img 0 0 w h (int-array (* w h)) 0 w)]
    (viz/contour-matrix w h (map #(bit-and % 0xff) rgb))))

(def viz-spec
  {:x-axis (viz/linear-axis
            {:domain [0 79]
             :range [50 550]
             :major 8
             :minor 2
             :pos 550})
   :y-axis (viz/linear-axis
            {:domain      [0 79]
             :range       [50 550]
             :major       8
             :minor       2
             :pos         50
             :label-dist  15
             :label-style {:text-anchor "end"}})
   :data   [{:matrix          (load-image "assets/california-detail-gis.png")
             :value-domain    [0.0 255.0]
             :attribs         {:fill "none"}
             :palette         (->> :orange-blue grad/cosine-schemes (grad/cosine-gradient 100))
             :contour-attribs (fn [col] {:stroke col})
             :layout          viz/svg-contour-plot}]})

(doseq [res [6 12 18 24]]
  (->> (assoc-in viz-spec [:data 0 :levels] (range 0 255 res))
       (viz/svg-plot2d-cartesian)
       (svg/svg {:width 600 :height 600})
       (svg/serialize)
       (spit (str "out/terrain-" res ".svg"))))
