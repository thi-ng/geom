(require '[thi.ng.geom.core :as g])
(require '[thi.ng.geom.vector :refer [vec2]])
(require '[thi.ng.geom.utils.subdiv :as sd])
(require '[thi.ng.geom.svg.core :as svg])
(require '[thi.ng.math.core :as m])
(require '[thi.ng.dstruct.core :as d])

(defn demo
  [points [id col]]
  (for [i (range 1 6)]
    (let [off-x (* 120 (dec i))
          pts   (->> points
                     (mapv #(m/+ % off-x 0))
                     (d/iterate-n i (partial sd/subdivide-closed (id sd/schemes))))]
      (svg/group
       {:stroke col}
       (svg/polygon pts)
       (map #(svg/circle % 1.5) pts)))))

(def points
  (mapv vec2
        [[10 10] [60 60] [110 10] [110 60]
         [85 80] [110 100] [110 150] [60 100]
         [10 150] [10 100] [35 80] [10 60]]))

(->> [[:chaikin "red"] [:cubic-bezier "blue"]]
     (map #(demo points %))
     (apply svg/svg
            {:width 600 :height 200 :fill "none" :font-family "Arial" :font-size 12}
            (svg/text [300 170] ":chaikin" {:fill "red"})
            (svg/text [300 186] ":cubic-bezier" {:fill "blue"}))
     (svg/serialize)
     (spit "out/svg-subdiv-curves.svg"))
