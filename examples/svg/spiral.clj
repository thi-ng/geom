(ns thi.ng.geom.examples.svg.spiral
  (:require
   [thi.ng.geom.core :as g]
   [thi.ng.geom.vector :refer [vec2]]
   [thi.ng.geom.svg.core :as svg]
   [thi.ng.color.core :as col]
   [thi.ng.math.core :as m]
   [thi.ng.math.macros :as mm]))

(defn spiral
  [center start end r1 r2 steps]
  (map
   (fn [r theta] (m/+ (g/as-cartesian (vec2 r theta)) center))
   (range r1 r2 (mm/subdiv r2 r1 steps))
   (range start end (mm/subdiv end start steps))))

(def rainbow-gradient (map (fn [h] [h (col/hsva h 1 1)]) (m/norm-range 12)))

(->> (svg/svg
      {:width 300 :height 300}
      (svg/defs
        (apply svg/radial-gradient "rainbow" {} rainbow-gradient))
      (svg/line-strip
       (spiral [150 150] 0 (* 6 m/TWO_PI) 0 140 300)
       (assoc svg/stroke-round
              :stroke "url(#rainbow)"
              :stroke-width 10)))
     (svg/serialize)
     (spit "out/svg-spiral.svg"))
