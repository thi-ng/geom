(ns thi.ng.geom.examples.svg.instancing
  (:require
   [thi.ng.geom.core :as g]
   [thi.ng.geom.vector :refer [vec2]]
   [thi.ng.geom.matrix :refer [M32]]
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
      {:width 600 :height 300}
      (svg/defs
        (apply svg/radial-gradient "rainbow-rad" {} rainbow-gradient)
        (apply svg/linear-gradient "rainbow-lin" {} rainbow-gradient)
        (svg/line-strip
         (spiral [0 0] 0 (* 6 m/TWO_PI) 0 140 300)
         (assoc svg/stroke-round :id "spiral")))
      (svg/instance
       "spiral"
       {:transform (-> M32 (g/translate (vec2 150 150)))
        :stroke "url(#rainbow-rad)"
        :stroke-width 10})
      (svg/instance
       "spiral"
       {:transform (-> M32 (g/translate (vec2 450 150)) (g/rotate m/PI))
        :stroke "url(#rainbow-lin)"
        :stroke-width 5}))
     (svg/serialize)
     (spit "out/svg-instancing.svg"))
