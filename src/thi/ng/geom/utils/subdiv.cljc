(ns thi.ng.geom.utils.subdiv
  #?(:cljs
     (:require-macros
      [thi.ng.math.macros :as mm]))
  (:require
   [thi.ng.math.core :as m]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.vector :as v :refer [vec2 vec3]]
   #?(:clj [thi.ng.math.macros :as mm])))

;; Iterative curve subdivision
;; http://algorithmicbotany.org/papers/subgpu.sig2003.pdf

;; See example in: /examples/svg/subdiv.clj

(defn subdiv-kernel3
  [u v [a b c]]
  [(->> (m/* c (u 2)) (m/madd b (u 1)) (m/madd a (u 0)))
   (->> (m/* c (v 2)) (m/madd b (v 1)) (m/madd a (v 0)))])

(defn subdiv-kernel5
  [u v [a b c d e]]
  [(->> (m/* e (u 4)) (m/madd d (u 3)) (m/madd c (u 2)) (m/madd b (u 1)) (m/madd a (u 0)))
   (->> (m/* e (v 4)) (m/madd d (v 3)) (m/madd c (v 2)) (m/madd b (v 1)) (m/madd a (v 0)))])

(defn subdivide-closed
  ([scheme points]
   (subdivide-closed (get scheme :fn) (get scheme :coeff) points))
  ([f [u v] points]
   (let [n  (count u)
         n2 (int (/ n 2))]
     (->> (concat (take-last n2 points) points (take n2 points))
          (partition n 1)
          (mapcat #(f u v %))))))

(def schemes
  {:chaikin      {:fn subdiv-kernel3 :coeff [[0.25 0.75 0] [0 0.75 0.25]]}
   :cubic-bezier {:fn subdiv-kernel3 :coeff [[0.125 0.75 0.125] [0 0.5 0.5]]}})
