(ns thi.ng.geom.test.types.bezier
  #?(:cljs
     (:require-macros
      [cemerick.cljs.test :refer (is deftest)]))
  (:require
   [thi.ng.math.core :as m]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.types]
   [thi.ng.geom.bezier :as b]
   [thi.ng.geom.vector :as v]
   #?(:clj
      [clojure.test :refer :all]
      :cljs
      [cemerick.cljs.test])))

(deftest test-flip
  (let [bezier (b/bezier2 [(v/vec2) (v/vec2 2 2) (v/vec2 4 0)])
        flip-bezier (g/flip bezier)]
    (is (m/delta= (g/point-at bezier 0.4) (v/vec2 1.6 1.6)))
    (is (m/delta= (g/point-at flip-bezier 0.6) (v/vec2 1.6 1.6)))
    (is (m/delta= (g/point-at bezier 0.5) (g/point-at flip-bezier 0.5)))
    (is (m/delta= (g/point-at bezier 0.1) (g/point-at flip-bezier 0.9))))
  (let [bezier (b/bezier3 [(v/vec3) (v/vec3 2 2 2) (v/vec3 4 0 0)])
        flip-bezier (g/flip bezier)]
    (is (m/delta= (g/point-at bezier 0.6) (v/vec3 2.4 1.6 1.6)))
    (is (m/delta= (g/point-at flip-bezier 0.4) (v/vec3 2.4 1.6 1.6)))
    (is (m/delta= (g/point-at bezier 0.5) (g/point-at flip-bezier 0.5)))
    (is (m/delta= (g/point-at bezier 0.1) (g/point-at flip-bezier 0.9)))))
