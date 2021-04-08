(ns thi.ng.geom.test.types.line
  #?(:cljs
     (:require-macros
      [cemerick.cljs.test :refer (is deftest)]))
  (:require
   [thi.ng.math.core :as m]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.types]
   [thi.ng.geom.line :as l]
   [thi.ng.geom.vector :as v]
   #?(:clj
      [clojure.test :refer :all]
      :cljs
      [cemerick.cljs.test])))

(deftest test-flip
  (let [line (l/line2 (v/vec2) (v/vec2 1 10))
        flip-line (g/flip line)]
    (is (m/delta= (g/point-at line 0.4) (v/vec2 0.4 4)))
    (is (m/delta= (g/point-at flip-line 0.6) (v/vec2 0.4 4)))
    (is (m/delta= (g/point-at line 0.5) (g/point-at flip-line 0.5)))
    (is (m/delta= (g/point-at line 0.1) (g/point-at flip-line 0.9))))
  (let [line (l/line3 (v/vec3) (v/vec3 1 10 1))
        flip-line (g/flip line)]
    (is (m/delta= (g/point-at line 0.4) (v/vec3 0.4 4 0.4)))
    (is (m/delta= (g/point-at flip-line 0.6) (v/vec3 0.4 4 0.4)))
    (is (m/delta= (g/point-at line 0.5) (g/point-at flip-line 0.5)))
    (is (m/delta= (g/point-at line 0.1) (g/point-at flip-line 0.9)))))
