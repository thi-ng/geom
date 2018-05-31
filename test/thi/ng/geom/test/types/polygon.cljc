(ns thi.ng.geom.test.types.polygon
  #?(:cljs
     (:require-macros
      [cemerick.cljs.test :refer (is deftest)]))
  (:require
   [thi.ng.geom.core :as g]
   [thi.ng.geom.vector :refer [vec2]]
   [thi.ng.geom.types]
   [thi.ng.geom.polygon :as p]
   #?(:clj
      [clojure.test :refer :all]
      :cljs
      [cemerick.cljs.test])))

(deftest test-flip
  (let [poly (p/polygon2 [[0 0] [1 0] [1 1] [0 1]])
        flipped-poly (g/flip poly)]
    (is (= (-> poly g/as-mesh g/faces count)
           (-> flipped-poly g/as-mesh g/faces count))
        "flipped polygon tesselates to same number of faces")))
