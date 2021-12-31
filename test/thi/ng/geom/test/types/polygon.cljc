(ns thi.ng.geom.test.types.polygon
  #?(:cljs
     (:require-macros
      [cemerick.cljs.test :refer (is deftest)]))
  (:require
   [thi.ng.geom.core :as g]
   #?(:clj [thi.ng.geom.types] :cljs [thi.ng.geom.types :refer [Triangle2]])
   [thi.ng.geom.polygon :as p]
   #?(:clj
      [clojure.test :refer :all]
      :cljs
      [cemerick.cljs.test]))
  #?(:clj (:import [thi.ng.geom.types Triangle2])))

(deftest test-flip
  (let [poly (p/polygon2 [[0 0] [1 0] [1 1] [0 1]])
        flipped-poly (g/flip poly)]
    (is (= (-> poly g/as-mesh g/faces count)
           (-> flipped-poly g/as-mesh g/faces count))
        "flipped polygon tesselates to same number of faces")))

(deftest tessellation
  (let [poly (p/polygon2 [[0 0] [1 0] [1 1] [0 1]])]
    (is (every? #(instance? Triangle2 %) (g/tessellate poly)))))
