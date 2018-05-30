(ns thi.ng.geom.test.voxel.core
  #?(:cljs
     (:require-macros
      [cemerick.cljs.test :refer (is deftest with-test run-tests testing)]))
  (:require
   [thi.ng.geom.core :as g]
   [thi.ng.geom.vector :refer [vec3]]
   [thi.ng.geom.voxel.svo :as svo]
   [thi.ng.geom.voxel.isosurface :as iso]
   #?(:clj
      [clojure.test :refer :all]
      :cljs
      [cemerick.cljs.test])))

(deftest test-main
  (is true "is true"))
