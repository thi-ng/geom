(ns thi.ng.geom.test.physics.core
  #?(:cljs
     (:require-macros
      [cemerick.cljs.test :refer (is deftest with-test run-tests testing)]))
  (:require
   [thi.ng.geom.core :as g]
   [thi.ng.geom.vector :refer [vec2 vec3]]
   [thi.ng.geom.physics.core :as ph]
   [thi.ng.math.core :as m]
   #?(:clj
      [clojure.test :refer :all]
      :cljs
      [cemerick.cljs.test])))

(deftest test-main
  (is true "is true"))
