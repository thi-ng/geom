(ns thi.ng.geom.test.mesh.core
  #?(:cljs
     (:require-macros
      [cemerick.cljs.test :refer (is deftest with-test run-tests testing)]))
  (:require
   [thi.ng.geom.core :as g]
   [thi.ng.geom.vector :refer [vec2 vec3]]
   [thi.ng.geom.types]
   [thi.ng.geom.aabb :as a]
   [thi.ng.geom.circle :as c]
   [thi.ng.geom.rect :as r]
   [thi.ng.geom.sphere :as s]
   [thi.ng.geom.triangle :as t]
   [thi.ng.math.core :as m]
   #?(:clj
      [clojure.test :refer :all]
      :cljs
      [cemerick.cljs.test])))

(deftest test-main
  (is true "is true"))
