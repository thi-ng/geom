(ns thi.ng.geom.test.types.cuboid
  #?(:cljs
     (:require-macros
      [cemerick.cljs.test :refer [is deftest]]))
  (:require
   [thi.ng.math.core :as m]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.utils :as gu]
   [thi.ng.geom.vector :refer [vec3]]
   [thi.ng.geom.matrix :refer [M44]]
   [thi.ng.geom.types]
   [thi.ng.geom.aabb :as a]
   [thi.ng.geom.plane :as pl]
   [thi.ng.geom.cuboid :as cu]
   [thi.ng.geom.sphere :as s]
   [thi.ng.geom.gmesh :as gm]
   [thi.ng.geom.basicmesh :as bm]
   [thi.ng.geom.triangle :as t]
   #?(:clj
      [clojure.test :refer :all]
      :cljs
      [cemerick.cljs.test])))

(deftest test-ctors
  (is true))

(deftest test-impls
  (is true))
