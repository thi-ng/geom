(ns thi.ng.geom.test.viz.core
  (:require
   [thi.ng.geom.core :as g]
   [thi.ng.geom.viz.core :as viz]
   [thi.ng.geom.svg.core :as svg]
   [thi.ng.math.core :as m]
   #?(:clj
      [clojure.test :refer :all]
      :cljs
      [cemerick.cljs.test :refer-macros [is deftest]])))

(deftest test-main
  (is true "is true"))
