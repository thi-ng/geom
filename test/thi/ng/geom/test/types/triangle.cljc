(ns thi.ng.geom.test.types.triangle
  #?(:cljs
     (:require-macros
      [cemerick.cljs.test :refer (is deftest)]))
  (:require
   [thi.ng.geom.types]
   [thi.ng.geom.circle :as c]
   [thi.ng.geom.triangle :as t]
   [thi.ng.geom.vector :as v]
   #?(:clj
      [clojure.test :refer [is deftest]]
      :cljs
      [cemerick.cljs.test])))

(deftest test-circumcircle
  (let [a (v/vec2 0 0)
        b (v/vec2 4 0)
        c (v/vec2 0 2)
        expected (c/circle (v/vec2 2.0 0.0) 2.0)]
    (is (= expected (t/circumcircle a b c)))
    (is (= expected (t/circumcircle (t/triangle2 a b c))))
    (is (= expected (t/circumcircle {:a a :b b :c c})))
    (is (= expected (t/circumcircle [a b c])))))
