(ns thi.ng.geom.test.types.path
  #?(:cljs
     (:require-macros [cemerick.cljs.test :refer (is deftest)]))
  (:require
   [thi.ng.math.core :as m]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.types]
   [thi.ng.geom.line :as l]
   [thi.ng.geom.vector :as v]
   [thi.ng.geom.path :as p]
   #?(:clj
      [clojure.test :refer :all]
      :cljs
      [cemerick.cljs.test])))

(def svg-path-examples
  "Examples of path definitions for various commands taken from MDN:

  https://developer.mozilla.org/en-US/docs/Web/SVG/Tutorial/Paths
  https://developer.mozilla.org/en-US/docs/Web/SVG/Element/path"
  [;; square
   "M 10 10 H 90 V 90 H 10 L 10 10"
   ;; heart
   "M 10,30
A 20,20 0,0,1 50,30
A 20,20 0,0,1 90,30
Q 90,60 50,90
Q 10,60 10,30 z"
   ;; cubic curves
   "M 10 10 C 20 20, 40 20, 50 10"
   "M 70 10 C 70 20, 110 20, 110 10"
   "M 130 10 C 120 20, 180 20, 170 10"
   "M 10 60 C 20 80, 40 80, 50 60"
   "M 70 60 C 70 80, 110 80, 110 60"
   "M 130 60 C 120 80, 180 80, 170 60"
   "M 10 110 C 20 140, 40 140, 50 110"
   "M 70 110 C 70 140, 110 140, 110 110"
   "M 130 110 C 120 140, 180 140, 170 110"
   ;; smooth cubic
   "M 10 80 C 40 10, 65 10, 95 80 S 150 150, 180 80"
   ;; quadratic
   "M 10 80 Q 95 10 180 80"
   ;; quadratic shorthand
   "M 10 80 Q 52.5 10, 95 80 T 180 80"
   ;; arcs
   "M 10 315
L 110 215
A 30 50 0 0 1 162.55 162.45
L 172.55 152.45
A 30 50 -45 0 1 215.1 109.9
L 315 10"
   "M 10 315
L 110 215
A 36 60 0 0 1 150.71 170.29
L 172.55 152.45
A 30 50 -45 0 1 215.1 109.9
L 315 10"
   "M 80 80
A 45 45, 0, 0, 0, 125 125
L 125 80 Z"
   "M 230 80
A 45 45, 0, 1, 0, 275 125
L 275 80 Z"
   "M 80 230
A 45 45, 0, 0, 1, 125 275
L 125 230 Z"
   "M 230 230
A 45 45, 0, 1, 1, 275 275
L 275 230 Z"
   ]
  )

(deftest svg-path-parse
  (testing "parsing SVG path definitions")
  (doseq [ex svg-path-examples]
    (testing (str ex)
      (is (some? (p/parse-svg-path ex))
          "SVG path should parse into geometry type"))))
