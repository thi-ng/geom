(ns thi.ng.geom.test.types.path
  #?(:cljs
     (:require-macros [cemerick.cljs.test :refer (is deftest)]))
  (:require
   [thi.ng.math.core :as m]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.types :as types]
   [thi.ng.geom.line :as l]
   [thi.ng.geom.vector :as v]
   [thi.ng.geom.path :as p]
   #?(:clj
      [clojure.test :refer :all]
      [clojure.string :as str]
      :cljs
      [cemerick.cljs.test])))

(def svg-path-examples
  "Examples of path definitions for various commands taken from MDN + W3C:

  https://developer.mozilla.org/en-US/docs/Web/SVG/Tutorial/Paths
  https://developer.mozilla.org/en-US/docs/Web/SVG/Element/path
  https://www.w3.org/TR/SVG11/paths.html#PathElement"
  [;; triangle
   "M 100 100 L 300 100 L 200 300 z"
   ;; square
   "M 10 10 H 90 V 90 H 10 L 10 10"
   ;; heart
   "M 10,30
A 20,20 0,0,1 50,30
A 20,20 0,0,1 90,30
Q 90,60 50,90
Q 10,60 10,30 z"
   ;; implicit polyline after moveto command
   ;; this invalidates the test defined using the command count function below
   "M 10,80 20,20 40,40 0,10 Z"
   ;; relative equivalent of above shape
   "m 10 80 10 -60 l 20 20 l -40 -30 z"

   ;; implicit move + explicit polyline
   "m 10 80 10 -60 l 20 20 l -40 -30 v 10 l 5 5 5 10 20 10"

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

   ;; incorrect / pathological / flawed examples
   ;;                         ↓ extra coordinate not in a pair
   "m 10 80 l 20 20 l -40 -30 20 v 10 l 5 5 5 10 20 10"
   "M 100-200" ; lack of space after coordinate
   "M 0.6.5" ; multiple decimals within coordinate
   ]
  )

(defn num-commands [path-str]
  (count (re-seq #"[MmLlHhVvCcSsQqAaTt]\s+" path-str)))

(deftest svg-path-parse
  (testing "coordinate parsing"
    (is (= [(v/vec2 0.6 0.5)] (p/parse-svg-coords "0.6.5")))
    (is (= [(v/vec2 100 -200)] (p/parse-svg-coords "100-200")))
    )

  (testing "parsing SVG path definitions"
    (doseq [ex svg-path-examples]
      (testing (str ex)
        (let [segments (p/parse-svg-path ex)
              n-commands (num-commands ex)
              path-geom (types/->Path2 segments)
              no-commas (str/replace ex #"\," "")]
          (is (some? segments)
              "SVG path should parse into segment definitions")
          (is (some? (:segments path-geom))
              "SVG path should parse into geometry object")
          (is (= n-commands
                 (count (:segments path-geom)))
              "Geometry should have the same number of segments as the number of path commands")

          (is (= segments (p/parse-svg-path no-commas))
              "Comma placement should be irrelevant to parsing")
          )))))
