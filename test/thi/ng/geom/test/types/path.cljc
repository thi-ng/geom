(ns thi.ng.geom.test.types.path
  #?(:cljs
     (:require-macros [cemerick.cljs.test :refer (is deftest)]))
  (:require
   [clojure.string :as str]
   [thi.ng.math.core :as m]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.types :as types]
   [thi.ng.geom.line :as l]
   [thi.ng.geom.vector :as v]
   [thi.ng.geom.path :as p]
   [hiccup.core :refer [html]]
   #?(:clj
      [clojure.test :refer :all]
      [clojure.string :as str]
      :cljs
      [cemerick.cljs.test])))

;; idea: store all of these examples in an actual SVG document
;; they can be moved with a transform operation so they don't overlap
;; and metadata about each one can be stored as XML properties

;; will it work for invalid paths that also need to be tested?

(def svg-path-examples
  "Examples of path definitions for various commands taken from MDN + W3C:

  https://developer.mozilla.org/en-US/docs/Web/SVG/Tutorial/Paths
  https://developer.mozilla.org/en-US/docs/Web/SVG/Element/path
  https://www.w3.org/TR/SVG11/paths.html#PathElement"
  [{:path "M 100 100 L 300 100 L 200 300 z", :description "triangle", :num-segments 4}
   {:path "M 10 10 H 90 V 90 H 10 L 10 10", :description "square", :num-segments 5}
   {:path "M 10,30\nA 20,20 0,0,1 50,30\nA 20,20 0,0,1 90,30\nQ 90,60 50,90\nQ 10,60 10,30 z",
    :description "heart", :num-segments 6}
   {:path "M 10,80 20,20 40,40 0,10 Z",
    :description "implicit polyline after move command", :num-segments 2}
   {:path "m 10,80 20,20 40,40 0,10 z",
    :description "implicit polyline after move command (relative)", :num-segments 2}
   {:path "m 10 80 10 -60 l 20 20 l -40 -30 v 10 l 5 5 5 10 20 10",
    :description "implicit polyline after move command + explicit polyline", :num-segments 5}
   {:path "M 10 10 C 20 20, 40 20, 50 10",
    :description "cubic curve", :num-segments 2}
   {:path "M 70 10 C 70 20, 110 20, 110 10",
    :description "cubic curve", :num-segments 2}
   {:path "M 130 10 C 120 20, 180 20, 170 10",
    :description "cubic curve", :num-segments 2}
   {:path "M 10 60 C 20 80, 40 80, 50 60",
    :description "cubic curve", :num-segments 2}
   {:path "M 70 60 C 70 80, 110 80, 110 60",
    :description "cubic curve", :num-segments 2}
   {:path "M 130 60 C 120 80, 180 80, 170 60",
    :description "cubic curve", :num-segments 2}
   {:path "M 10 110 C 20 140, 40 140, 50 110",
    :description "cubic curve", :num-segments 2}
   {:path "M 70 110 C 70 140, 110 140, 110 110",
    :description "cubic curve", :num-segments 2}
   {:path "M 130 110 C 120 140, 180 140, 170 110",
    :description "cubic curve", :num-segments 2}
   {:path "M 10 80 C 40 10, 65 10, 95 80 S 150 150, 180 80",
    :description "smooth cubic curve",
    :num-segments 3}
   {:path "M 10 80 Q 95 10 180 80", :description "quadratic curve", :num-segments 2}
   {:path "M 10 80 Q 52.5 10, 95 80 T 180 80", :description "quadratic curve (shorthand)",
    :num-segments 3}
   {:path "M 10 315\nL 110 215\nA 30 50 0 0 1 162.55 162.45\nL 172.55 152.45\nA 30 50 -45 0 1 215.1 109.9\nL 315 10", :description "arc",
    :num-segments 6}
   {:path "M 10 315\nL 110 215\nA 36 60 0 0 1 150.71 170.29\nL 172.55 152.45\nA 30 50 -45 0 1 215.1 109.9\nL 315 10", :description "arc",
    :num-segments 6}
   {:path "M 80 80\nA 45 45, 0, 0, 0, 125 125\nL 125 80 Z", :description "arc",
    :num-segments 4}
   {:path "M 230 80\nA 45 45, 0, 1, 0, 275 125\nL 275 80 Z", :description "arc",
    :num-segments 4}
   {:path "M 80 230\nA 45 45, 0, 0, 1, 125 275\nL 125 230 Z", :description "arc",
    :num-segments 4}
   {:path "M 230 230\nA 45 45, 0, 1, 1, 275 275\nL 275 230", :description "arc",
    :num-segments 3}
   {:path "m 10 10", :description "move command without lines",
    :num-segments 1}
   {:path "m 10 80 l 20 20 l -40 -30 20 v 10 l 5 5 5 10 20 10",
    :description "extra coordinate not in an appropriate pair for line command",
    :num-segments 5}
   {:path "M 10 10 L 100-200", :description "no whitespace following coordinate",
    :num-segments 2}
   {:path "M 10 10 L 0.6.5", :description "multiple decimals within coordinate",
    :num-segments 2}]
  )

(deftest svg-path-parse
  (testing "coordinate parsing"
    (is (= [0.6 0.5] (p/parse-svg-coords "0.6.5")))
    (is (= [100.0 -200.0] (p/parse-svg-coords "100-200"))))

  (testing "individual commands"
    (let [[{:keys [type points]}]  (p/move-to "m" [0 0] [[10 80] [10 -60] [10 -90]])]
      (is (= :line type))
      (is (= 3 (count points))
          "Polyline should be produced from move command when defined")))

  (testing "parsing SVG path definitions -"
    (doseq [{:keys [path description num-segments] :as ex}
            svg-path-examples]
      (testing (str description ":\n" path)
        (let [segments (p/parse-svg-path path)
              path-geom (types/->Path2 segments)
              no-commas (str/replace path #"\," " ")]
          (is (seq? segments)
              "SVG path should parse into a sequence of segment definitions")
          (is (= num-segments (count segments))
              "SVG path should parse into the correct number of segments for the given shape")
          (is (some? (:segments path-geom))
              "SVG path should parse into geometry object")
          (is (some? (g/sample-uniform path-geom 1.0 true))
              "Geometry object should be functional")
          (is (= segments (p/parse-svg-path no-commas))
              "Comma placement should be irrelevant to parsing"))))))

(defn path-svg-grid
  "Generate Hiccup data for SVG export from example paths according a simple grid layout."
  [paths {:keys [width height] :as opts}]
  (let [n-paths (count paths)
        nearest-square
        (->> (range (max width height))
             (map #(Math/pow % 2))
             (filter #(> % n-paths))
             first
             Math/sqrt)
        x-interval (/ (* width 1.0) nearest-square)
        y-interval (/ (* height 1.0) nearest-square)]

    (->> (for [x (range nearest-square)
               y (range nearest-square)] [x y])
         (take n-paths)
         (map (fn [[x y]]
                (let [ix (+ (* x nearest-square) y)
                      {:keys [path description num-segments]} (nth paths ix)]
                  [:path {:id (str "svg-test-example-" (long ix))
                          :d path
                          :transform (format "translate(%f,%f)"
                                             (* x x-interval)
                                             (* y y-interval))
                          :data-description description
                          :data-num-segments num-segments}])))
         (reduce conj [:svg {:id "svg-path-test-data"
                             :width width
                             :height height}]))))

(comment

  ;; generate SVG for visual inspection from example paths
  (spit "assets/test-path-grid.svg"
        (html {:mode :xml} (path-svg-grid svg-path-examples {:width 1000 :height 1000})))

  (g/sample-uniform
   (types/->Path2 (p/parse-svg-path-old "M 10 10 L 20 20"))  2.5 true)

  (g/sample-uniform (p/path2 (first (p/parse-svg-path-old "M 10 10 L 20 20")))
                    2.5 true)

  (g/random-point
   (types/->Path2 (p/parse-svg-path-old "M 10 10 L 10 10")))

  (count svg-path-examples)
  (p/parse-svg-path (:path (first svg-path-examples)))
  (p/parse-svg-path (:path (nth svg-path-examples 1)))

  (p/parse-svg-path "m 10 80 l 20 20 l -40 -30 20 v 10 l 5 5 5 10 20 10")
  (-> "M 10 315
L 110 215
A 36 60 0 0 1 150.71 170.29
L 172.55 152.45
A 30 50 -45 0 1 215.1 109.9
L 315 10"
      (p/parse-svg-path)
      types/->Path2)

  (let [[a b & rest] [1 2 3 4 5 6 7]]
    (peek rest)))
