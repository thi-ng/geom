(ns thi.ng.geom.test.types.aabb
  #?(:cljs
     (:require-macros
      [cemerick.cljs.test :refer (is deftest with-test run-tests testing)]))
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
  (is (= (a/aabb [100 200 300] [10 20 30])
         (a/aabb-from-minmax [100 200 300] [110 220 330]))
      "aabb-from-minmax")
  (is (= (a/aabb [0 0 0] [10 10 10]) (a/aabb 10))
      "aabb n")
  (is (= (a/aabb [0 0 0] [10 20 30]) (a/aabb 10 20 30))
      "aabb sz sy sz"))

(deftest test-impls
  (let [[px py pz :as p] (vec3 100 200 300)
        [w h d :as s]    (vec3 10 20 30)
        [qx qy qz :as q] (m/+ p s)
        c (m/madd s 0.5 p)
        a (a/aabb p s)]
    (is (== (* 2 (+ (* w h) (* w d) (* h d))) (g/area a)) "area")
    (is (== (* w h d) (g/volume a)) "volume")
    (is (= a (g/bounds a)) "bounds")
    (is (= w (g/width a)) "width")
    (is (= h (g/height a)) "height")
    (is (= d (g/depth a)) "depth")
    (is (= (s/sphere c (g/dist c (m/+ p s))) (g/bounding-sphere a)) "bounding sphere")
    (is (= c (g/centroid a)) "centroid")
    (is (= (vec3) (g/centroid (g/center a))) "center + centroid")
    (is (= (vec3 -1 -2 -3) (g/centroid (g/center a (vec3 -1 -2 -3)))) "center p + centroid")
    (is (= 8 (count (g/vertices a))) "vert count")
    (is (= 6 (count (g/faces a))) "face count")
    (is (= 12 (count (g/edges a))) "edge count")
    (is (instance? thi.ng.geom.types.BasicMesh (g/as-mesh a)) "as bmesh")
    (is (instance? thi.ng.geom.types.GMesh (g/as-mesh a {:mesh (gm/gmesh)})) "as gmesh")
    (is (= 1 (count (g/faces (g/as-mesh a {:flags :n})))))
    (is (= 1 (count (g/faces (g/as-mesh a {:flags :s})))))
    (is (= 1 (count (g/faces (g/as-mesh a {:flags :e})))))
    (is (= 1 (count (g/faces (g/as-mesh a {:flags :w})))))
    (is (= 1 (count (g/faces (g/as-mesh a {:flags :f})))))
    (is (= 1 (count (g/faces (g/as-mesh a {:flags :b})))))
    (is (every? #(g/contains-point? a %)
                (take 1000 (repeatedly #(g/random-point-inside a))))
        "random-p-inside contains")
    (is (every? pos?
                (take 1000 (repeatedly #(g/classify-point a (g/random-point-inside a)))))
        "random-p-inside classify")
    (is (every? zero?
                (take 1000 (repeatedly #(g/classify-point a (g/random-point a)))))
        "random-p classify on surface")
    (is (= 27 (count (g/subdivide a {:num 3}))) "subdiv :num")
    (is (= 6 (count (g/subdivide a {:cols 3 :rows 2}))) "subdiv :cols :rows")
    (is (= 12 (count (g/subdivide a {:cols 3 :rows 2 :slices 2}))) "subdiv :cols :rows :slices")
    (is (= 12 (count (g/tessellate a))) "tessellate")
    (is (= (a/aabb s) (g/translate a (m/- p))) "translate")
    (is (= (a/aabb (m/* p 2) (m/* s 2)) (g/scale a 2)) "scale")
    (is (= (a/aabb (m/madd s -0.5 p) (m/* s 2)) (g/scale-size a 2)) "scale-size")
    (is (= (cu/cuboid (vec3) s) (g/transform a (g/translate M44 (m/- p)))) "translate via mat")
    (is (= (cu/cuboid (m/* p 2) (m/* s 2)) (g/transform a (g/scale M44 2))) "scale via mat")
    (is (= (a/aabb [-1 -2 -3] [3 5 7]) (m/union (a/aabb [-1 -2 -3] 1) (a/aabb [1 2 3] 1))) "union")
    (is (= (a/aabb) (m/union (a/aabb) (a/aabb))) "union self")
    (is (= (a/aabb 0.5 0.5) (m/intersection (a/aabb) (a/aabb 0.5 1))) "intersection aabb 1")
    (is (= (a/aabb 1 0) (m/intersection (a/aabb) (a/aabb 1 1))) "intersection aabb 2")
    (is (= (vec3) (g/map-point a p)) "map-point 1")
    (is (= (vec3 1) (g/map-point a q)) "map-point 2")
    (is (= (vec3 0 1 1) (g/map-point a (vec3 px qy qz))) "map-point 3")
    (is (= (vec3 1 0 1) (g/map-point a (vec3 qx py qz))) "map-point 4")
    (is (= (vec3 1 1 0) (g/map-point a (vec3 qx qy pz))) "map-point 5")
    (is (= (vec3 0.5) (g/map-point a (g/centroid a))) "map-point centroid")
    (is (= p (g/unmap-point a (vec3))) "unmap-point 1")
    (is (= q (g/unmap-point a (vec3 1))) "unmap-point 2")
    (is (= c (g/unmap-point a (vec3 0.5))) "unmap-point 3")))

;; There're 26 distinct cases for producing the closest point on an AABB
;; to a given random point:
;;
;; - 6 sides
;; - 12 edges
;; - 8 corners

(deftest test-proximity
  (let [box       (a/aabb 1)
        sectors   (->> (for [z (range -1 2) y (range -1 2) x (range -1 2)
                             :when (not (and (zero? x) (zero? y) (zero? z)))
                             :let [kx ({-1 '-x 0 "-0" 1 '+x} x)
                                   ky ({-1 '-y 0 "-0" 1 '+y} y)
                                   kz ({-1 '-z 0 "-0" 1 '+z} z)
                                   id (keyword (str kx ky kz))]]
                         [id (a/aabb [x y z] 1)])
                       (into {}))
        samples   1000
        on-plane? (fn [p n secid]
                    (let [pl  (pl/plane-with-point p n)
                          sec (sectors secid)]
                      (prn :sector-plane sec)
                      (every?
                       (fn [q]
                         (let [cp  (g/closest-point box q)
                               res (g/classify-point pl cp)]
                           (or (zero? res) (prn :fail q :cp cp :res res))))
                       (repeatedly samples #(g/random-point-inside sec)))))
        on-edge?  (fn [a b secid]
                    (let [sec (sectors secid)]
                      (prn :sector-edge sec)
                      (every?
                       (fn [q]
                         (let [cp  (g/closest-point box q)
                               cp' (gu/closest-point-on-line cp a b)
                               d   (g/dist-squared cp cp')]
                           (or (m/delta= 0 d) (prn :fail q :cp cp :dist d))))
                       (repeatedly samples #(g/random-point-inside sec)))))
        is-point? (fn [p secid]
                    (let [sec (sectors secid)]
                      (prn :sector-point sec)
                      (every?
                       (fn [q]
                         (let [cp (g/closest-point box q)]
                           (or (m/delta= p cp) (prn :fail q :cp cp))))
                       (repeatedly samples #(g/random-point-inside sec)))))]
    (is (== 26 (count sectors)))
    (is (on-plane? (vec3 1 0 0) (vec3 1 0 0) :+x-0-0))
    (is (on-plane? (vec3 0 0 0) (vec3 -1 0 0) :-x-0-0))
    (is (on-plane? (vec3 0 1 0) (vec3 0 1 0) :-0+y-0))
    (is (on-plane? (vec3 0 0 0) (vec3 0 -1 0) :-0-y-0))
    (is (on-plane? (vec3 0 0 1) (vec3 0 0 1) :-0-0+z))
    (is (on-plane? (vec3 0 0 0) (vec3 0 0 -1) :-0-0-z))
    (is (on-edge? (vec3 0 0 0) (vec3 1 0 0) :-0-y-z))
    (is (on-edge? (vec3 0 1 0) (vec3 1 1 0) :-0+y-z))
    (is (on-edge? (vec3 0 0 1) (vec3 1 0 1) :-0-y+z))
    (is (on-edge? (vec3 0 1 1) (vec3 1 1 1) :-0+y+z))
    (is (on-edge? (vec3 0 0 0) (vec3 0 1 0) :-x-0-z))
    (is (on-edge? (vec3 1 0 0) (vec3 1 1 0) :+x-0-z))
    (is (on-edge? (vec3 0 0 1) (vec3 0 1 1) :-x-0+z))
    (is (on-edge? (vec3 1 0 1) (vec3 1 1 1) :+x-0+z))
    (is (on-edge? (vec3 0 0 0) (vec3 0 0 1) :-x-y-0))
    (is (on-edge? (vec3 1 0 0) (vec3 1 0 1) :+x-y-0))
    (is (on-edge? (vec3 0 1 0) (vec3 0 1 1) :-x+y-0))
    (is (on-edge? (vec3 1 1 0) (vec3 1 1 1) :+x+y-0))
    (is (is-point? (vec3 0 0 0) :-x-y-z))
    (is (is-point? (vec3 1 0 0) :+x-y-z))
    (is (is-point? (vec3 0 1 0) :-x+y-z))
    (is (is-point? (vec3 1 1 0) :+x+y-z))
    (is (is-point? (vec3 0 0 1) :-x-y+z))
    (is (is-point? (vec3 1 0 1) :+x-y+z))
    (is (is-point? (vec3 0 1 1) :-x+y+z))
    (is (is-point? (vec3 1 1 1) :+x+y+z))))
