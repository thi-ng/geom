(ns thi.ng.geom.gl.camera
  (:refer-clojure :exclude [apply])
  (:require
   [thi.ng.math.core :as m]
   [thi.ng.geom.vector :as v :refer [vec3]]
   [thi.ng.geom.matrix :as mat]))

;; Perspective camera
;;
;; | *Key*     | *Type*        |      *Default* | *Description*                           |
;; |-----------+---------------+----------------+-----------------------------------------|
;; | `:eye`    | vec3          |          `nil` | Camera position                         |
;; | `:target` | vec3          |     `(vec3 0)` | Camera target (center of view)          |
;; | `:up`     | vec3          | `(vec3 0 1 0)` | Camera up axis                          |
;; | `:fov`    | float         |             45 | Vertical FOV in degrees                 |
;; | `:aspect` | float or rect |           16:9 | Camera aspect ratio (or view rectangle) |
;; | `:near`   | float         |            0.1 | Camera near clipping distance           |
;; | `:far`    | float         |            100 | Camera far clipping distance            |

(defn apply
  "Takes a GL model spec map & camera, injects :view & :proj
  uniforms into spec."
  [spec cam]
  (update spec :uniforms merge {:view (get cam :view) :proj (get cam :proj)}))

(defn update-keys
  "Takes a map m, key seq and map of new vals, replaces keys in m with
  new vals. If a value in opts map is a function, applies fn to value
  of key in original map."
  [m ks opts]
  (reduce-kv
   (fn [acc k v] (assoc acc k (if (fn? v) (v (m k)) v)))
   m (select-keys opts ks)))

(defn set-view
  [cam opts]
  (let [cam (update-keys cam [:eye :target :up] opts)]
    (assoc cam :view (mat/look-at (get cam :eye) (get cam :target) (get cam :up)))))

(defn set-projection
  [cam opts]
  (let [cam (update-keys cam  [:fov :aspect :near :far] opts)]
    (assoc cam :proj (mat/perspective (get cam :fov) (get cam :aspect) (get cam :near) (get cam :far)))))

(defn perspective-camera
  [opts]
  (-> (merge
       {:eye    (vec3 0.0 0.0 2.0)
        :target v/V3
        :up     v/V3Y
        :fov    45
        :near   0.1
        :far    100
        :aspect (/ 16.0 9.0)}
       opts)
      (set-view opts)
      (set-projection opts)))
