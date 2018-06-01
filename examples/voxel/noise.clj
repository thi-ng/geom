(ns thi.ng.geom.examples.voxel.noise
  (:require
   [thi.ng.geom.core :as g]
   [thi.ng.geom.vector :refer [vec3]]
   [thi.ng.geom.voxel.svo :as svo]
   [thi.ng.geom.voxel.isosurface :as iso]
   [thi.ng.geom.mesh.io :as mio]
   [thi.ng.math.core :as m]
   [clojure.java.io :as io]
   [thi.ng.math.noise :as n]))

(def res (double 1/4))
(def n-scale 0.1)
(def iso-val 0.33)

(def v
  (let [r (range 1 31 res)]
    (->> (for [x r y r z r
               :when (<= iso-val (m/abs* (n/noise3 (* x n-scale) (* y n-scale) (* z n-scale))))]
           (vec3 x y z))
         (svo/apply-voxels svo/set-at (svo/voxeltree 32 res)))))

(time
 (with-open [o (io/output-stream "out/voxel-noise.stl")]
   (mio/write-stl
    (mio/wrapped-output-stream o)
    (g/tessellate (iso/surface-mesh v 10 iso-val)))))
