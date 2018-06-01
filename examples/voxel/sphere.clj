(ns thi.ng.geom.examples.voxel.sphere
  (:require
   [thi.ng.geom.core :as g]
   [thi.ng.geom.vector :refer [vec3]]
   [thi.ng.geom.voxel.svo :as svo]
   [thi.ng.geom.voxel.isosurface :as iso]
   [thi.ng.geom.mesh.io :as mio]
   [thi.ng.math.core :as m]
   [clojure.java.io :as io]))

(def res (double 1/4))
(def num-holes 30)

(defn voxel-sphere
  ([tree op o r res]
   (let [rg (range (- r) (+ r res) res)]
     (->> (for [x rg y rg, z rg
                :let [v (vec3 x y z)]
                :when (<= (m/mag v) r)] (m/+ o v))
          (svo/apply-voxels op tree)))))

(time
 (def v
   (reduce
    (fn [tree [op o r]] (voxel-sphere tree op o r res))
    (svo/voxeltree 32 res)
    (concat
     [[svo/set-at (vec3 15 15 15) 14]]
     (repeatedly
      num-holes
      #(vector
        svo/delete-at
        (vec3 (m/random 32) (m/random 32) (m/random 32))
        (m/random 4 8)))))))

(time
 (with-open [o (io/output-stream "out/voxel-sphere.stl")]
   (mio/write-stl
    (mio/wrapped-output-stream o)
    (g/tessellate (iso/surface-mesh v 10 0.5)))))
