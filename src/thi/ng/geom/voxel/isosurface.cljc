(ns thi.ng.geom.voxel.isosurface
  #?(:cljs (:require-macros [thi.ng.geom.macros.voxel :refer [not-cond->]]))
  (:require
   [thi.ng.geom.core :as g]
   [thi.ng.geom.vector :as v :refer [vec3]]
   [thi.ng.geom.basicmesh :as bm]
   [thi.ng.geom.voxel.svo :as svo :refer [cell-index select-cells voxel-cell voxel-config-at-depth]]
   [thi.ng.math.core :as m]
   #?(:clj [thi.ng.geom.macros.voxel :refer [not-cond->]])))

;; Marching cube lookup tables
;; Edge offset table

(def edge-offsets
  [[0 0 0 0] [1 0 0 2] [0 0 1 0] [0 0 0 2]
   [0 1 0 0] [1 1 0 2] [0 1 1 0] [0 1 0 2]
   [0 0 0 1] [1 0 0 1] [1 0 1 1] [0 0 1 1]])

;; Triangles per cube index

(def cell-triangles
  [[]
   [[0 8 3]]
   [[0 1 9]]
   [[1 8 3] [9 8 1]]
   [[1 2 10]]
   [[0 8 3] [1 2 10]]
   [[9 2 10] [0 2 9]]
   [[2 8 3] [2 10 8] [10 9 8]]
   [[3 11 2]]
   [[0 11 2] [8 11 0]]
   [[1 9 0] [2 3 11]]
   [[1 11 2] [1 9 11] [9 8 11]]
   [[3 10 1] [11 10 3]]
   [[0 10 1] [0 8 10] [8 11 10]]
   [[3 9 0] [3 11 9] [11 10 9]]
   [[9 8 10] [10 8 11]]
   [[4 7 8]]
   [[4 3 0] [7 3 4]]
   [[0 1 9] [8 4 7]]
   [[4 1 9] [4 7 1] [7 3 1]]
   [[1 2 10] [8 4 7]]
   [[3 4 7] [3 0 4] [1 2 10]]
   [[9 2 10] [9 0 2] [8 4 7]]
   [[2 10 9] [2 9 7] [2 7 3] [7 9 4]]
   [[8 4 7] [3 11 2]]
   [[11 4 7] [11 2 4] [2 0 4]]
   [[9 0 1] [8 4 7] [2 3 11]]
   [[4 7 11] [9 4 11] [9 11 2] [9 2 1]]
   [[3 10 1] [3 11 10] [7 8 4]]
   [[1 11 10] [1 4 11] [1 0 4] [7 11 4]]
   [[4 7 8] [9 0 11] [9 11 10] [11 0 3]]
   [[4 7 11] [4 11 9] [9 11 10]]
   [[9 5 4]]
   [[9 5 4] [0 8 3]]
   [[0 5 4] [1 5 0]]
   [[8 5 4] [8 3 5] [3 1 5]]
   [[1 2 10] [9 5 4]]
   [[3 0 8] [1 2 10] [4 9 5]]
   [[5 2 10] [5 4 2] [4 0 2]]
   [[2 10 5] [3 2 5] [3 5 4] [3 4 8]]
   [[9 5 4] [2 3 11]]
   [[0 11 2] [0 8 11] [4 9 5]]
   [[0 5 4] [0 1 5] [2 3 11]]
   [[2 1 5] [2 5 8] [2 8 11] [4 8 5]]
   [[10 3 11] [10 1 3] [9 5 4]]
   [[4 9 5] [0 8 1] [8 10 1] [8 11 10]]
   [[5 4 0] [5 0 11] [5 11 10] [11 0 3]]
   [[5 4 8] [5 8 10] [10 8 11]]
   [[9 7 8] [5 7 9]]
   [[9 3 0] [9 5 3] [5 7 3]]
   [[0 7 8] [0 1 7] [1 5 7]]
   [[1 5 3] [3 5 7]]
   [[9 7 8] [9 5 7] [10 1 2]]
   [[10 1 2] [9 5 0] [5 3 0] [5 7 3]]
   [[8 0 2] [8 2 5] [8 5 7] [10 5 2]]
   [[2 10 5] [2 5 3] [3 5 7]]
   [[7 9 5] [7 8 9] [3 11 2]]
   [[9 5 7] [9 7 2] [9 2 0] [2 7 11]]
   [[2 3 11] [0 1 8] [1 7 8] [1 5 7]]
   [[11 2 1] [11 1 7] [7 1 5]]
   [[9 5 8] [8 5 7] [10 1 3] [10 3 11]]
   [[5 7 0] [5 0 9] [7 11 0] [1 0 10] [11 10 0]]
   [[11 10 0] [11 0 3] [10 5 0] [8 0 7] [5 7 0]]
   [[11 10 5] [7 11 5]]
   [[10 6 5]]
   [[0 8 3] [5 10 6]]
   [[9 0 1] [5 10 6]]
   [[1 8 3] [1 9 8] [5 10 6]]
   [[1 6 5] [2 6 1]]
   [[1 6 5] [1 2 6] [3 0 8]]
   [[9 6 5] [9 0 6] [0 2 6]]
   [[5 9 8] [5 8 2] [5 2 6] [3 2 8]]
   [[2 3 11] [10 6 5]]
   [[11 0 8] [11 2 0] [10 6 5]]
   [[0 1 9] [2 3 11] [5 10 6]]
   [[5 10 6] [1 9 2] [9 11 2] [9 8 11]]
   [[6 3 11] [6 5 3] [5 1 3]]
   [[0 8 11] [0 11 5] [0 5 1] [5 11 6]]
   [[3 11 6] [0 3 6] [0 6 5] [0 5 9]]
   [[6 5 9] [6 9 11] [11 9 8]]
   [[5 10 6] [4 7 8]]
   [[4 3 0] [4 7 3] [6 5 10]]
   [[1 9 0] [5 10 6] [8 4 7]]
   [[10 6 5] [1 9 7] [1 7 3] [7 9 4]]
   [[6 1 2] [6 5 1] [4 7 8]]
   [[1 2 5] [5 2 6] [3 0 4] [3 4 7]]
   [[8 4 7] [9 0 5] [0 6 5] [0 2 6]]
   [[7 3 9] [7 9 4] [3 2 9] [5 9 6] [2 6 9]]
   [[3 11 2] [7 8 4] [10 6 5]]
   [[5 10 6] [4 7 2] [4 2 0] [2 7 11]]
   [[0 1 9] [4 7 8] [2 3 11] [5 10 6]]
   [[9 2 1] [9 11 2] [9 4 11] [7 11 4] [5 10 6]]
   [[8 4 7] [3 11 5] [3 5 1] [5 11 6]]
   [[5 1 11] [5 11 6] [1 0 11] [7 11 4] [0 4 11]]
   [[0 5 9] [0 6 5] [0 3 6] [11 6 3] [8 4 7]]
   [[6 5 9] [6 9 11] [4 7 9] [7 11 9]]
   [[10 4 9] [6 4 10]]
   [[4 10 6] [4 9 10] [0 8 3]]
   [[10 0 1] [10 6 0] [6 4 0]]
   [[8 3 1] [8 1 6] [8 6 4] [6 1 10]]
   [[1 4 9] [1 2 4] [2 6 4]]
   [[3 0 8] [1 2 9] [2 4 9] [2 6 4]]
   [[0 2 4] [4 2 6]]
   [[8 3 2] [8 2 4] [4 2 6]]
   [[10 4 9] [10 6 4] [11 2 3]]
   [[0 8 2] [2 8 11] [4 9 10] [4 10 6]]
   [[3 11 2] [0 1 6] [0 6 4] [6 1 10]]
   [[6 4 1] [6 1 10] [4 8 1] [2 1 11] [8 11 1]]
   [[9 6 4] [9 3 6] [9 1 3] [11 6 3]]
   [[8 11 1] [8 1 0] [11 6 1] [9 1 4] [6 4 1]]
   [[3 11 6] [3 6 0] [0 6 4]]
   [[6 4 8] [11 6 8]]
   [[7 10 6] [7 8 10] [8 9 10]]
   [[0 7 3] [0 10 7] [0 9 10] [6 7 10]]
   [[10 6 7] [1 10 7] [1 7 8] [1 8 0]]
   [[10 6 7] [10 7 1] [1 7 3]]
   [[1 2 6] [1 6 8] [1 8 9] [8 6 7]]
   [[2 6 9] [2 9 1] [6 7 9] [0 9 3] [7 3 9]]
   [[7 8 0] [7 0 6] [6 0 2]]
   [[7 3 2] [6 7 2]]
   [[2 3 11] [10 6 8] [10 8 9] [8 6 7]]
   [[2 0 7] [2 7 11] [0 9 7] [6 7 10] [9 10 7]]
   [[1 8 0] [1 7 8] [1 10 7] [6 7 10] [2 3 11]]
   [[11 2 1] [11 1 7] [10 6 1] [6 7 1]]
   [[8 9 6] [8 6 7] [9 1 6] [11 6 3] [1 3 6]]
   [[0 9 1] [11 6 7]]
   [[7 8 0] [7 0 6] [3 11 0] [11 6 0]]
   [[7 11 6]]
   [[7 6 11]]
   [[3 0 8] [11 7 6]]
   [[0 1 9] [11 7 6]]
   [[8 1 9] [8 3 1] [11 7 6]]
   [[10 1 2] [6 11 7]]
   [[1 2 10] [3 0 8] [6 11 7]]
   [[2 9 0] [2 10 9] [6 11 7]]
   [[6 11 7] [2 10 3] [10 8 3] [10 9 8]]
   [[7 2 3] [6 2 7]]
   [[7 0 8] [7 6 0] [6 2 0]]
   [[2 7 6] [2 3 7] [0 1 9]]
   [[1 6 2] [1 8 6] [1 9 8] [8 7 6]]
   [[10 7 6] [10 1 7] [1 3 7]]
   [[10 7 6] [1 7 10] [1 8 7] [1 0 8]]
   [[0 3 7] [0 7 10] [0 10 9] [6 10 7]]
   [[7 6 10] [7 10 8] [8 10 9]]
   [[6 8 4] [11 8 6]]
   [[3 6 11] [3 0 6] [0 4 6]]
   [[8 6 11] [8 4 6] [9 0 1]]
   [[9 4 6] [9 6 3] [9 3 1] [11 3 6]]
   [[6 8 4] [6 11 8] [2 10 1]]
   [[1 2 10] [3 0 11] [0 6 11] [0 4 6]]
   [[4 11 8] [4 6 11] [0 2 9] [2 10 9]]
   [[10 9 3] [10 3 2] [9 4 3] [11 3 6] [4 6 3]]
   [[8 2 3] [8 4 2] [4 6 2]]
   [[0 4 2] [4 6 2]]
   [[1 9 0] [2 3 4] [2 4 6] [4 3 8]]
   [[1 9 4] [1 4 2] [2 4 6]]
   [[8 1 3] [8 6 1] [8 4 6] [6 10 1]]
   [[10 1 0] [10 0 6] [6 0 4]]
   [[4 6 3] [4 3 8] [6 10 3] [0 3 9] [10 9 3]]
   [[10 9 4] [6 10 4]]
   [[4 9 5] [7 6 11]]
   [[0 8 3] [4 9 5] [11 7 6]]
   [[5 0 1] [5 4 0] [7 6 11]]
   [[11 7 6] [8 3 4] [3 5 4] [3 1 5]]
   [[9 5 4] [10 1 2] [7 6 11]]
   [[6 11 7] [1 2 10] [0 8 3] [4 9 5]]
   [[7 6 11] [5 4 10] [4 2 10] [4 0 2]]
   [[3 4 8] [3 5 4] [3 2 5] [10 5 2] [11 7 6]]
   [[7 2 3] [7 6 2] [5 4 9]]
   [[9 5 4] [0 8 6] [0 6 2] [6 8 7]]
   [[3 6 2] [3 7 6] [1 5 0] [5 4 0]]
   [[6 2 8] [6 8 7] [2 1 8] [4 8 5] [1 5 8]]
   [[9 5 4] [10 1 6] [1 7 6] [1 3 7]]
   [[1 6 10] [1 7 6] [1 0 7] [8 7 0] [9 5 4]]
   [[4 0 10] [4 10 5] [0 3 10] [6 10 7] [3 7 10]]
   [[7 6 10] [7 10 8] [5 4 10] [4 8 10]]
   [[6 9 5] [6 11 9] [11 8 9]]
   [[3 6 11] [0 6 3] [0 5 6] [0 9 5]]
   [[0 11 8] [0 5 11] [0 1 5] [5 6 11]]
   [[6 11 3] [6 3 5] [5 3 1]]
   [[1 2 10] [9 5 11] [9 11 8] [11 5 6]]
   [[0 11 3] [0 6 11] [0 9 6] [5 6 9] [1 2 10]]
   [[11 8 5] [11 5 6] [8 0 5] [10 5 2] [0 2 5]]
   [[6 11 3] [6 3 5] [2 10 3] [10 5 3]]
   [[5 8 9] [5 2 8] [5 6 2] [3 8 2]]
   [[9 5 6] [9 6 0] [0 6 2]]
   [[1 5 8] [1 8 0] [5 6 8] [3 8 2] [6 2 8]]
   [[1 5 6] [2 1 6]]
   [[1 3 6] [1 6 10] [3 8 6] [5 6 9] [8 9 6]]
   [[10 1 0] [10 0 6] [9 5 0] [5 6 0]]
   [[0 3 8] [5 6 10]]
   [[10 5 6]]
   [[11 5 10] [7 5 11]]
   [[11 5 10] [11 7 5] [8 3 0]]
   [[5 11 7] [5 10 11] [1 9 0]]
   [[10 7 5] [10 11 7] [9 8 1] [8 3 1]]
   [[11 1 2] [11 7 1] [7 5 1]]
   [[0 8 3] [1 2 7] [1 7 5] [7 2 11]]
   [[9 7 5] [9 2 7] [9 0 2] [2 11 7]]
   [[7 5 2] [7 2 11] [5 9 2] [3 2 8] [9 8 2]]
   [[2 5 10] [2 3 5] [3 7 5]]
   [[8 2 0] [8 5 2] [8 7 5] [10 2 5]]
   [[9 0 1] [5 10 3] [5 3 7] [3 10 2]]
   [[9 8 2] [9 2 1] [8 7 2] [10 2 5] [7 5 2]]
   [[1 3 5] [3 7 5]]
   [[0 8 7] [0 7 1] [1 7 5]]
   [[9 0 3] [9 3 5] [5 3 7]]
   [[9 8 7] [5 9 7]]
   [[5 8 4] [5 10 8] [10 11 8]]
   [[5 0 4] [5 11 0] [5 10 11] [11 3 0]]
   [[0 1 9] [8 4 10] [8 10 11] [10 4 5]]
   [[10 11 4] [10 4 5] [11 3 4] [9 4 1] [3 1 4]]
   [[2 5 1] [2 8 5] [2 11 8] [4 5 8]]
   [[0 4 11] [0 11 3] [4 5 11] [2 11 1] [5 1 11]]
   [[0 2 5] [0 5 9] [2 11 5] [4 5 8] [11 8 5]]
   [[9 4 5] [2 11 3]]
   [[2 5 10] [3 5 2] [3 4 5] [3 8 4]]
   [[5 10 2] [5 2 4] [4 2 0]]
   [[3 10 2] [3 5 10] [3 8 5] [4 5 8] [0 1 9]]
   [[5 10 2] [5 2 4] [1 9 2] [9 4 2]]
   [[8 4 5] [8 5 3] [3 5 1]]
   [[0 4 5] [1 0 5]]
   [[8 4 5] [8 5 3] [9 0 5] [0 3 5]]
   [[9 4 5]]
   [[4 11 7] [4 9 11] [9 10 11]]
   [[0 8 3] [4 9 7] [9 11 7] [9 10 11]]
   [[1 10 11] [1 11 4] [1 4 0] [7 4 11]]
   [[3 1 4] [3 4 8] [1 10 4] [7 4 11] [10 11 4]]
   [[4 11 7] [9 11 4] [9 2 11] [9 1 2]]
   [[9 7 4] [9 11 7] [9 1 11] [2 11 1] [0 8 3]]
   [[11 7 4] [11 4 2] [2 4 0]]
   [[11 7 4] [11 4 2] [8 3 4] [3 2 4]]
   [[2 9 10] [2 7 9] [2 3 7] [7 4 9]]
   [[9 10 7] [9 7 4] [10 2 7] [8 7 0] [2 0 7]]
   [[3 7 10] [3 10 2] [7 4 10] [1 10 0] [4 0 10]]
   [[1 10 2] [8 7 4]]
   [[4 9 1] [4 1 7] [7 1 3]]
   [[4 9 1] [4 1 7] [0 8 1] [8 7 1]]
   [[4 0 3] [7 4 3]]
   [[4 8 7]]
   [[9 10 8] [10 11 8]]
   [[3 0 9] [3 9 11] [11 9 10]]
   [[0 1 10] [0 10 8] [8 10 11]]
   [[3 1 10] [11 3 10]]
   [[1 2 11] [1 11 9] [9 11 8]]
   [[3 0 9] [3 9 11] [1 2 9] [2 11 9]]
   [[0 2 11] [8 0 11]]
   [[3 2 11]]
   [[2 3 8] [2 8 10] [10 8 9]]
   [[9 10 2] [0 9 2]]
   [[2 3 8] [2 8 10] [0 1 8] [1 10 8]]
   [[1 10 2]]
   [[1 3 8] [9 1 8]]
   [[0 9 1]]
   [[0 3 8]]
   []])

;; Edges per cube index

(def compute-edges
  [0 7 1 6 0 7 1 6 4 3 5 2 4 3 5 2
   2 5 3 4 2 5 3 4 6 1 7 0 6 1 7 0
   0 7 1 6 0 7 1 6 4 3 5 2 4 3 5 2
   2 5 3 4 2 5 3 4 6 1 7 0 6 1 7 0
   0 7 1 6 0 7 1 6 4 3 5 2 4 3 5 2
   2 5 3 4 2 5 3 4 6 1 7 0 6 1 7 0
   0 7 1 6 0 7 1 6 4 3 5 2 4 3 5 2
   2 5 3 4 2 5 3 4 6 1 7 0 6 1 7 0
   0 7 1 6 0 7 1 6 4 3 5 2 4 3 5 2
   2 5 3 4 2 5 3 4 6 1 7 0 6 1 7 0
   0 7 1 6 0 7 1 6 4 3 5 2 4 3 5 2
   2 5 3 4 2 5 3 4 6 1 7 0 6 1 7 0
   0 7 1 6 0 7 1 6 4 3 5 2 4 3 5 2
   2 5 3 4 2 5 3 4 6 1 7 0 6 1 7 0
   0 7 1 6 0 7 1 6 4 3 5 2 4 3 5 2
   2 5 3 4 2 5 3 4 6 1 7 0 6 1 7 0])

;; Voxel lookups

(defn voxel-id-front
  (^long
   [voxels {:keys [stride stride-z]} idx]
   (let [y2 (+ idx stride), z2 (+ idx stride-z), yz (+ z2 stride)
         idx1 (inc idx), y21 (inc y2), z21 (inc z2), yz1 (inc yz)]
     (not-cond->
      0
      (voxels idx)  (bit-or 0x01)
      (voxels idx1) (bit-or 0x02)
      (voxels z21)  (bit-or 0x04)
      (voxels z2)   (bit-or 0x08)
      (voxels y2)   (bit-or 0x10)
      (voxels y21)  (bit-or 0x20)
      (voxels yz1)  (bit-or 0x40)
      (voxels yz)   (bit-or 0x80)))))

(defn voxel-id-back
  (^long
   [voxels {:keys [stride stride-z]} idx]
   (let [y2 (- idx stride), z2 (- idx stride-z), yz (- z2 stride)
         idx1 (dec idx), y21 (dec y2), z21 (dec z2), yz1 (dec yz)]
     (not-cond->
      0
      (voxels idx)  (bit-or 0x01)
      (voxels idx1) (bit-or 0x02)
      (voxels z21)  (bit-or 0x04)
      (voxels z2)   (bit-or 0x08)
      (voxels y2)   (bit-or 0x10)
      (voxels y21)  (bit-or 0x20)
      (voxels yz1)  (bit-or 0x40)
      (voxels yz)   (bit-or 0x80)))))

(defn boundary-voxel-checker
  [f config cells]
  (fn [v] (let [id (f cells config v)] (if (> id 0) (< id 0xff)))))

(defn boundary-voxels
  [config cells]
  (let [front (boundary-voxel-checker voxel-id-front config cells)
        back  (boundary-voxel-checker voxel-id-back config cells)]
    (eduction (filter #(if (front %) true (back %))) cells)))

(defn thicken-boundary
  [offsets cells]
  (into #{} (mapcat (fn [v] (eduction (map #(+ v %)) offsets))) cells))

(defn precompute-cells
  [voxels config cells]
  (for [idx cells
        :let [id (voxel-id-front voxels config idx)]
        :when (if (> id 0) (< id 0xff))]
    [id (* 3 idx) idx (voxel-cell config idx)]))

;; Isosurface computation

(defn- cell-vertice-builder
  [size iso iso*]
  (fn [vertices cell]
    (let [eflags (int (compute-edges (first cell)))]
      (if (> eflags 0)
        (let [[voxel-id vid idx cell] cell
              vid1     (inc vid)
              vid2     (inc vid1)
              delta    (if (zero? (bit-and voxel-id 1)) iso* iso)
              [x y z]  (m/* cell size)
              vertices (if (> (bit-and eflags 0x01) 0)
                         (if (nil? (vertices vid))
                           (assoc! vertices vid
                                   (vec3
                                    (if (zero? (bit-and voxel-id 0x02))
                                      (+ x delta)
                                      (- x delta))
                                    y
                                    z))
                           vertices)
                         vertices)
              vertices (if (> (bit-and eflags 0x02) 0)
                         (if (nil? (vertices vid1))
                           (assoc! vertices vid1
                                   (vec3
                                    x
                                    (if (zero? (bit-and voxel-id 0x10))
                                      (+ y delta)
                                      (- y delta))
                                    z))
                           vertices)
                         vertices)]
          (if (> (bit-and eflags 0x04) 0)
            (if (nil? (vertices vid2))
              (assoc! vertices vid2
                      (vec3
                       x
                       y
                       (if (zero? (bit-and voxel-id 0x08))
                         (+ z delta)
                         (- z delta))))
              vertices)
            vertices))
        vertices))))

(defn surface-faces
  "Computes mesh faces of a voxel tree's iso surface at the given tree depth and iso value
  (between 0.0 ... 1.0)"
  [{:keys [dim maxdepth] :as tree} depth iso-val]
  (let [{:keys [depth size stride stride-z] :as config} (voxel-config-at-depth tree depth)
        kernel [-1 0 1]
        offsets (vec (for [z kernel y kernel x kernel] (cell-index stride stride-z x y z)))
        indexed-eo (mapv (fn [[x y z w]]
                           (+ (* 3 (cell-index stride stride-z x y z)) w)) edge-offsets)
        iso (* size iso-val)
        iso* (- iso size)
        voxels (select-cells tree depth)
        ;; select boundary voxels
        cells (->> voxels
                   (boundary-voxels config)
                   (thicken-boundary offsets)
                   (precompute-cells voxels config))
        vertices (persistent! (reduce (cell-vertice-builder size iso iso*) (transient {}) cells))]
    (eduction
     (mapcat
      (fn [[vid eid]]
        (eduction
         (map
          (fn [t]
            [[(vertices (+ eid (indexed-eo (t 0))))
              (vertices (+ eid (indexed-eo (t 2))))
              (vertices (+ eid (indexed-eo (t 1))))]]))
         (cell-triangles vid))))
     cells)))

(defn surface-mesh
  "Computes a triangle mesh of a voxel tree's iso surface at the given tree depth and iso value
  (between 0.0 ... 1.0)"
  [tree depth iso-val]
  (g/into (bm/basic-mesh) (surface-faces tree depth iso-val)))

(extend-type thi.ng.geom.voxel.tree.SVO
  g/IMeshConvert
  (as-mesh
    ([tree] (g/as-mesh tree {}))
    ([tree {:keys [mesh attribs depth iso-value] :as options}]
     (let [faces (sequence (surface-faces tree depth (or iso-value 0.5)))
           target-mesh (or mesh (glm/gl-mesh (count faces) (set (keys attribs))))
           attr-fn (fn [i [verts]] (attr/generate-face-attribs verts i attribs options))
           attributed-faces (map-indexed attr-fn faces)]
       (g/into target-mesh attributed-faces)))))
