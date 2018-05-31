(ns thi.ng.geom.basicmesh
  (:require
   [thi.ng.geom.core :as g]
   [thi.ng.geom.utils :as gu]
   [thi.ng.geom.vector :as v :refer [vec2 vec3]]
   [thi.ng.geom.matrix :refer [M44]]
   [thi.ng.geom.meshface :as mf]
   #?(:clj [thi.ng.geom.types] :cljs [thi.ng.geom.types :refer [BasicMesh]])
   [thi.ng.dstruct.core :as d]
   [thi.ng.math.core :as m :refer [*eps*]]
   [thi.ng.xerror.core :as err]
   [clojure.core.reducers :as r])
   #?(:clj (:import [thi.ng.geom.types BasicMesh])))

(declare basic-mesh)

(defn- add-face*
  [mesh [fverts]]
  (BasicMesh.
   (into (get mesh :vertices) fverts)
   (conj (get mesh :faces) (thi.ng.geom.meshface.MeshFace. fverts nil))
   (get mesh :fnormals)))

(defn basic-mesh
  "Builds a new 3d mesh data structure and (optionally) populates it with
  the given items (a seq of existing meshes and/or faces). Faces are defined
  as vectors of their vertices."
  [] (BasicMesh. #{} #{} {}))

(extend-type BasicMesh

  g/IArea
  (area
    [_] (gu/total-area-3d (mf/xf-face-verts _) (get _ :faces)))

  g/IBounds
  (bounds [_] (gu/bounding-box (seq (get _ :vertices))))
  (width [_]  (gu/axis-range 0 (get _ :vertices)))
  (height [_] (gu/axis-range 1 (get _ :vertices)))
  (depth [_]  (gu/axis-range 2 (get _ :vertices)))

  g/IBoundingSphere
  (bounding-sphere
    [_] (gu/bounding-sphere (g/centroid _) (get _ :vertices)))

  g/ICenter
  (center
    ([_]   (g/center _ (vec3)))
    ([_ o] (g/translate _ (m/- o (g/centroid _)))))
  (centroid
    [_]    (gu/centroid (seq (get _ :vertices))))

  g/IFlip
  (flip [_] (gu/map-mesh (fn [f] [(vec (rseq f))]) _))

  g/IVertexAccess
  (vertices
    [_] (get _ :vertices))

  g/IEdgeAccess
  (edges
    [_]
    (into
     #{}
     (comp
      (map #(g/vertices % _))
      (mapcat #(d/successive-nth 2 (conj % (first %))))
      (map set))
     (get _ :faces)))

  g/IFaceAccess
  (faces
    ([_] (get _ :faces))
    ([_ opts]
     (if opts
       (map #(g/raw % _) (get _ :faces))
       (get _ :faces))))
  (add-face
    [_ face] (add-face* _ face))
  (vertex-faces
    [_ v]
    (sequence
     (comp
      (map #(g/vertices % _))
      (filter
       #(pos? #?(:clj (.indexOf ^clojure.lang.PersistentVector % v)
                 :cljs (d/index-of % v))))
      (get _ :faces))))
  (remove-face
    [_ f]
    (err/unsupported!)) ;; TODO implement

  g/INormalAccess
  (face-normals
    [_ force?] (if (seq (get _ :fnormals)) (get _ :fnormals) (if force? (get (g/compute-face-normals _) :fnormals))))
  (face-normal
    [_ f] (get (get _ :fnormals) f))
  (vertex-normals
    [_ force?] (if force? (err/unsupported!)))
  (vertex-normal
    [_ v] (err/unsupported!))
  (compute-face-normals
    [_]
    (loop [fnorms (transient {}), faces (get _ :faces)]
      (if faces
        (let [f (first faces)]
          (recur (assoc! fnorms f (gu/ortho-normal (g/vertices f _))) (next faces)))
        (assoc _ :fnormals (persistent! fnorms)))))
  (compute-vertex-normals
    [_] (err/unsupported!))

  g/IGeomContainer
  (into
    [_ faces] (gu/into-mesh _ add-face* faces))

  g/IClear
  (clear*
    [_] (basic-mesh))

  g/IMeshConvert
  (as-mesh
    ([_] _)
    ([_ opts] (g/into (get opts :mesh) (get _ :faces))))

  g/ITessellate
  (tessellate
    ([_]      (g/tessellate _ {}))
    ([_ opts] (gu/map-mesh (or (get opts :fn) (gu/tessellate-face gu/tessellate-with-first)) _)))

  g/IScale
  (scale
    [_ s] (gu/transform-mesh _ add-face* #(m/* % s)))
  (scale-size
    [_ s]
    (let [c (g/centroid _)]
      (gu/transform-mesh _ add-face* #(m/madd (m/- % c) s c))))

  g/ITranslate
  (translate
    [_ t] (gu/transform-mesh _ add-face* #(m/+ % t)))

  g/ITransform
  (transform
    [_ m]
    (gu/transform-mesh _ add-face* m))

  g/IVolume
  (volume
    [_] (gu/total-volume (mf/xf-face-verts _) (get _ :faces))))
