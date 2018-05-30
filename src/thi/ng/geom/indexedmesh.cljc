(ns thi.ng.geom.indexedmesh
  (:require
   [thi.ng.geom.core :as g]
   [thi.ng.geom.utils :as gu]
   [thi.ng.geom.vector :as v :refer [vec2 vec3]]
   [thi.ng.geom.matrix :refer [M44]]
   [thi.ng.geom.meshface :as mf]
   [thi.ng.geom.types]
   [thi.ng.dstruct.core :as d]
   [thi.ng.dstruct.bidirindex :as idx]
   [thi.ng.math.core :as m :refer [*eps*]]
   [thi.ng.xerror.core :as err]
   [clojure.core.reducers :as r]
   [clojure.set :as set]))

(defn indexed-mesh
  [] (thi.ng.geom.types.IndexedMesh. (idx/monotonic-index) [] {}))

(defn- add-face*
  [mesh [verts attribs]]
  #_(prn :add-face verts :attr attribs)
  (let [[vindex vids] (idx/index-coll (get mesh :vertices) verts)
        [aindex aids] (idx/index-attribs (get mesh :attribs) attribs)]
    (thi.ng.geom.types.IndexedMesh.
     vindex
     (conj (get mesh :faces) (thi.ng.geom.meshface.IndexedMeshFace. vids aids nil))
     aindex)))

(defn- flip-face*
  [[verts attribs]]
  [(vec (rseq verts))
   (reduce-kv (fn [acc k v] (assoc acc k (vec (rseq v)))) attribs attribs)])

(extend-type thi.ng.geom.types.IndexedMesh

  g/IArea
  (area
    [_] (gu/total-area-3d (mf/xf-face-verts _) (get _ :faces)))

  g/IBounds
  (bounds [_] (gu/bounding-box (g/vertices _)))
  (width [_] (gu/axis-range 0 (g/vertices _)))
  (height [_] (gu/axis-range 1 (g/vertices _)))
  (depth [_] (gu/axis-range 2 (g/vertices _)))

  g/IBoundingSphere
  (bounding-sphere
    [_] (gu/bounding-sphere (g/centroid _) (g/vertices _)))

  g/ICenter
  (center
    ([_] (g/center _ (vec3)))
    ([_ o] (g/translate _ (m/- o (g/centroid _)))))
  (centroid
    [_] (gu/centroid (g/vertices _)))

  g/IFlip
  (flip
    [_]
    (thi.ng.geom.types.IndexedMesh.
     (get _ :vertices)
     (mapv flip-face* (get _ :faces))
     (dissoc (get _ :attribs) :fnormals :vnormals)))

  g/IGeomContainer
  (into
    [_ faces] (gu/into-mesh _ add-face* faces))

  g/IClear
  (clear*
    [_] (indexed-mesh))

  g/IVertexAccess
  (vertices
    [_] (-> _ (get :vertices) (get :id->v)))

  g/IEdgeAccess
  (edges
    [_] (err/unsupported!))

  g/IFaceAccess
  (faces
    ([_] (get _ :faces))
    ([_ opts]
     (if opts
       (map #(g/raw % _) (get _ :faces))
       (get _ :faces))))
  (add-face
    [_ f] (add-face* _ f))
  #_(vertex-faces
      [_ v] (vertex-faces* _ v))
  #_(remove-face
      [{:keys [vertices edges faces fnormals vnormals] :as _} f]
      (if (get faces f)
        (loop [verts vertices
               vnorms vnormals
               edges edges
               fedges (d/successive-nth 2 (conj f (first f)))]
          (if fedges
            (let [[a b] (first fedges)
                  e #{a b}
                  efaces (disj (get edges e) f)
                  edges (if (seq efaces)
                          (assoc edges e efaces)
                          (dissoc edges e))
                  ve (filter #(not= (get % :f) f) (get verts a))]
              (if (seq ve)
                (recur (assoc verts a (into #{} ve)) vnorms edges (next fedges))
                (recur (dissoc verts a) (dissoc vnorms a) edges (next fedges))))
            (assoc _
                   :vertices verts
                   :vnormals vnorms
                   :edges edges
                   :faces (disj faces f)
                   :fnormals (dissoc fnormals f))))
        _))

  g/INormalAccess
  (face-normals
    [_ force?]
    (if-let [fns (-> _ (get :attribs) (get :fnorm))]
      (get fns :id->v)
      (if force? (-> (g/compute-face-normals _) (get :attribs) (get :fnorm) (get :id->v)))))
  (face-normal
    [_ f]
    ((-> _ (get :attribs) (get :fnorm) (get :id->v))
     ((.-attribs ^thi.ng.geom.meshface.IndexedMeshFace f) :fnorm)))
  (vertex-normals
    [_ force?] (if force? (err/unsupported!)))
  (vertex-normal
    [_ v] (err/unsupported!))
  (compute-face-normals
    [_]
    (let [vertices (get _ :vertices)]
      (loop [fnorms (idx/monotonic-index), faces' (transient []), faces (get _ :faces)]
        (if faces
          (let [^thi.ng.geom.meshface.IndexedMeshFace f (first faces)
                fverts     (idx/attrib-values vertices (.-vertices f))
                [fnorms n] (idx/index fnorms (gu/ortho-normal fverts))
                fattr      (assoc (.-attribs f) :fnorm n)]
            (recur
             fnorms
             (conj! faces' (thi.ng.geom.meshface.IndexedMeshFace. (.-vertices f) fattr nil))
             (next faces)))
          (thi.ng.geom.types.IndexedMesh.
           vertices
           (persistent! faces')
           (assoc (get _ :attribs) :fnorm fnorms))))))
  (compute-vertex-normals
    [_] (err/unsupported!))

  g/ITessellate
  (tessellate
    ([_]      (g/tessellate _ {:attribs (zipmap (keys (get _ :attribs)) (repeat m/mix))}))
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
    [_ tx]
    (gu/transform-mesh _ add-face* tx))

  g/IVolume
  (volume
    [_] (gu/total-volume (mf/xf-face-verts _) (get _ :faces))))
