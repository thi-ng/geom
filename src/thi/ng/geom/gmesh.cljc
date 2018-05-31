(ns thi.ng.geom.gmesh
  (:require
   [thi.ng.geom.core :as g]
   [thi.ng.geom.utils :as gu]
   [thi.ng.geom.vector :as v :refer [vec2 vec3]]
   [thi.ng.geom.matrix :refer [M44]]
   [thi.ng.geom.meshface :as mf]
   #?(:clj [thi.ng.geom.types] :cljs [thi.ng.geom.types :refer [AABB GMesh]])
   [thi.ng.dstruct.core :as d]
   [thi.ng.math.core :as m :refer [*eps*]]
   [clojure.core.reducers :as r]
   [clojure.set :as set])
   #?(:clj (:import [thi.ng.geom.types AABB GMesh])))

(defn- add-face*
  [{:keys [vertices edges faces] :as mesh} [verts]]
  (let [f (mapv #(get (find vertices %) 0 %) verts)]
    (if (and (nil? (get faces f))
             (= (count f) (count (set f))))
      (let [mf       (thi.ng.geom.meshface.MeshFace. f nil)
            vertices (->> (d/wrap-seq f [(peek f)] [(first f)])
                          (partition 3 1)
                          (reduce
                           (fn [acc [p c n]]
                             (d/index-kv acc c {:next n :prev p :f mf}))
                           vertices))
            edges    (->> (conj f (first f))
                          (partition 2 1)
                          (reduce
                           (fn [acc pair] (d/index-kv acc (set pair) mf))
                           edges))]
        (assoc mesh
               :vertices vertices
               :edges edges
               :faces (conj faces mf)))
      mesh)))

(defn vertices-planar?
  [[a b c :as verts]]
  (or (< (count verts) 4)
      (let [n (gu/ortho-normal a b c)]
        (every? #(m/delta= n (gu/ortho-normal %))
                (partition 3 1 (conj (rest verts) a))))))

(defn face-neighbors-shared-edges
  [{:keys [edges]} f]
  (->> (conj f (first f))
       (partition 2 1)
       (reduce
        (fn [acc pair] (into acc (-> pair set edges (disj f))))
        [])))

(defn vertex-neighbors*
  [{vertices :vertices} v]
  (set/union
   (d/value-set :next vertices v)
   (d/value-set :prev vertices v)))

(defn vertex-valence*
  [mesh v] (-> mesh (get :vertices) (get v) count inc))

(defn vertex-faces*
  [mesh v] (d/value-set :f (get mesh :vertices) v))

(defn remove-vertex*
  [mesh v]
  (if (find (get mesh :vertices) v)
    (reduce g/remove-face mesh (vertex-faces* mesh v))
    mesh))

(defn replace-vertex*
  ([mesh v v2]
   (let [vfaces (vertex-faces* mesh v)]
     (-> (reduce g/remove-face mesh vfaces)
         (replace-vertex* v v2 vfaces))))
  ([mesh v v2 faces]
   (reduce #(add-face* % (replace {v v2} %2)) mesh faces)))

(defn merge-vertices*
  [mesh a b]
  (if ((vertex-neighbors* mesh a) b)
    (let [fa (vertex-faces* mesh a) fb (vertex-faces* mesh b)
          ab-isec (set/intersection fa fb)
          a-xor (set/difference fa ab-isec)
          b-xor (set/difference fb ab-isec)
          mp (m/mix a b)]
      (-> (reduce g/remove-face mesh (set/union ab-isec a-xor b-xor))
          (replace-vertex* a mp a-xor)
          (replace-vertex* b mp b-xor)))
    mesh))

(defn gmesh
  "Builds a new 3d mesh data structure and (optionally) populates it with
  the given items (a seq of existing meshes and/or faces). Faces are defined
  as vectors of their vertices."
  [] (GMesh. {} #{} {} {} {} #{}))

;; *** Creating a mesh from a lathe
;;
;; Revolving a seq of points (usually all in a plane) around an axis
;; is a classic and very powerful way to construct a
;; symmetrical/cylindrical 3d mesh. The =lathe-mesh= function does
;; exactly this (but in a more flexible way): It takes a seq of
;; =Vec3='s and treats them as lathe to form a mesh. The lathe is
;; revolved around an axis using a given rotation fn and radial extend
;; & resolution. The rotation fn must accept two args: a point &
;; rotation angle. Apart from =rotate-around-axis= all other methods
;; of the =PRotate3D= protocol (implemented by =Vec3=) satisfy this
;; condition. To use =rotate-around-axis= as rotation fn, it needs to
;; be wrapped in a closure with the axis pre-configured.
;;
;; The =lathe-mesh= fn first divides the given revolution angle Φ
;; (phi) by the given resolution and creates a seq of rotated point
;; strips. It then proceeds to form faces between pairs of these
;; strips. If face points lie on the rotation axis, triangles will be
;; created or faces skipped entirely to avoid degenerate meshes.
;; Optionally, each resulting face can also be transformed before
;; being added to the mesh (e.g. scaled to create gaps or subdivide).
;; This face transform fn should accept a seq of points and return a
;; seq of faces (or return nil to skip a face).
;;
;; The rotation fn too can be used to not just rotate a point. E.g.
;; Scaling points based on Θ (the rotation angle) post-rotation can
;; produce very interesting results. The example function below does
;; this for a lathe defined in the XZ plane and centered around the
;; Z-axis:
;;
;; ```
;; (fn [p theta]
;;   (let [s (inc (* (Math/sin (* theta 6)) 0.25))]
;;     (-> p
;;         (g/rotate-z theta) ;; first rotate
;;         (m/* s s 1.0))))   ;; then scale in XY plane only
;; ```
;;
;; Finally, in order to create a fully closed mesh, the revolution
;; angle Φ must be 2*Π and the first and last points of the lathe seq
;; must be located on the rotation axis.

(defn lathe-mesh
  ([points res phi rot-fn]
   (lathe-mesh points res phi rot-fn nil))
  ([points res phi rot-fn face-fn]
   (let [strips    (mapv
                    (fn [i]
                      (let [theta (* i phi)]
                        (mapv #(let [p (rot-fn % theta)]
                                 (if (m/delta= p % *eps*)
                                   % p))
                              points)))
                    (butlast (m/norm-range res)))
         strips    (if (m/delta= phi m/TWO_PI)
                     (conj strips (first strips))
                     strips)
         make-face (fn [[a1 a2] [b1 b2]] ;; TODO add attrib support
                     (let [f (cond
                               (< (count (hash-set a1 a2 b1 b2)) 3) nil
                               (= a1 b1) [b1 b2 a2]
                               (= a2 b2) [b1 a2 a1]
                               :default [b1 b2 a2 a1])]
                       [(if (and f face-fn) (face-fn f) [f])]))]
     (->> (partition 2 1 strips)
          (mapcat ;; TODO transduce
           (fn [[sa sb]]
             (mapcat make-face
                     (partition 2 1 sa)
                     (partition 2 1 sb))))
          (gu/into-mesh (gmesh) add-face*)))))

(defn saddle
  [s]
  (let [sv (vec3 s)]
    (reduce
     (fn [m [p flags]]
       (gu/into-mesh m add-face* (g/as-mesh (AABB. p s) {:flags flags})))
     (gmesh)
     [[(vec3) :ewsfb]
      [(vec3 0 s 0) :wfb]
      [(vec3 s s 0) :ensfb]
      [(vec3 0 (* s 2) 0) :ewnfb]])))

(extend-type GMesh

  g/IArea
  (area
    [_] (gu/total-area-3d (mf/xf-face-verts _) (get _ :faces)))

  g/IBounds
  (bounds [_] (gu/bounding-box (keys (get _ :vertices))))
  (width [_] (gu/axis-range 0 (keys (get _ :vertices))))
  (height [_] (gu/axis-range 1 (keys (get _ :vertices))))
  (depth [_] (gu/axis-range 2 (keys (get _ :vertices))))

  g/IBoundingSphere
  (bounding-sphere
    [_] (gu/bounding-sphere (g/centroid _) (g/vertices _)))

  g/ICenter
  (center
    ([_] (g/center _ (vec3)))
    ([_ o] (g/translate _ (m/- o (g/centroid _)))))
  (centroid
    [_] (gu/centroid (keys (get _ :vertices))))

  g/IFlip
  (flip [_] (gu/map-mesh (fn [f] [(vec (rseq f))]) _))

  g/IGraph
  (connected-components
    [_] [_]) ;; TODO
  (vertex-neighbors
    [_ v] (vertex-neighbors* _ v))
  (vertex-valence
    [_ v] (vertex-valence* _ v))
  (remove-vertex
    [_ v] (remove-vertex* _ v))
  (replace-vertex
    [_ v v2] (replace-vertex* _ v v2))
  (merge-vertices
    [_ a b] (merge-vertices* _ a b))

  g/IVertexAccess
  (vertices
    [_] (keys (get _ :vertices)))

  g/IEdgeAccess
  (edges
    [_] (keys (get _ :edges)))

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
    [_ v] (vertex-faces* _ v))
  (remove-face
    [{:keys [vertices edges faces fnormals vnormals] :as _} f]
    (if (get faces f)
      (let [fv (g/vertices f _)]
        (loop [verts  vertices
               vnorms vnormals
               edges  edges
               fedges (partition 2 1 (conj fv (first fv)))]
          (if fedges
            (let [[a b]  (first fedges)
                  e      #{a b}
                  efaces (disj (get edges e) f)
                  edges  (if (seq efaces)
                           (assoc edges e efaces)
                           (dissoc edges e))
                  ve     (filter #(not= (get % :f) f) (get verts a))]
              (if (seq ve)
                (recur (assoc verts a (into #{} ve)) vnorms edges (next fedges))
                (recur (dissoc verts a) (dissoc vnorms a) edges (next fedges))))
            (assoc _
                   :vertices verts
                   :vnormals vnorms
                   :edges edges
                   :faces (disj faces f)
                   :fnormals (dissoc fnormals f)))))
      _))

  g/INormalAccess
  (face-normals
    [_ force?]
    (if (seq (get _ :fnormals))
      (get _ :fnormals)
      (if force? (get (g/compute-face-normals _) :fnormals))))
  (face-normal
    [_ f] (get (get _ :fnormals) f))
  (vertex-normals
    [_ force?]
    (if (seq (get _ :vnormals))
      (get _ :vnormals)
      (if force? (get (g/compute-vertex-normals _) :vnormals))))
  (vertex-normal
    [_ v] (get (get _ :vnormals) v))
  (compute-face-normals
    [_]
    (loop [norms (transient #{}), fnorms (transient (hash-map)), faces (get _ :faces)]
      (if faces
        (let [^thi.ng.geom.meshface.MeshFace f (first faces)
              [norms n] (d/index! norms (gu/ortho-normal (.-vertices f)))]
          (recur norms (assoc! fnorms f n) (next faces)))
        (assoc _
               :normals  (persistent! norms)
               :fnormals (persistent! fnorms)))))
  (compute-vertex-normals
    [_]
    (let [this (if (seq (get _ :fnormals)) _ (g/compute-face-normals _))
          {:keys [vertices normals fnormals]} this
          ntx (map #(get fnormals %))]
      (loop [norms (transient normals), vnorms (transient (hash-map)), verts (keys vertices)]
        (if verts
          (let [v (first verts)
                [norms n] (->> (d/value-set :f vertices v)
                               (transduce ntx m/+ v/V3)
                               (m/normalize)
                               (d/index! norms))]
            (recur norms (assoc! vnorms v n) (next verts)))
          (assoc this
                 :normals  (persistent! norms)
                 :vnormals (persistent! vnorms))))))

  g/IGeomContainer
  (into
    [_ faces] (gu/into-mesh _ add-face* faces))

  g/IClear
  (clear*
    [_] (gmesh))

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
    [_ tx]
    (gu/transform-mesh _ add-face* tx))

  g/IVolume
  (volume
    [_] (gu/total-volume (mf/xf-face-verts _) (get _ :faces))))
