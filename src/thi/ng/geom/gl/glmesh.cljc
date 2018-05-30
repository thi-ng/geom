(ns thi.ng.geom.gl.glmesh
  #?(:clj
     (:import
      [java.nio Buffer FloatBuffer ShortBuffer IntBuffer]))
  (:require
   [thi.ng.math.core :as m]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.vector :as v :refer [vec2 vec3]]
   [thi.ng.geom.utils :as gu]
   [thi.ng.geom.gl.core :as gl]
   [thi.ng.dstruct.streams :as streams]
   [thi.ng.xerror.core :as err]
   #?@(:clj
       [[thi.ng.geom.gl.jogl.buffers :as native]
        [thi.ng.geom.gl.jogl.constants :as glc]]
       :cljs
       [[thi.ng.geom.gl.webgl.constants :as glc]
        [thi.ng.typedarrays.core :as ta]])))

;; This namespace provides speed optimized, *mutable* mesh
;; implementations primarily intended for display purposes via OpenGL
;; / WebGL. Both the `GLMesh` and `IndexedGLMesh` types have the
;; following features:
;;
;; - optional vertex attributes: face normals, vertex normals,
;;   texture coordinates & colors
;; - native storage in `java.nio` buffers or JS typed arrays
;; - automatic triangle tessellation when adding mesh faces
;; - automatic face normal calculation when adding faces (and face
;;   normals are enabled)
;; - customizable attribute names when calling `gl/as-gl-buffer-spec`
;;   (see further below)
;;
;; The `IndexedGLMesh` furthermore does automatic indexing based on
;; unique vertex values (and their attributes) and therefore reduces
;; the amount of data needing to be sent and processed by the GPU.
;;
;; On the other hand the `GLMesh` builds flattened attribute buffers,
;; is faster to construct and too allows for bigger meshes, since the
;; 16bit element index limitation of OpenGL does not apply here.
;;
;; Limitations
;;
;; - Meshes are not resizable (due to fixed length buffers)
;; - Max. 65536 unique vertices (OpenGL limitation, `IndexedGLMesh` only)
;; - No support for vertex normal calculation. V-normals must be pre-assigned
;; - Currently only partial support for mesh analysis and transformations
;;
;; Constructors
;;
;; Since the meshes are built on JS typed arrays, the number of faces
;; must be known at construction time. This doesn't need to (but
;; generally should) be the exact number, as long as it's at least the
;; number of faces going to be added. The meshes internally keep track
;; of the actual elements used, but *do no bounds checking*.
;;
;; The second arg given is an optional set of vertex attribute buffer
;; IDs (see tables under `IGLConvert` further below for details).

(declare gl-mesh indexed-gl-mesh)

(declare add-face* add-face-indexed* into-glmesh* into-iglmesh* transform-vertices)

(deftype GLMesh
    #?(:clj
       [^FloatBuffer vertices
        ^FloatBuffer fnormals
        ^FloatBuffer vnormals
        ^FloatBuffer uvs
        ^FloatBuffer cols
        ^:unsynchronized-mutable id
        attribs]
       :cljs
       [^js/Float32Array vertices
        ^js/Float32Array fnormals
        ^js/Float32Array vnormals
        ^js/Float32Array uvs
        ^js/Float32Array cols
        ^:mutable id
        attribs])

  g/IBounds
  (bounds [_] (gu/bounding-box (seq (g/vertices _))))
  (width [_] (gu/axis-range 0 (seq (g/vertices _))))
  (height [_] (gu/axis-range 1 (seq (g/vertices _))))
  (depth [_] (gu/axis-range 2 (seq (g/vertices _))))

  g/IBoundingSphere
  (bounding-sphere
    [_] (gu/bounding-sphere (g/centroid _) (seq (g/vertices _))))

  g/ICenter
  (center
    [_] (g/center _ (vec3)))
  (center
    [_ o] (g/translate _ (m/- o (g/centroid _))))
  (centroid
    [_] (gu/centroid (g/vertices _)))

  g/IVertexAccess
  (vertices
    [_]
    (let [num (* id 9)]
      (loop [acc (transient #{}), i 0]
        (if (< i num)
          #?(:clj
             (recur
              (conj! acc (vec3 (.get vertices i) (.get vertices (+ i 1)) (.get vertices (+ i 2))))
              (+ i 3))
             :cljs
             (recur
              (conj! acc (thi.ng.geom.vector.Vec3. (.slice vertices i (+ i 3)) nil nil))
              (+ i 3)))
          (persistent! acc)))))

  g/IFaceAccess
  (add-face
    [_ f] (add-face* _ f))
  (faces
    [_] (err/unsupported!))
  (faces
    [_ opts] (err/unsupported!))

  g/IGeomContainer
  (into
    [_ faces]
    (if (instance? GLMesh faces)
      (into-glmesh* _ faces)
      (gu/into-mesh _ add-face* faces)))

  g/IClear
  (clear*
    [_] (gl-mesh (int (/ #?(:clj (.capacity vertices) :cljs (.-length vertices)) 9)) attribs))
  (clear!
    [_] (set! id 0) _)

  g/IScale
  (scale
    [_ s] (transform-vertices #(m/*! % s) vertices (* id 9)) _)
  (scale-size
    [_ s]
    (let [c (g/centroid _)]
      (transform-vertices #(m/madd! (m/-! % c) s c) vertices (* id 9))) _)

  g/ITranslate
  (translate
    [_ t] (transform-vertices #(m/+! % t) vertices (* id 9)) _)

  g/ITransform
  (transform
    [_ tx] (transform-vertices #(g/transform-vector tx %) vertices (* id 9)) _)

  gl/IGLConvert
  (as-gl-buffer-spec
    [_ {:keys [attribs] :as opts}]
    {:attribs      (cond->    {(get attribs :position :position) {:data vertices :size 3}}
                     fnormals (assoc (get attribs :fnorm :normal) {:data fnormals :size 3})
                     vnormals (assoc (get attribs :vnorm :normal) {:data vnormals :size 3})
                     uvs      (assoc (get attribs :uv :uv) {:data uvs :size 2})
                     cols     (assoc (get attribs :col :color) {:data cols :size 4}))
     :num-vertices (or (get opts :num-vertices) (* id 3))
     :mode         (get opts :mode glc/triangles)})

  g/IMutableProps
  (get-prop
    [_ k] (if (= :id k) id (err/illegal-arg! k)))
  (set-prop!
    [_ k v] (if (= :id k) (set! id v) (err/illegal-arg! k)) _))

(deftype IndexedGLMesh
    #?(:clj
       [^FloatBuffer vertices
        ^FloatBuffer fnormals
        ^FloatBuffer vnormals
        ^FloatBuffer uvs
        ^FloatBuffer cols
        ^ShortBuffer indices
        attribs
        ^:unsynchronized-mutable index
        ^:unsynchronized-mutable id
        ^:unsynchronized-mutable fid]
       :cljs
       [^js/Float32Array vertices
        ^js/Float32Array fnormals
        ^js/Float32Array vnormals
        ^js/Float32Array uvs
        ^js/Float32Array cols
        ^js/Uint16Array indices
        attribs
        ^:mutable index
        ^:mutable id
        ^:mutable fid])

  g/IBounds
  (bounds [_] (gu/bounding-box (seq (g/vertices _))))
  (width [_] (gu/axis-range 0 (seq (g/vertices _))))
  (height [_] (gu/axis-range 1 (seq (g/vertices _))))
  (depth [_] (gu/axis-range 2 (seq (g/vertices _))))

  g/IBoundingSphere
  (bounding-sphere
    [_] (gu/bounding-sphere (g/centroid _) (seq (g/vertices _))))

  g/ICenter
  (center
    [_] (g/center _ (vec3)))
  (center
    [_ o] (g/translate _ (m/- o (g/centroid _))))
  (centroid
    [_] (gu/centroid (g/vertices _)))

  ;; The `g/vertices` implementation returns the set of unique
  ;; vertices in the mesh (without their attributes, only positions).

  g/IVertexAccess
  (vertices
    [_]
    (let [num (* id 3)]
      (loop [acc (transient #{}), i 0]
        (if (< i num)
          #?(:clj
             (recur
              (conj! acc (vec3 (.get vertices i) (.get vertices (+ i 1)) (.get vertices (+ i 2))))
              (+ i 3))
             :cljs
             (recur
              (conj! acc (thi.ng.geom.vector.Vec3. (.slice vertices i (+ i 3)) nil nil))
              (+ i 3)))
          (persistent! acc)))))

  g/IFaceAccess
  (add-face
    [_ f] (add-face-indexed* _ f))
  (faces
    [_] (err/unsupported!))
  (faces
    [_ opts] (err/unsupported!))

  g/IGeomContainer
  (into
    [_ m]
    (if (instance? IndexedGLMesh m)
      (into-iglmesh* _ m)
      (gu/into-mesh _ add-face-indexed* m)))

  ;; These protocol methods are used internally by `g/into`.

  g/IClear
  (clear*
    [_] (indexed-gl-mesh (int (/ #?(:clj (.capacity vertices) :cljs (.-length vertices)) 9)) attribs))
  (clear!
    [_] (set! id 0) (set! fid 0) _)

  g/IScale
  (scale
    [_ s] (transform-vertices #(m/*! % s) vertices (* id 3)) _)
  (scale-size
    [_ s]
    (let [c (g/centroid _)]
      (transform-vertices #(m/madd! (m/-! % c) s c) vertices (* id 3))) _)

  g/ITranslate
  (translate
    [_ t] (transform-vertices #(m/+! % t) vertices (* id 3)) _)

  g/ITransform
  (transform
    [_ tx] (transform-vertices #(g/transform-vector tx %) vertices (* id 3)) _)

  ;; Since a `GLMesh` holds all its data in typed arrays, conversion
  ;; into a readily usable format for display purposes is reduced to a
  ;; straight forward re-formatting into GL spec map. By supplying an
  ;; `:attribs` map in the 2nd arg (options map), all mesh attributes
  ;; can be renamed in the spec map. The default attribute names are:
  ;;
  ;; | *Key*       | *Default value* | *Description*     |
  ;; |-------------+-----------------+-------------------|
  ;; | `:position` | `:position`     | vertex positions  |
  ;; | `:fnorm`    | `:normal`       | face normals      |
  ;; | `:vnorm`    | `:normal`       | vertex normals    |
  ;; | `:uv`       | `:uv`           | UV texture coords |
  ;; | `:col`      | `:color`        | vertex colors     |
  ;;
  ;; *Note:* As mentioned above, the `GLMesh` is *not* able to compute
  ;; vertex normals itself.
  ;;
  ;; If a shader requires that an attribute must be renamed in the
  ;; resulting spec map, call the function like this:
  ;;
  ;; rename :position & :uv attribs in result spec:
  ;; ```
  ;; (gl/as-gl-buffer-spec mesh {:attribs {:uv :texcoord, :position :pos}})
  ;; ```
  ;;
  ;; Furthermore, the options map also supports the following other
  ;; keys:
  ;;
  ;; - `:mode` - GL draw mode (default: `glc/triangles`)
  ;; - `:num-vertices` - number of vertices to draw (default: number mesh verts)

  gl/IGLConvert
  (as-gl-buffer-spec
    [_ {:keys [attribs] :as opts}]
    {:attribs      (cond->    {(get attribs :position :position) {:data vertices :size 3}}
                     fnormals (assoc (get attribs :fnorm :normal) {:data fnormals :size 3})
                     vnormals (assoc (get attribs :vnorm :normal) {:data vnormals :size 3})
                     uvs      (assoc (get attribs :uv :uv) {:data uvs :size 2})
                     cols     (assoc (get attribs :col :color) {:data cols :size 4}))
     :indices      {:data indices}
     :num-items    (get opts :num-items fid)
     :num-vertices id
     :mode         (get opts :mode glc/triangles)})

  g/IMutableProps
  (get-prop
    [_ k]
    (case (keyword k)
      :id id
      :fid fid
      :index index
      (err/illegal-arg! k)))
  (set-prop!
    [_ k v]
    (case (keyword k)
      :id (set! id v)
      :fid (set! fid v)
      :index (set! index v)
      (err/illegal-arg! k))
    _)
  (update-prop!
    [_ k f]
    (case (keyword k)
      :id (set! id (f id))
      :fid (set! fid (f fid))
      :index (set! index (f index))
      (err/illegal-arg! k))
    _))

(defn gl-mesh
  "Builds a fixed size mesh with given face count & attribs (a set of
  #{:fnorm :vnorm :uv :col}). All attributes (incl. vertices) are
  stored directly in typed array buffers."
  ([numf] (gl-mesh numf nil))
  ([numf attribs]
   (GLMesh.
    (#?(:clj native/float-buffer-direct :cljs ta/float32) (* numf 9))
    (if (get attribs :fnorm)
      (#?(:clj native/float-buffer-direct :cljs ta/float32) (* numf 9)))
    (if (get attribs :vnorm)
      (#?(:clj native/float-buffer-direct :cljs ta/float32) (* numf 9)))
    (if (get attribs :uv)
      (#?(:clj native/float-buffer-direct :cljs ta/float32) (* numf 6)))
    (if (get attribs :col)
      (#?(:clj native/float-buffer-direct :cljs ta/float32) (* numf 12)))
    0
    attribs)))

(defn indexed-gl-mesh
  "Builds a fixed size, indexed mesh with given face count & optional
  attribs (a set of #{:fnorm :vnorm :uv :col}). All attributes (incl.
  vertices) are stored directly in typed array buffers. Internally
  builds index of unique vertices (position + attribs) and re-uses
  indices where possible."
  ([numf] (indexed-gl-mesh numf nil))
  ([numf attribs]
   (IndexedGLMesh.
    (#?(:clj native/float-buffer-direct :cljs ta/float32) (* numf 9))
    (if (get attribs :fnorm)
      (#?(:clj native/float-buffer-direct :cljs ta/float32) (* numf 9)))
    (if (get attribs :vnorm)
      (#?(:clj native/float-buffer-direct :cljs ta/float32) (* numf 9)))
    (if (get attribs :uv)
      (#?(:clj native/float-buffer-direct :cljs ta/float32) (* numf 6)))
    (if (get attribs :col)
      (#?(:clj native/float-buffer-direct :cljs ta/float32) (* numf 12)))
    (#?(:clj native/short-buffer-direct :cljs ta/uint16) (* numf 3))
    attribs
    {} 0 0)))

(defn- add-face*
  [^GLMesh m [verts attribs :as f]]
  (if (> (count verts) 3)
    (->> f
         ((gu/tessellate-face gu/tessellate-with-first))
         (run! #(add-face* m %)))
    (let [vertices (.-vertices m)
          fnormals (.-fnormals m)
          vnormals (.-vnormals m)
          uvs      (.-uvs m)
          cols     (.-cols m)
          id      #?(:clj (g/get-prop m :id) :cljs (.-id m))
          idv     (* id 9)
          iduv    (* id 6)
          idcol   (* id 12)]
      #?@(:clj
          [(->> idv
                (gl/into-float-buffer-vec3 (first verts) vertices 3)
                (gl/into-float-buffer-vec3 (nth verts 1) vertices 3)
                (gl/into-float-buffer-vec3 (nth verts 2) vertices 3))]
          :cljs
          [(.set vertices (.-buf ^thi.ng.geom.vector.Vec3 (first verts)) idv)
           (.set vertices (.-buf ^thi.ng.geom.vector.Vec3 (nth verts 1)) (+ idv 3))
           (.set vertices (.-buf ^thi.ng.geom.vector.Vec3 (nth verts 2)) (+ idv 6))])
      (when fnormals
        #?(:clj
           (let [n (or (get attribs :fnorm) (gu/ortho-normal verts))]
             (->> idv
                  (gl/into-float-buffer-vec3 n fnormals 3)
                  (gl/into-float-buffer-vec3 n fnormals 3)
                  (gl/into-float-buffer-vec3 n fnormals 3)))
           :cljs
           (let [n  (or (get attribs :fnorm) (gu/ortho-normal verts))
                 nb (.-buf ^thi.ng.geom.vector.Vec3 n)]
             (.set fnormals nb idv)
             (.set fnormals nb (+ idv 3))
             (.set fnormals nb (+ idv 6)))))
      (when-let [vn (if vnormals (get attribs :vnorm))]
        #?@(:clj
            [(->> idv
                  (gl/into-float-buffer-vec3 (first vn) vnormals 3)
                  (gl/into-float-buffer-vec3 (nth vn 1) vnormals 3)
                  (gl/into-float-buffer-vec3 (nth vn 2) vnormals 3))]
            :cljs
            [(.set vnormals (.-buf ^thi.ng.geom.vector.Vec3 (first vn)) idv)
             (.set vnormals (.-buf ^thi.ng.geom.vector.Vec3 (nth vn 1)) (+ idv 3))
             (.set vnormals (.-buf ^thi.ng.geom.vector.Vec3 (nth vn 2)) (+ idv 6))]))
      (when-let [uv (if uvs (get attribs :uv))]
        #?@(:clj
            [(->> iduv
                  (gl/into-float-buffer-vec2 (first uv) uvs 2)
                  (gl/into-float-buffer-vec2 (nth uv 1) uvs 2)
                  (gl/into-float-buffer-vec2 (nth uv 2) uvs 2))]
            :cljs
            [(.set uvs (.-buf ^thi.ng.geom.vector.Vec2 (first uv)) iduv)
             (.set uvs (.-buf ^thi.ng.geom.vector.Vec2 (nth uv 1)) (+ iduv 2))
             (.set uvs (.-buf ^thi.ng.geom.vector.Vec2 (nth uv 2)) (+ iduv 4))]))
      (when-let [col (if cols (get attribs :col))]
        (->> idcol
             (streams/into-float-buffer (first col) cols 4)
             (streams/into-float-buffer (nth col 1) cols 4)
             (streams/into-float-buffer (nth col 2) cols 4)))
      #?(:clj  (g/set-prop! m :id (inc id))
         :cljs (set! (.-id m) (inc id)))))
  m)

(defn- index-vertex*
  [^IndexedGLMesh m va vertices fnormals vnormals cols uvs]
  (or (get (g/get-prop m :index) va)
      (let [index #?(:clj (g/get-prop m :index) :cljs (.-index m))
            id    #?(:clj (g/get-prop m :id) :cljs (.-id m))
            idv   (* id 3)
            [v fn vn col uv] va]
        #?(:clj  (gl/into-float-buffer-vec3 v vertices 3 idv)
           :cljs (.set vertices (.-buf ^thi.ng.geom.vector.Vec3 v) idv))
        (if (if fnormals fn)
          #?(:clj  (gl/into-float-buffer-vec3 fn fnormals 3 idv)
             :cljs (.set fnormals (.-buf ^thi.ng.geom.vector.Vec3 fn) idv))
          (if (if vnormals vn)
            #?(:clj  (gl/into-float-buffer-vec3 vn vnormals 3 idv)
               :cljs (.set vnormals (.-buf ^thi.ng.geom.vector.Vec3 vn) idv))))
        (when (if cols col)
          (streams/into-float-buffer col cols 4 (* id 4)))
        (when (if uvs uv)
          #?(:clj  (gl/into-float-buffer-vec2 uv uvs 2 (* id 2))
             :cljs (.set uvs (.-buf ^thi.ng.geom.vector.Vec2 uv) (* id 2))))
        #?@(:clj
            [(g/set-prop! m :index (assoc index va id))
             (g/set-prop! m :id (inc id))]
            :cljs
            [(set! (.-index m) (assoc index va id))
             (set! (.-id m) (inc id))])
        id)))

(defn- add-face-indexed*
  [^IndexedGLMesh m [verts attribs :as f]]
  (if (> (count verts) 3)
    (->> f
         ((gu/tessellate-face gu/tessellate-with-first))
         (run! #(add-face-indexed* m %)))
    (let [vertices (.-vertices m)
          fnormals (.-fnormals m)
          vnormals (.-vnormals m)
          uvs      (.-uvs m)
          cols     (.-cols m)
          ^ShortBuffer indices (.-indices m)
          fid      #?(:clj (g/get-prop m :fid) :cljs (.-fid m))
          {:keys [vnorm uv col]} attribs
          fnorm (if fnormals (or (get attribs :fnorm) (gu/ortho-normal verts)))]
      (#?(:clj .put :cljs aset) indices fid
                                (index-vertex*
                                 m [(nth verts 0) fnorm (nth vnorm 0 nil) (nth col 0 nil) (nth uv 0 nil)]
                                 vertices fnormals vnormals cols uvs))
      (#?(:clj .put :cljs aset) indices (+ fid 1)
                                (index-vertex*
                                 m [(nth verts 1) fnorm (nth vnorm 1 nil) (nth col 1 nil) (nth uv 1 nil)]
                                 vertices fnormals vnormals cols uvs))
      (#?(:clj .put :cljs aset) indices (+ fid 2)
                                (index-vertex*
                                 m [(nth verts 2) fnorm (nth vnorm 2 nil) (nth col 2 nil) (nth uv 2 nil)]
                                 vertices fnormals vnormals cols uvs))
      #?(:clj (g/set-prop! m :fid (+ fid 3)) :cljs (set! (.-fid m) (+ fid 3)))))
  m)

(defn- transform-vertices
  #?(:clj [f ^FloatBuffer buf num] :cljs [f buf num])
  (let [tv (vec3 0)
        ^doubles tb (.-buf ^thi.ng.geom.vector.Vec3 tv)]
    #?(:clj (.rewind buf))
    (loop [i 0]
      (when (< i num)
        #?@(:clj
            [(aset tb 0 (.get buf i))
             (aset tb 1 (.get buf (unchecked-add-int i 1)))
             (aset tb 2 (.get buf (unchecked-add-int i 2)))
             (gl/into-float-buffer-vec3 (f tv) buf 3 i)]
            :cljs
            [(.set tb (.slice buf i (+ i 3)) 0)
             (.set buf (.-buf ^thi.ng.geom.vector.Vec3 (f tv)) i)])
        (recur (+ i 3))))))

(defn- into-glmesh*
  [^GLMesh dest ^GLMesh src]
  (let [{sverts :vertices sfn :fnormals svn :vnormals scol :cols suv :uvs sid :id} src
        {dverts :vertices dfn :fnormals dvn :vnormals dcol :cols duv :uvs did :id} dest
        sidv   (* sid 9)
        didv   (* did 9)]
    #?(:clj  (native/copy-float-buffer dverts sverts didv 0 sidv)
       :cljs (.set dverts (.slice sverts 0 sidv) didv))
    (when (if dfn sfn)
      #?(:clj  (native/copy-float-buffer dfn sfn didv 0 sidv)
         :cljs (.set dfn (.slice sfn 0 sidv) didv)))
    (when (if dvn svn)
      #?(:clj  (native/copy-float-buffer dvn svn didv 0 sidv)
         :cljs (.set dvn (.slice svn 0 sidv) didv)))
    (when (if dcol scol)
      #?(:clj  (native/copy-float-buffer dcol scol (* did 12) 0 (* sid 12))
         :cljs (.set dcol (.slice scol 0 (* sid 12)) (* did 12))))
    (when (if duv suv)
      #?(:clj  (native/copy-float-buffer duv suv (* did 6) 0 (* sid 6))
         :cljs (.set duv (.slice suv 0 (* sid 6)) (* did 6))))
    #?(:clj (g/set-prop! dest :id (+ did sid)) :cljs (set! (.-id dest) (+ did sid)))
    dest))

(defn- build-rindex
  [dindex sindex start]
  (reduce-kv
   (fn [[idx nid :as s] v id]
     (if (get dindex v) s [(assoc! idx id [nid v]) (inc nid)]))
   [(transient {}) start]
   sindex))

(defn- merge-index
  [dindex rindex]
  (into dindex (map (fn [kv] [(peek (val kv)) (key kv)])) rindex))

(defn- into-iglmesh*
  [^IndexedGLMesh dest ^IndexedGLMesh src]
  (let [{sverts :vertices sfn :fnormals svn :vnormals scol :cols
         suv :uvs sidx :indices sindex :index sid :id sfid :fid} src
        {dverts :vertices dfn :fnormals dvn :vnormals dcol :cols
         duv :uvs didx :indices dindex :index did :id dfid :fid} dest
        [rindex did'] (build-rindex dindex sindex did)
        dindex (merge-index dindex (persistent! rindex))
        sidv   sfid
        fn?    (if dfn sfn)
        vn?    (if dvn svn)
        col?   (if dcol scol)
        uv?    (if duv suv)]
    ;; (debug :rindex rindex)
    ;; (debug :dindex dindex)
    ;; (debug :dfid-old dfid :didv (* did 3))
    ;; TODO implement fast path if no verts can be reused
    (loop [i 0]
      (when (< i sidv)
        (if-let [nid (first (get rindex #?(:clj (.get ^ShortBuffer sidx i) :cljs (aget sidx i))))]
          (let [sid    #?(:clj (.get ^ShortBuffer sidx i) :cljs (aget sidx i))
                sidv   (* sid 3)
                didv   (* nid 3)
                sidcol (* sid 4)
                siduv  (* sid 2)]
            ;;(debug :reindex sid :> nid :dfid (+ dfid i) :didv didv)
            #?(:clj  (.put ^ShortBuffer didx (unchecked-add-int dfid i) nid)
               :cljs (aset didx (+ dfid i) nid))
            #?(:clj  (native/copy-float-buffer-vec3 dverts sverts didv sidv)
               :cljs (.set dverts (.slice sverts sidv (+ sidv 3)) didv))
            (when fn?
              #?(:clj  (native/copy-float-buffer-vec3 dfn sfn didv sidv)
                 :cljs (.set dfn (.slice sfn sidv (+ sidv 3)) didv)))
            (when vn?
              #?(:clj  (native/copy-float-buffer-vec3 dvn svn didv sidv)
                 :cljs (.set dvn (.slice svn sidv (+ sidv 3)) didv)))
            (when col?
              #?(:clj  (native/copy-float-buffer-vec4 dcol scol (* nid 4) sidcol)
                 :cljs (.set dcol (.slice scol sidcol (+ sidcol 4)) (* nid 4))))
            (when uv?
              #?(:clj  (native/copy-float-buffer-vec2 duv suv (* nid 2) siduv)
                 :cljs (.set duv (.slice suv siduv (+ siduv 2)) (* nid 2)))))
          (do ;;(debug :reuse (aget sidx i) :dfid (+ dfid i))
            #?(:clj  (.put ^ShortBuffer didx (.get ^ShortBuffer sidx i))
               :cljs (aset didx (+ dfid i) (aget sidx i)))))
        (recur (inc i))))
    #?@(:clj
        [(g/set-prop! dest :index dindex)
         (g/set-prop! dest :id did')
         (g/set-prop! dest :fid (+ dfid sfid))]
        :cljs
        [(set! (.-index dest) dindex)
         (set! (.-id dest) did')
         (set! (.-fid dest) (+ dfid sfid))])
    dest))
