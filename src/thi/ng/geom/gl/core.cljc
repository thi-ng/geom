(ns thi.ng.geom.gl.core
  #?(:clj
     (:import
      [com.jogamp.opengl GL GL2 GL3 GL4]
      [com.jogamp.opengl.util GLBuffers]
      [java.nio Buffer DoubleBuffer FloatBuffer ShortBuffer IntBuffer]))
  (:require
   [thi.ng.math.core :as m]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.utils :as gu]
   [thi.ng.geom.vector :as v :refer [vec2 vec3 V3Z]]
   [thi.ng.geom.matrix :as mat]
   [thi.ng.geom.gmesh :as gm]
   [thi.ng.geom.rect :as r]
   #?(:clj [thi.ng.geom.types]
      :cljs [thi.ng.geom.types :refer [LineStrip2 LineStrip3 Polygon2 Rect2 BasicMesh GMesh]])
   [thi.ng.geom.gl.shaders :as sh]
   [thi.ng.color.core :as col]
   [thi.ng.dstruct.streams :as streams]
   [thi.ng.xerror.core :as err]
   #?@(:clj
       [[thi.ng.geom.gl.jogl.buffers :as native]
        [thi.ng.geom.gl.jogl.constants :as glc]]
       :cljs
       [[thi.ng.geom.gl.webgl.constants :as glc]
        [thi.ng.typedarrays.core :as ta]]))
    #?(:clj (:import [thi.ng.geom.types LineStrip2 LineStrip3 Polygon2 Rect2 BasicMesh GMesh])))

(declare into-float-buffer-vec3)

;; This protocol is implemented by types which can produce an
;; OpenGL/WebGL representation of their data. Implementations must
;; create a hash map (from here on called "GL model spec") of the
;; following format:
;;
;; example GL model spec (for Clojure) defining:
;; - 2 attribute buffers defining position & color for 3 vertices
;; - 1 index buffer defining a single triangle (optional)
;; - number of indices (only required if :indices given)
;; - number of vertices
;; - GL draw mode
;;
;; ```
;; {:attribs      {:position {:data (native/float-buffer [0 0 0,   1 0 0,   0 1 0])   :size 3}
;;                 :color    {:data (native/float-buffer [1 0 0 1, 0 1 0 1, 0 0 1 1]) :size 4}}
;;  :indices      {:data (native/short-buffer [0 1 2])}
;;  :num-items    3
;;  :num-vertices 3
;;  :mode         glc/triangles}
;; ```
;;
;; *Important:* The collections for `:data` and `:indices` keys MUST
;; be native buffers (in Clojure) or JS Typed Arrays (Clojurescript).
;; These can be created using helper functions in the
;; `thi.ng.geom.gl.jogl.buffers` or the `thi.ng.typedarrays.core`
;; namespaces respectively.

(defprotocol IGLConvert
  (as-gl-buffer-spec [_ opts]))

(defprotocol IRelease
  (release [_]))

(defprotocol IBind
  (bind [_] [_ opts])
  (unbind [_] [_ opts]))

(defprotocol IConfigure
  (configure [_ opts]))

(defprotocol ITexture
  (set-texture-filter [_ min mag])
  (set-texture-wrap [_ wrap-s wrap-t]))

(defprotocol IFramebuffer
  (set-fbo-color-texture [_ tex])
  (set-fbo-depth-buffer [_ depth-buffer]))

(defn zero-fill-coll
  [coll stride]
  (let [len (count coll)]
    (cond
      (< len stride) (take stride (concat coll (repeat 0)))
      (> len stride) (take stride coll)
      :else          coll)))

#?(:clj
   (defn fill-buffer-vec2
     [^FloatBuffer buf idx v num stride]
     (let [^float vbuf (float-array (.-buf ^thi.ng.geom.vector.Vec2 v))]
       (.position buf 0)
       (loop [num num]
         (when (pos? num)
           (.put buf vbuf)
           (recur (dec num)))))
     buf)
   :cljs
   (defn fill-buffer-vec2
     [buf idx v num stride]
     (let [vbuf (.-buf ^thi.ng.geom.vector.Vec2 v)]
       (loop [idx 0, num num]
         (when (pos? num)
           (.set buf vbuf idx)
           (recur (+ idx stride) (dec num)))))
     buf))

#?(:clj
   (defn fill-buffer-vec3
     [^FloatBuffer buf idx v num stride]
     (let [^floats vbuf (float-array (.-buf ^thi.ng.geom.vector.Vec3 v))]
       (.position buf 0)
       (loop [num num]
         (when (pos? num)
           (.put buf vbuf)
           (recur (dec num)))))
     buf)
   :cljs
   (defn fill-buffer-vec3
     [buf idx v num stride]
     (let [vbuf (.-buf ^thi.ng.geom.vector.Vec3 v)]
       (loop [idx 0, num num]
         (when (pos? num)
           (.set buf vbuf idx)
           (recur (unchecked-add-int idx stride) (dec num)))))
     buf))

#?(:clj
   (defn fill-buffer
     [^FloatBuffer buf idx coll n stride]
     (let [^floats b (float-array (zero-fill-coll coll stride))]
       (.position buf 0)
       (loop [i idx, n n]
         (when (pos? n)
           (.put buf b)
           (recur (unchecked-add-int i stride) (dec n))))
       buf))
   :cljs
   (defn fill-buffer
     [buf idx coll n stride]
     (let [b (ta/float32 (zero-fill-coll coll stride))]
       (loop [i idx, n n]
         (when (pos? n)
           (.set buf b i)
           (recur (+ i stride) (dec n))))
       buf)))

(defn fill-vertex-buffer
  [buf coll stride]
  (loop [i 0, coll (seq coll)]
    (if coll
      (recur (int (streams/into-float-buffer (first coll) buf stride i)) (next coll))
      buf)))

(defn fill-vertex-buffer-faces
  [f buf coll stride]
  (loop [i 0, coll (seq coll)]
    (if coll
      (let [fv (first coll)]
        (recur (->> (f (first fv) buf stride i)
                    (f (nth fv 1) buf stride)
                    (f (nth fv 2) buf stride)
                    (int))
               (next coll)))
      buf)))

(defn face-normals-buffer
  [faces]
  (let [buf (#?(:clj native/float-buffer-direct :cljs ta/float32) (* (count faces) 3 3))]
    (loop [faces faces #?@(:cljs [idx 0])]
      (if faces
        #?(:clj
           (let [^floats nbuf (float-array (.-buf ^thi.ng.geom.vector.Vec3 (gu/ortho-normal (first faces))))]
             (.put buf nbuf)
             (.put buf nbuf)
             (.put buf nbuf)
             (recur (next faces)))
           :cljs
           (let [nbuf (.-buf ^thi.ng.geom.vector.Vec3 (gu/ortho-normal (first faces)))]
             (.set buf nbuf idx)
             (.set buf nbuf (+ idx 3))
             (.set buf nbuf (+ idx 6))
             (recur (next faces) (+ idx 9))))
        buf))))

(defn mesh-face-normals-buffer
  [m]
  (let [fnorms (g/face-normals m true)
        faces  (g/faces m)
        buf    (#?(:clj native/float-buffer-direct :cljs ta/float32) (* (count faces) 9))]
    (loop [faces faces #?@(:cljs [idx 0])]
      (if faces
        #?(:clj
           (let [^floats nbuf (float-array (.-buf ^thi.ng.geom.vector.Vec3 (fnorms (first faces))))]
             (.put buf nbuf)
             (.put buf nbuf)
             (.put buf nbuf)
             (recur (next faces)))
           :cljs
           (let [nbuf (.-buf ^thi.ng.geom.vector.Vec3 (fnorms (first faces)))]
             (.set buf nbuf idx)
             (.set buf nbuf (+ idx 3))
             (.set buf nbuf (+ idx 6))
             (recur (+ idx 9) (next faces))))
        buf))))

(defn mesh-vertex-normals-buffer
  [m]
  (let [vnorms (g/vertex-normals m true)
        faces  (g/faces m)
        buf    (#?(:clj native/float-buffer-direct :cljs ta/float32) (* (count faces) 9))]
    (loop [faces faces #?@(:cljs [idx 0])]
      (if faces
        #?(:clj
           (let [f (first faces)
                 ^doubles vna (.-buf ^thi.ng.geom.vector.Vec3 (get vnorms (first f)))
                 ^doubles vnb (.-buf ^thi.ng.geom.vector.Vec3 (get vnorms (nth f 1)))
                 ^doubles vnc (.-buf ^thi.ng.geom.vector.Vec3 (get vnorms (nth f 2)))]
             (.put buf (float (aget vna 0)))
             (.put buf (float (aget vna 1)))
             (.put buf (float (aget vna 2)))
             (.put buf (float (aget vnb 0)))
             (.put buf (float (aget vnb 1)))
             (.put buf (float (aget vnb 2)))
             (.put buf (float (aget vnc 0)))
             (.put buf (float (aget vnc 1)))
             (.put buf (float (aget vnc 2)))
             (recur (next faces)))
           :cljs
           (let [f (first faces)]
             (.set buf (.-buf ^thi.ng.geom.vector.Vec3 (get vnorms (first f))) idx)
             (.set buf (.-buf ^thi.ng.geom.vector.Vec3 (get vnorms (nth f 1))) (+ idx 3))
             (.set buf (.-buf ^thi.ng.geom.vector.Vec3 (get vnorms (nth f 2))) (+ idx 6))
             (recur (+ idx 9) (next faces))))
        buf))))

;; Attribute buffer generation
;; 
;; This needs more work: Colors & UVs might/will be based on vertices,
;; but vertex buffer contains copies of each vertex due to unrolling
;; and use of non-indexed VBOs.
;;
;; Therefore, currently only normals & single color options are
;; properly supported.

(defn common-attrib-buffer-specs
  [{:keys [num-vertices] :as acc} {:keys [normals fixed-normal uv colors fixed-color]}]
  (let [c-stride (if colors
                   (count (first colors))
                   (if fixed-color (count fixed-color)))]
    (cond->
        acc

      normals
      (assoc-in [:attribs :normal] {:data normals :size 3})

      fixed-normal
      (assoc-in
       [:attribs :normal]
       {:data (fill-buffer-vec3
               (#?(:clj native/float-buffer-direct :cljs ta/float32) (* num-vertices 3))
               0 fixed-normal num-vertices 3)
        :size 3})

      uv
      (assoc-in
       [:attribs :uv]
       {:data (fill-vertex-buffer
               (#?(:clj native/float-buffer-direct :cljs ta/float32) (* 2 (count uv))) uv 2)
        :size 2})

      colors
      (assoc-in
       [:attribs :color]
       {:data (fill-vertex-buffer
               (#?(:clj native/float-buffer-direct :cljs ta/float32) (* c-stride (count colors)))
               colors c-stride)
        :size c-stride})

      fixed-color
      (assoc-in
       [:attribs :color]
       {:data (fill-buffer
               (#?(:clj native/float-buffer-direct :cljs ta/float32) (* num-vertices c-stride))
               0 fixed-color num-vertices c-stride)
        :size c-stride}))))

;; GL buffer conversions
;;
;; Vec2 & Vec3
;;
;; In addition to the protocol implementation we also provide the same
;; functionality as standalone function. This is meant for special
;; cases where we know an item is a `Vec3` and want to avoid the
;; overhead of using the protocol.

#?(:clj
   (defn into-float-buffer-vec2
     [^thi.ng.geom.vector.Vec2 v ^FloatBuffer buf stride idx]
     (.put buf idx       (float (aget ^doubles (.-buf v) 0)))
     (.put buf (inc idx) (float (aget ^doubles (.-buf v) 1)))
     (unchecked-add-int idx stride))

   :cljs
   (defn into-float-buffer-vec2
     [^thi.ng.geom.vector.Vec2 v buf stride idx]
     (.set buf (.-buf v) idx)
     (+ idx stride)))

#?(:clj
   (defn into-float-buffer-vec3
     [^thi.ng.geom.vector.Vec3 v ^FloatBuffer buf stride idx]
     (.put buf idx       (float (aget ^doubles (.-buf v) 0)))
     (.put buf (inc idx) (float (aget ^doubles (.-buf v) 1)))
     (.put buf (+ idx 2) (float (aget ^doubles (.-buf v) 2)))
     (unchecked-add-int idx stride))

   :cljs
   (defn into-float-buffer-vec3
     [^thi.ng.geom.vector.Vec3 v buf stride idx]
     (.set buf (.-buf v) idx)
     (+ idx stride)))

;; PersistentVector

(extend-type
    #?(:clj clojure.lang.PersistentVector
       :cljs cljs.core.PersistentVector)

  streams/IBuffer
  (get-float-buffer
    [_] (#?(:clj native/float-buffer :cljs ta/float32) _))

  streams/IIntoBuffer
  (into-float-buffer
    [_ buf stride idx]
    (let [t     (type (first _))
          into* (condp = t
                  thi.ng.geom.vector.Vec2 into-float-buffer-vec2
                  thi.ng.geom.vector.Vec3 into-float-buffer-vec3
                  streams/into-float-buffer)]
      (case (count _)
        2 (->> idx
               (into* (first _) buf stride)
               (into* (nth _ 1) buf stride))
        3 (->> idx
               (into* (first _) buf stride)
               (into* (nth _ 1) buf stride)
               (into* (nth _ 2) buf stride))
        4 (->> idx
               (into* (first _) buf stride)
               (into* (nth _ 1) buf stride)
               (into* (nth _ 2) buf stride)
               (into* (nth _ 3) buf stride))
        (loop [idx idx, xs _]
          (if xs
            (recur (into* (first xs) buf stride idx) (next xs))
            idx))))))

;; LineStrip2

(extend-type LineStrip2

  IGLConvert
  (as-gl-buffer-spec
    [{:keys [points]} {:keys [stride] :or {stride 2} :as spec}]
    (let [numv (count points)]
      (common-attrib-buffer-specs
       {:attribs
        {:position    {:data (fill-vertex-buffer
                              (#?(:clj native/float-buffer :cljs ta/float32) (* numv stride))
                              points stride)
                       :size stride}}
        :mode         glc/line-strip
        :num-vertices numv}
       spec))))

;; LineStrip3

(extend-type LineStrip3
  IGLConvert
  (as-gl-buffer-spec
    [{:keys [points]} {:keys [stride] :or {stride 3} :as spec}]
    (let [numv (count points)]
      (common-attrib-buffer-specs
       {:attribs
        {:position    {:data (fill-vertex-buffer
                              (#?(:clj native/float-buffer :cljs ta/float32) (* numv stride))
                              points stride)
                       :size stride}}
        :mode         glc/line-strip
        :num-vertices numv}
       spec))))

;; Rect2

(extend-type Rect2
  IGLConvert
  (as-gl-buffer-spec
    [r {:keys [stride normals] :or {stride 2} :as spec}]
    (let [[a b c d] (g/vertices r)]
      (common-attrib-buffer-specs
       {:attribs
        {:position    {:data (fill-vertex-buffer
                              (#?(:clj native/float-buffer :cljs ta/float32) (* 4 stride))
                              [a b d c] stride)
                       :size stride}}
        :mode         glc/triangle-strip
        :num-vertices 4}
       (if normals
         (-> spec (assoc :fixed-normal V3Z) (dissoc :normals))
         spec)))))

;; Polygon2

(extend-type Polygon2
  IGLConvert
  (as-gl-buffer-spec
    [_ {:keys [normals stride mode] :or {stride 2, normals true, mode glc/triangles} :as spec}]
    (if (= glc/triangles mode)
      (let [faces     (g/tessellate _)
            num-faces (count faces)
            num-verts (* num-faces 3)]
        (common-attrib-buffer-specs
         {:attribs {:position {:data (fill-vertex-buffer
                                      (#?(:clj native/float-buffer :cljs ta/float32) (* num-verts stride))
                                      faces stride)
                               :size stride}}
          :mode         mode
          :num-vertices num-verts
          :num-faces    num-faces}
         (if normals
           (-> spec (assoc :fixed-normal V3Z) (dissoc :normals))
           spec)))
      (let [verts     (g/vertices _)
            verts     (conj verts (first verts))
            num-verts (count verts)]
        (common-attrib-buffer-specs
         {:attribs {:position {:data (fill-vertex-buffer
                                      (#?(:clj native/float-buffer :cljs ta/float32) (* num-verts stride))
                                      verts stride)
                               :size stride}}
          :mode         mode
          :num-vertices num-verts}
         (dissoc spec :normals))))))

;; BasicMesh

(extend-type BasicMesh
  IGLConvert
  (as-gl-buffer-spec
    [_ {:keys [fnormals tessellate stride]
        :or   {fnormals true, tessellate true, stride 3} :as spec}]
    (let [m         (if tessellate (g/tessellate _) _)
          faces     (map #(g/vertices % m) (g/faces m))
          num-faces (count faces)
          num-verts (* num-faces 3)]
      (common-attrib-buffer-specs
       {:attribs
        {:position    {:data (fill-vertex-buffer-faces
                              into-float-buffer-vec3
                              (#?(:clj native/float-buffer :cljs ta/float32) (* num-verts stride))
                              faces stride)
                       :size stride}}
        :mode         glc/triangles
        :num-vertices num-verts
        :num-faces    num-faces}
       (assoc spec :normals (if fnormals (mesh-face-normals-buffer m)))))))

;; GMesh

(extend-type GMesh
  IGLConvert
  (as-gl-buffer-spec
    [_ {:keys [vnormals fnormals tessellate stride]
        :or   {fnormals true, tessellate true, stride 3} :as spec}]
    (let [m         (if tessellate (g/tessellate _) _)
          faces     (map #(g/vertices % m) (g/faces m))
          num-faces (count faces)
          num-verts (* num-faces 3)
          normals   (cond
                      vnormals (mesh-vertex-normals-buffer m)
                      fnormals (mesh-face-normals-buffer m)
                      :default nil)]
      (common-attrib-buffer-specs
       {:attribs
        {:position    {:data (fill-vertex-buffer-faces
                              into-float-buffer-vec3
                              (#?(:clj native/float-buffer :cljs ta/float32) (* num-verts stride))
                              faces stride)
                       :size stride}}
        :mode         glc/triangles
        :num-vertices num-verts
        :num-faces    num-faces}
       (assoc spec :normals normals)))))

;; WegGL context

;; Default configuration

#?(:cljs
   (def context-default-attribs
     {:alpha                                true
      :antialias                            true
      :depth                                true
      :fail-if-major-performance-caveat     false
      :prefer-low-power-to-high-performance false
      :premultiplied-alpha                  true
      :preserve-drawing-buffer              false
      :stencil                              false}))

;; Context creation

#?(:cljs
   (defn gl-context
     ([canvas] (gl-context canvas {}))
     ([canvas attribs]
      (let [canvas (if (string? canvas) (.getElementById js/document canvas) canvas)
            attribs (clj->js (merge context-default-attribs attribs))
            getctx #(try
                      (let [ctx (.getContext canvas % attribs)]
                        (set! (.-onselectstart canvas) (constantly false))
                        ctx)
                      (catch js/Error e nil))
            ctx (loop [ids ["webgl" "experimental-webgl" "webkit-3d" "moz-webgl"]]
                  (when ids
                    (if-let [ctx (getctx (first ids))]
                      ctx
                      (recur (next ids)))))]
        (or ctx (err/unsupported! "WebGL not available"))))))

;; Context manipulation functions

#?(:clj
   (defn clear-color-buffer
     ([^GL3 gl col]
      (let [^thi.ng.color.core.RGBA c (col/as-rgba col)]
        (clear-color-buffer gl (.-r c) (.-g c) (.-b c) (.-a c))))
     ([^GL3 gl r g b a]
      (doto gl
        (.glClearColor r g b a)
        (.glClear glc/color-buffer-bit))))

   :cljs
   (defn clear-color-buffer
     ([^WebGLRenderingContext gl col]
      (let [^thi.ng.color.core.RGBA c (col/as-rgba col)]
        (clear-color-buffer gl (.-r c) (.-g c) (.-b c) (.-a c))))
     ([^WebGLRenderingContext gl r g b a]
      (doto gl
        (.clearColor r g b a)
        (.clear glc/color-buffer-bit)))))

#?(:clj
   (defn clear-depth-buffer
     [^GL3 gl d]
     (doto gl
       (.glClearDepth (float d))
       (.glClear glc/depth-buffer-bit)))

   :cljs
   (defn clear-depth-buffer
     [^WebGLRenderingContext gl d]
     (doto gl
       (.clearDepth d)
       (.clear glc/depth-buffer-bit))))

#?(:clj
   (defn clear-color-and-depth-buffer
     ([^GL3 gl col d]
      (let [^thi.ng.color.core.RGBA c (col/as-rgba col)]
        (clear-color-and-depth-buffer gl (.-r c) (.-g c) (.-b c) (.-a c) d)))
     ([^GL3 gl r g b a d]
      (doto gl
        (.glClearColor r g b a)
        (.glClearDepth d)
        (.glClear (bit-or glc/depth-buffer-bit glc/color-buffer-bit)))))

   :cljs
   (defn clear-color-and-depth-buffer
     ([^WebGLRenderingContext gl col d]
      (let [^thi.ng.color.core.RGBA c (col/as-rgba col)]
        (clear-color-and-depth-buffer gl (.-r c) (.-g c) (.-b c) (.-a c) d)))
     ([^WebGLRenderingContext gl r g b a d]
      (doto gl
        (.clearColor r g b a)
        (.clearDepth d)
        (.clear (bit-or glc/depth-buffer-bit glc/color-buffer-bit))))))

#?(:clj
   (defn disable [^GL3 gl flag] (.glDisable gl flag) gl)

   :cljs
   (defn disable [^WebGLRenderingContext gl flag] (.disable gl flag) gl))

#?(:clj
   (defn enable [^GL3 gl flag] (.glEnable gl flag) gl)

   :cljs
   (defn enable [^WebGLRenderingContext gl flag] (.enable gl flag) gl))

#?(:clj
   (defn scissor-test
     ([^GL3 gl {[x y] :p [w h] :size}]
      (scissor-test gl x y w h))
     ([^GL3 gl x y w h]
      (doto gl
        (.glEnable glc/scissor-test)
        (.glScissor x y w h))))

   :cljs
   (defn scissor-test
     ([^WebGLRenderingContext gl {[x y] :p [w h] :size}]
      (scissor-test gl x y w h))
     ([^WebGLRenderingContext gl x y w h]
      (doto gl
        (.enable glc/scissor-test)
        (.scissor x y w h)))))

#?(:clj
   (defn cull-faces
     [^GL3 gl side]
     (doto gl
       (.glEnable glc/cull-face)
       (.glCullFace side)))

   :cljs
   (defn cull-faces
     [^WebGLRenderingContext gl side]
     (doto gl
       (.enable glc/cull-face)
       (.cullFace side))))

#?(:clj
   (defn set-viewport
     ([^GL3 gl {[x y] :p [w h] :size}] (.glViewport gl x y w h) gl)
     ([^GL3 gl x y w h] (.glViewport gl x y w h) gl))

   :cljs
   (defn set-viewport
     ([^WebGLRenderingContext gl {[x y] :p [w h] :size}] (.viewport gl x y w h) gl)
     ([^WebGLRenderingContext gl x y w h] (.viewport gl x y w h) gl)))

#?(:clj
   (defn get-viewport-rect
     [^GL3 gl]
     (let [^IntBuffer b (native/int-buffer 4)]
       (.glGetIntegeri_v gl glc/viewport 0 b)
       (r/rect (.get b 0) (.get b 1) (.get b 2) (.get b 3))))

   :cljs
   (defn get-viewport-rect
     [^WebGLRenderingContext gl]
     (let [b (.getParameter gl glc/viewport)]
       (r/rect (aget b 0) (aget b 1) (aget b 2) (aget b 3)))))

(def perspective mat/perspective)

(def ortho mat/ortho)

;; WebGL extensions

(def ^:private float-ext-ids
  ["OES_texture_float"
   "OES_texture_half_float"
   "OES_texture_float_linear"
   "OES_texture_half_float_linear"
   "WEBGL_color_buffer_float"
   "EXT_color_buffer_half_float"])

#?(:cljs
   (defn get-extension
     [^WEBGL_color_buffer_float gl ext]
     (.getExtension gl ext)))

#?(:cljs
   (defn get-float-extension
     [^WebGLRenderingContext gl spec]
     (let [[s-tex h-tex s-lin h-lin s-fbo h-fbo :as ext] (map #(.getExtension gl %) float-ext-ids)]
       (->> [{:texture s-tex :filterable s-lin :renderable s-fbo
              :type glc/float
              :precision :single :single true}
             {:texture h-tex :filterable h-lin :renderable h-fbo
              :type (when h-tex (.-HALF_FLOAT_OES h-tex))
              :precision :half :half true}]
            (reduce
             (fn [acc c]
               (if (and (get c :texture) (every? c (get spec :require)))
                 (conj acc c) acc))
             [])
            (map
             (fn [c]
               (assoc c :score
                      (apply + (map (fn [pref score] (if (c pref) score 0))
                                    (get spec :prefer) [0x80 0x40 0x20 0x10 0x8 0x4 0x2 0x1])))))
            (sort-by :score)
            (last)))))

#?(:cljs
   (defn get-supported-extensions
     [^WebGLRenderingContext gl]
     (.getSupportedExtensions gl)))

;; Attribute buffers

#?(:clj
   (def ^:private buffer-element-sizes
     {glc/array-buffer         4
      glc/element-array-buffer 2}))

#?(:clj
   (defn make-array-buffer
     [^GL3 gl target mode data]
     (let [size (or (get buffer-element-sizes target)
                    (err/unsupported! "Buffer type must be array-buffer or element-array-buffer"))
           ^IntBuffer id (native/int-buffer 1)]
       (.rewind ^Buffer data)
       (doto gl
         (.glGenBuffers 1 id)
         (.glBindBuffer target (.get id 0))
         (.glBufferData target (* size (.capacity ^Buffer data)) data mode)
         (.glBindBuffer target 0))
       id))

   :cljs
   (defn make-array-buffer
     [^WebGLRenderingContext gl target mode data]
     (let [buffer (.createBuffer gl)]
       (.bindBuffer gl target buffer)
       (.bufferData gl target data mode)
       buffer)))

#?(:clj
   (defn make-vertex-array
     [^GL3 gl spec]
     (let [^IntBuffer id (native/int-buffer 1)]
       (.glGenVertexArrays gl 1 id)
       (.glBindVertexArray gl (.get id 0))
       (doseq [[attr {:keys [id loc offset size stride]
                      :or   {stride 0 offset 0}}] (:attribs spec)]
         (let [loc (int (-> spec (get :shader) (get :attribs) (get attr)))]
           (doto gl
             (.glBindBuffer glc/array-buffer (.get ^IntBuffer id 0))
             (.glEnableVertexAttribArray loc)
             (.glVertexAttribPointer loc (int size) glc/float false (int stride) (int offset)))))
       (when-let [index (:index spec)]
         (.glBindBuffer gl glc/element-array-buffer (.get ^IntBuffer index 0)))
       (.glBindVertexArray gl 0)
       (assoc spec :vao id))))

(defn make-attribute-buffers
  #?(:clj
     [^GL3 gl mode attribs]
     :cljs
     [^WebGLRenderingContext gl mode attribs])
  (reduce-kv
   (fn [attribs id {:keys [data target] :or {target glc/array-buffer}}]
     (update-in
      attribs [id] merge
      {:buffer      (make-array-buffer gl target mode data)
       :target      target
       :buffer-mode mode}))
   attribs attribs))

(defn make-buffers-in-spec
  [spec gl mode]
  (let [spec (update spec :attribs #(make-attribute-buffers gl mode %))]
    (if (get spec :indices)
      (update spec :indices
              #(merge %
                      {:buffer (make-array-buffer gl glc/element-array-buffer mode (get % :data))
                       :buffer-mode mode}))
      spec)))

#?(:clj
   (defn update-buffer-in-spec
     [^GL3 gl spec id coll]
     (let [{:keys [target data buffer buffer-mode size]} (-> spec :attribs id)]
       (fill-vertex-buffer data coll size)
       (.glBindBuffer gl target buffer)
       (.glBufferData gl target (* size (.capacity ^Buffer data)) data buffer-mode)
       gl))

   :cljs
   (defn update-buffer-in-spec
     [^WebGLRenderingContext gl spec id coll]
     (let [{:keys [target data buffer buffer-mode size]} (-> spec :attribs id)]
       (fill-vertex-buffer data coll size)
       (.bindBuffer gl target buffer)
       (.bufferData gl target data buffer-mode)
       gl)))

;; Shader usage

(defn begin-shader
  #?(:clj  [^GL3 gl shader uniforms attribs indices]
     :cljs [^WebGLRenderingContext gl shader uniforms attribs indices])
  #?(:clj  (.glUseProgram gl (int (get shader :program)))
     :cljs (.useProgram gl (get shader :program)))
  (sh/apply-default-uniforms shader uniforms)
  (reduce-kv #(sh/set-uniform shader uniforms %2 %3) nil uniforms)
  (reduce-kv #(sh/set-attribute gl shader %2 %3) nil attribs)
  (when indices
    #?(:clj  (.glBindBuffer gl glc/element-array-buffer (.get ^IntBuffer (get indices :buffer) 0))
       :cljs (.bindBuffer gl glc/element-array-buffer (get indices :buffer)))))

(defn end-shader
  #?(:clj  [^GL3 gl shader]
     :cljs [^WebGLRenderingContext gl shader])
  (reduce #(sh/disable-attribute gl shader (key %2)) nil (get shader :attribs)))

;; Shading state preparation
;;
;; Shader specs can specify WebGL render state flags which can then be
;; automatically setup using the `prepare-render-state` function below.
;; Currently the following options are supported:
;; 
;; | *Key*         | *Values*                                     | *Description*                  |
;; |---------------+----------------------------------------------+--------------------------------|
;; | `:depth-test` | boolean                                      | enabled if true, else disabled |
;; | `:blend`      | boolean                                      | enabled if true, else disabled |
;; | `:blend-fn`   | 2-elem vector `[src-coeff dest-coeff]`       | blend function coeffs,         |
;; |               | reference[1]                                 | only used if `:blend true`     |
;; | `:blend-eq`   | one of: `glc/func-add`, `glc/func-subtract`, | blend equation mode            |
;; |               | `glc/func-reverse-subtract`                  |                                |
;; |               | reference[2]                                 |                                |
;; | `:tex`        | single texture or seq of textures            | binds textures to tex. units   |
;; |               |                                              | starting from 0                |
;; 
;; [1] https://www.khronos.org/opengles/sdk/docs/man/xhtml/glBlendFunc.xml
;; [2] https://www.khronos.org/opengles/sdk/docs/man/xhtml/glBlendEquation.xml
                                   
(defn bind-sequentially
  [coll]
  (loop [i 0, coll coll]
    (when coll
      (when-let [x (first coll)]
        (bind x i))
      (recur (inc i) (next coll)))))

(defn prepare-render-state
  "Takes a GL context and shader spec, sets GL render flags stored
  under :state key (only if :state is present)."
  #?(:clj  [^GL3 gl {:keys [state]}]
     :cljs [^WebGLRenderingContext gl {:keys [state]}])
  (when state
    (if (get state :depth-test true)
      (enable gl glc/depth-test)
      (disable gl glc/depth-test))
    (if (get state :blend)
      (let [[src dest] (or (get state :blend-fn) [glc/src-alpha glc/one])
            eq         (or (get state :blend-eq glc/func-add))]
        (doto gl
          (enable glc/blend)
          (#?(:clj .glBlendFunc :cljs .blendFunc) src dest)
          (#?(:clj .glBlendEquation :cljs .blendEquation) eq)))
      (disable gl glc/blend))
    (when-let [tex (get state :tex)]
      (if (sequential? tex)
        (bind-sequentially tex)
        (bind tex 0))))
  gl)

;; Drawing helpers

(defn compute-normal-matrix
  [m v] (-> v (m/* m) (m/invert) (m/transpose)))

(defn auto-normal-matrix
  [model-id view-id]
  (fn [shader uniforms]
    (compute-normal-matrix
     (or (get uniforms model-id) (get-in shader [:uniforms model-id :default]))
     (or (get uniforms view-id) (get-in shader [:uniforms view-id :default])))))

(defn inject-normal-matrix
  [spec model-mat view-mat normal-mat-id]
  (let [model-mat (if (keyword? model-mat)
                    (-> spec :uniforms model-mat)
                    model-mat)
        view-mat (if (keyword? view-mat)
                   (-> spec :uniforms view-mat)
                   view-mat)]
    (assoc-in
     spec [:uniforms normal-mat-id]
     (compute-normal-matrix model-mat view-mat))))

;; Drawing

#?(:clj
   (defn draw
     [^GL3 gl spec]
     (let [mode (get spec :mode glc/triangles)]
       (if (get spec :indices)
         (.glDrawElements gl mode (get spec :num-items) glc/unsigned-short 0)
         (.glDrawArrays gl mode 0 (get spec :num-vertices)))
       gl))

   :cljs
   (defn draw
     [^WebGLRenderingContext gl spec]
     (let [mode (get spec :mode glc/triangles)]
       (if (get spec :indices)
         (.drawElements gl mode (get spec :num-items) glc/unsigned-short 0)
         (.drawArrays gl mode 0 (get spec :num-vertices)))
       gl)))

(defn draw-with-shader
  #?(:clj  [^GL3 gl {:keys [shader] :as spec}]
     :cljs [^WebGLRenderingContext gl {:keys [shader] :as spec}])
  (prepare-render-state gl shader)
  (begin-shader gl shader (get spec :uniforms) (get spec :attribs) (get spec :indices))
  (draw gl spec)
  (end-shader gl shader)
  gl)
