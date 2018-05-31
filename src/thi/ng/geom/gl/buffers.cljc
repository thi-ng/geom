(ns thi.ng.geom.gl.buffers
  #?(:clj
     (:import
      [com.jogamp.opengl GL3]
      [com.jogamp.opengl.util.texture TextureData TextureIO]
      [java.nio Buffer DoubleBuffer FloatBuffer ShortBuffer IntBuffer]))
  (:require
   [thi.ng.geom.gl.core :as gl]
   [thi.ng.xerror.core :as err]
   #?@(:clj
       [[thi.ng.geom.gl.jogl.buffers :as native]
        [thi.ng.geom.gl.jogl.constants :as glc]]
       :cljs
       [[thi.ng.geom.gl.webgl.constants :as glc]])))

(declare check-fbo)

;; Textures
;;
;; | *Key*          | *Default*           | *Description*             |
;; |----------------+---------------------+---------------------------|
;; | `:image`       | `nil`               | HTML image or canvas      |
;; | `:pixels`      | `nil`               | Image data array          |
;; | `:format`      | `glc/rgba`          | Texture color format      |
;; | `:type`        | `glc/unsigned-byte` | Encoding                  |
;; | `:filter`      | `nil`               | Interpolation `[min mag]` |
;; | `:wrap`        | `nil`               | Texture wrap mode `[s t]` |
;; | `:flip`        | `nil`               | Vertical flip?            |
;; | `:premultiply` | `nil`               | Premultiplied alpha?      |
;; | `:width`       | `nil`               | Texture width             |
;; | `:height`      | `nil`               | Texture height            |

(defrecord Texture2D
    #?(:clj  [^GL3 gl ^IntBuffer id target]
       :cljs [^WebGLRenderingContext gl id target])

  gl/IBind
  (bind [_]
    (gl/bind _ 0))
  (bind [_ unit]
    #?@(:clj
        [(.glActiveTexture gl (+ glc/texture0 unit))
         (.glBindTexture gl target (.get id 0))]
        :cljs
        [(.activeTexture gl (+ glc/texture0 unit))
         (.bindTexture gl target id)])
    _)
  (unbind [_]
    (gl/unbind _ 0))
  (unbind [_ unit]
    #?@(:clj
        [(.glActiveTexture gl (+ glc/texture0 unit))
         (.glBindTexture gl target 0)]
        :cljs
        [(.activeTexture gl (+ glc/texture0 unit))
         (.bindTexture gl target nil)])
    _)

  gl/IRelease
  (release [_]
    #?(:clj (.glDeleteTextures gl 1 id) :cljs (.deleteTexture gl id))
    _)

  gl/IConfigure
  (configure
    [_ {:keys [width height image pixels format type filter wrap flip premultiply]
        :or   {type glc/unsigned-byte format glc/rgb} :as config}]
    (when filter
      (let [[min mag] (if (sequential? filter) filter [filter filter])]
        #?@(:clj
            [(.glTexParameteri gl target glc/texture-min-filter min)
             (.glTexParameteri gl target glc/texture-mag-filter mag)]
            :cljs
            [(.texParameteri gl target glc/texture-min-filter min)
             (.texParameteri gl target glc/texture-mag-filter mag)])))
    (when wrap
      (let [[ws wt] (if (sequential? wrap) wrap [wrap wrap])]
        #?@(:clj
            [(.glTexParameteri gl target glc/texture-wrap-s ws)
             (.glTexParameteri gl target glc/texture-wrap-t wt)]
            :cljs
            [(.texParameteri gl target glc/texture-wrap-s ws)
             (.texParameteri gl target glc/texture-wrap-t wt)])))
    #?@(:cljs
        [(when (not (nil? flip))
           (.pixelStorei gl glc/unpack-flip-y-webgl flip))
         (when (not (nil? premultiply))
           (.pixelStorei gl glc/unpack-premultiply-alpha-webgl premultiply))])
    #?(:clj
       (cond
         image
         (let [^TextureData d image]
           (.glTexImage2D
            gl (int target) 0
            (.getInternalFormat d)
            (.getWidth d)
            (.getHeight d)
            0
            (.getPixelFormat d)
            (.getPixelType d)
            (.getBuffer d)))
         (and width height)
         (.glTexImage2D
          gl (int target) 0
          (int format)
          (int width)
          (int height)
          0
          (int format)
          (int type)
          ^Buffer pixels))
       :cljs
       (cond
         image              (.texImage2D gl target 0 format format type image)
         (and width height) (.texImage2D gl target 0 format width height 0 format type pixels)))
    _))

#?(:clj
   (defn make-texture
     [^GL3 gl opts]
     (let [id   (native/int-buffer 1)
           _    (.glGenTextures gl 1 id)
           tex  (Texture2D. gl id (get opts :target glc/texture-2d))
           opts (merge {:format glc/rgba :type glc/unsigned-byte} opts)]
       (gl/bind tex)
       (gl/configure tex opts)))

   :cljs
   (defn make-texture
     [^WebGLRenderingContext gl opts]
     (let [tex  (Texture2D. gl (.createTexture gl) (opts :target glc/texture-2d))
           opts (merge {:format glc/rgba :type glc/unsigned-byte} opts)]
       (gl/bind tex)
       (gl/configure tex opts))))

(comment
  ;; Float textures
  (make-texture gl {:width 512 :height 512 :type glc/float})
  )

#?(:cljs
   (defn make-canvas-texture
     ([^WebGLRenderingContext gl canvas]
      (make-canvas-texture gl canvas {}))
     ([^WebGLRenderingContext gl canvas opts]
      (->> opts
           (merge {:image       canvas
                   :filter      [glc/linear glc/linear]
                   :wrap        [glc/clamp-to-edge glc/clamp-to-edge]
                   :flip        false
                   :premultiply true})
           (make-texture gl)))))

#?(:clj
   (defn load-texture
     [^GL3 gl opts]
     (let [tex (->> opts
                    (merge {:format glc/rgba
                            :filter [glc/linear glc/linear]
                            :wrap   glc/clamp-to-edge})
                    (make-texture gl))
           img (TextureIO/newTextureData
                (.getGLProfile gl)
                ^java.io.File (get opts :src)
                false
                (name (get opts :src-format "png")))]
       (doto tex
         (gl/bind)
         (gl/configure (merge {:image img} opts)))
       tex))

   :cljs
   (defn load-texture
     [^WebGLRenderingcontext gl opts]
     (let [tex (->> opts
                    (merge {:format glc/rgba
                            :filter [glc/linear glc/linear]
                            :wrap   glc/clamp-to-edge})
                    (make-texture gl))
           img (js/Image.)]
       (set! (.-onload img)
             (fn []
               (doto tex
                 (gl/bind)
                 (gl/configure (merge {:flip false :image img} opts)))
               (when-let [cb (get opts :callback)] (cb tex img))))
       (when-let [ecb (:error-callback opts)]
         (set! (.-onerror img) ecb))
       (when-let [cors (:cors opts)]
         (set! (.-crossOrigin img) cors))
       (set! (.-src img) (get opts :src))
       tex)))

;; RenderBuffer

(defrecord RenderBuffer
    #?(:clj  [^GL3 gl ^IntBuffer id format width height]
       :cljs [^WebGLRenderingContext gl id format width height])

  gl/IBind
  (bind [_]
    #?(:clj  (.glBindRenderbuffer gl glc/renderbuffer (.get id 0))
       :cljs (.bindRenderbuffer gl glc/renderbuffer id))
    _)
  (bind [_ _] (err/unsupported!))
  (unbind [_]
    #?(:clj  (.glBindRenderbuffer gl glc/renderbuffer 0)
       :cljs (.bindRenderbuffer gl glc/renderbuffer nil))
    _)

  gl/IRelease
  (release [_]
    #?(:clj  (.glDeleteRenderbuffers gl 1 id)
       :cljs (.deleteRenderbuffer gl id))
    _)

  gl/IConfigure
  (configure [_ {:keys [format width height]}]
    (gl/bind _)
    #?(:clj  (.glRenderbufferStorage gl glc/renderbuffer (int format) (int width) (int height))
       :cljs (.renderbufferStorage gl glc/renderbuffer format width height))
    (gl/unbind _)
    (RenderBuffer. gl id format width height)))

#?(:clj
   (defn make-render-buffer
     ([^GL3 gl]
      (make-render-buffer gl nil))
     ([^GL3 gl opts]
      (let [id  (native/int-buffer 1)
            _   (.glGenRenderbuffers gl 1 id)
            buf (RenderBuffer. gl id nil nil nil)]
        (if opts (gl/configure buf opts) buf))))
   :cljs
   (defn make-render-buffer
     ([^WebGLRenderingContext gl]
      (make-render-buffer gl nil))
     ([^WebGLRenderingContext gl opts]
      (let [buf (RenderBuffer. gl (.createRenderbuffer gl) nil nil nil)]
        (if opts (gl/configure buf opts) buf)))))

#?(:clj
   (defn make-depth-buffer
     ([^GL3 gl size]
      (make-depth-buffer gl size size))
     ([^GL3 gl width height]
      (make-depth-buffer gl glc/depth-component16 width height))
     ([^GL3 gl format width height]
      (make-render-buffer gl {:format format :width width :height height})))

   :cljs
   (defn make-depth-buffer
     ([^WebGLRenderingContext gl size]
      (make-depth-buffer gl size size))
     ([^WebGLRenderingContext gl width height]
      (make-render-buffer gl {:format glc/depth-component16 :width width :height height}))))

;; Frame buffer

(defrecord FBO
    #?(:clj [^GL3 gl ^IntBuffer id]
       :cljs [^WebGLRenderingContext gl id])

  gl/IBind
  (bind [_]
    #?(:clj  (.glBindFramebuffer gl glc/framebuffer (.get id 0))
       :cljs (.bindFramebuffer gl glc/framebuffer id))
    _)
  (bind [_ _]
    (err/unsupported!))
  (unbind [_]
    #?(:clj  (.glBindFramebuffer gl glc/framebuffer 0)
       :cljs (.bindFramebuffer gl glc/framebuffer nil))
    _)

  gl/IRelease
  (release [_]
    #?(:clj  (.glDeleteFramebuffers gl 1 id)
       :cljs (.deleteFramebuffer gl id))
    _)

  gl/IFramebuffer
  (set-fbo-color-texture
    [_ {:keys [id]}]
    #?(:clj  (.glFramebufferTexture2D gl glc/framebuffer glc/color-attachment0 glc/texture-2d (.get ^IntBuffer id 0) 0)
       :cljs (.framebufferTexture2D gl glc/framebuffer glc/color-attachment0 glc/texture-2d id 0))
    (check-fbo gl)
    _)
  (set-fbo-depth-buffer
    [_ {:keys [id]}]
    #?(:clj  (.glFramebufferRenderbuffer gl glc/framebuffer glc/depth-attachment glc/renderbuffer (.get ^IntBuffer id 0))
       :cljs (.framebufferRenderbuffer gl glc/framebuffer glc/depth-attachment glc/renderbuffer id))
    (check-fbo gl)
    _))

(defn- check-fbo
  #?(:clj [^GL3 gl] :cljs [^WebGLRenderingContext gl])
  (condp == #?(:clj  (.glCheckFramebufferStatus gl glc/framebuffer)
               :cljs (.checkFramebufferStatus gl glc/framebuffer))
    glc/framebuffer-unsupported
    (err/throw! "FBO unsupported")
    glc/framebuffer-incomplete-attachment
    (err/throw! "FBO incomplete attachment")
    glc/framebuffer-incomplete-dimensions
    (err/throw! "FBO incomplete dimensions")
    glc/framebuffer-incomplete-missing-attachment
    (err/throw! "FBO incomplete missing attachment")
    gl))

#?(:clj
   (defn make-fbo
     [^GL3 gl]
     (let [id (native/int-buffer 1)]
       (.glGenFramebuffers gl 1 id)
       (FBO. gl id)))
   :cljs
   (defn make-fbo
     [^WebGLRenderingContext gl]
     (FBO. gl (.createFramebuffer gl))))

(defn make-fbo-with-attachments
  [gl {:keys [tex width height depth?]}]
  (let [fbo (make-fbo gl)]
    (gl/bind fbo)
    (gl/set-fbo-color-texture fbo tex)
    (when depth?
      (gl/set-fbo-depth-buffer fbo (make-depth-buffer gl width height)))
    (gl/unbind fbo)))
