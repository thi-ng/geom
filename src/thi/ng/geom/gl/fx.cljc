(ns thi.ng.geom.gl.fx
  #?(:clj
     (:import
      [com.jogamp.opengl GL3]
      [java.nio Buffer FloatBuffer ShortBuffer]))
  (:require
   [thi.ng.geom.matrix :as mat]
   [thi.ng.geom.gl.core :as gl]
   [thi.ng.geom.gl.buffers :as buf]
   [thi.ng.geom.gl.shaders :as sh]
   [thi.ng.xerror.core :as err]
   #?@(:clj
       [[thi.ng.geom.gl.jogl.buffers :as native]
        [thi.ng.geom.gl.jogl.constants :as glc]]
       :cljs
       [[thi.ng.geom.gl.webgl.constants :as glc]
        [thi.ng.typedarrays.core :as ta]])))

(comment

  ;; Example FX pipeline spec

  {:src-width  1280
   :src-height 720
   :version    330
   :fbos       {:src  {:scale 1}
                :ping {:scale 8}
                :pong {:scale 8}}
   :shaders    {:threshold threshold-shader-spec
                :blur      blur-shader-spec
                :comp      comp-shader-spec}
   :passes     [{:id       :threshold
                 :target   :ping
                 :shader   :threshold
                 :tex      :src}
                {:id       :blur-h
                 :target   :pong
                 :shader   :blur
                 :tex      :ping
                 :uniforms {:horizontal true}}
                {:id       :blur-v
                 :target   :ping
                 :shader   :blur
                 :tex      :pong
                 :uniforms {:horizontal false}}]}
  )

;; FX Shader template

(def passthrough-vs
  "void main(){vUV=uv;gl_Position=model*vec4(position,0.0,1.0);}")

(def passthrough-fs
  #?(:clj  "out vec4 fragColor;void main(){fragColor=texture(tex,vUV);}"
     :cljs "void main(){gl_FragColor=texture2D(tex,vUV);}"))

(def shader-spec
  {:vs passthrough-vs
   :fs passthrough-fs
   :attribs  {:position  :vec2
              :uv        :vec2}
   :varying  {:vUV       :vec2}
   :uniforms {:model     [:mat4 mat/M44]
              :tex       [:sampler2D 0]}
   :state    {:depth-test false}})

;; Helpers

(defn resolve-textures
  [fbos tex]
  (let [tex (mapv
             (fn [t] (if (keyword? t) (get-in fbos [t :tex]) t))
             (if (sequential? tex) tex [tex]))]
    (if (== 1 (count tex)) (first tex) tex)))

(defn resolve-pipeline-textures
  [pipe tex] (resolve-textures (get pipe :fbos) tex))

;; FX pipeline creation

(defn init-fx-quad
  [gl]
  {:attribs      (gl/make-attribute-buffers
                  gl glc/static-draw
                  {:position {:data (#?(:clj  native/float-buffer-direct
                                        :cljs ta/float32) [-1 -1, 1 -1, -1 1 1 1])
                              :size 2}
                   :uv       {:data (#?(:clj  native/float-buffer-direct
                                        :cljs ta/float32) [0 0, 1 0, 0 1, 1 1])
                              :size 2}})
   :num-vertices 4
   :mode         glc/triangle-strip})

(defn init-render-fbo
  [gl w h]
  (let [tex (buf/make-texture
             gl {:width w :height h :filter glc/linear :wrap glc/clamp-to-edge})
        fbo (buf/make-fbo-with-attachments
             gl {:tex tex :width w :height h :depth? true})]
    {:tex tex :fbo fbo :width w :height h}))

(defn init-fx-fbo
  ([gl w h scale]
   (init-fx-fbo gl (int (/ w scale)) (int (/ h scale))))
  ([gl w h]
   (let [tex (buf/make-texture
              gl {:width w :height h :filter glc/linear :wrap glc/clamp-to-edge})
         fbo (buf/make-fbo-with-attachments
              gl {:tex tex :width w :height h})]
     {:tex tex :fbo fbo :width w :height h})))

(defn init-pipeline-fbos
  [gl fbos srcw srch]
  (reduce-kv
   (fn [acc k {:keys [scale] :as v}]
     (let [f     (if (= :src k) init-render-fbo init-fx-fbo)
           [w h] (if scale
                   [(int (/ srcw scale)) (int (/ srch scale))]
                   [(:width v) (:height v)])]
       (assoc acc k (f gl w h))))
   {} fbos))

(defn init-pipeline-shaders
  ([gl shaders]
   (reduce-kv
    (fn [acc k v] (assoc acc k (sh/make-shader-from-spec gl v)))
    {} shaders))
  #?(:clj
     ([gl shaders version]
      (reduce-kv
       (fn [acc k v] (assoc acc k (sh/make-shader-from-spec gl v version)))
       {} shaders))))

(defn init-pipeline-passes
  [gl fbos shaders passes]
  (let [quad (init-fx-quad gl)]
    (reduce
     (fn [acc {:keys [id target shader tex uniforms viewport] :as v}]
       (let [fbo  (get fbos target)
             sh   (get shaders shader)
             sh   (assoc-in sh [:state :tex] (resolve-textures fbos tex))
             pass (assoc quad ::fx-pass-id id ::fx-fbo fbo :shader sh :viewport viewport)
             pass (if uniforms (merge pass {:uniforms uniforms}) pass)]
         (conj acc pass)))
     [] passes)))

(defn init-pipeline
  [gl {:keys [fbos width height shaders passes version] :as opts}]
  (let [fbos    (init-pipeline-fbos gl fbos width height)
        shaders #?(:clj
                   (if version
                     (init-pipeline-shaders gl shaders version)
                     (init-pipeline-shaders gl shaders))
                   :cljs
                   (init-pipeline-shaders gl shaders))
        passes  (init-pipeline-passes gl fbos shaders passes)]
    {:fbos    fbos
     :shaders shaders
     :passes  passes}))

;; FX pipeline processing

(defn draw-fx-pass
  [^GL3 gl spec]
  (let [{:keys [fbo] :as fx-fbo} (get spec ::fx-fbo)]
    (when fbo (gl/bind fbo))
    (if-let [vp (get spec :viewport)]
      (gl/set-viewport gl vp)
      (gl/set-viewport gl 0 0 (get fx-fbo :width) (get fx-fbo :height)))
    (gl/draw-with-shader gl spec)
    (when fbo (gl/unbind fbo))))

(defn execute-pipeline
  [gl spec]
  (loop [passes (get spec :passes)]
    (when passes (draw-fx-pass gl (first passes)) (recur (next passes)))))

(defn update-pipeline-pass
  [pipe id f & args]
  (update
   pipe :passes
   (fn [passes]
     (mapv
      (fn [pass]
        (if (= id (get pass ::fx-pass-id)) (apply f pass args) pass))
      passes))))
