(ns thi.ng.geom.examples.gl.render-to-texture
  (:require-macros
   [thi.ng.math.macros :as mm])
  (:require
   [thi.ng.math.core :as m :refer [PI HALF_PI TWO_PI]]
   [thi.ng.color.core :as col]
   [thi.ng.typedarrays.core :as arrays]
   [thi.ng.geom.gl.core :as gl]
   [thi.ng.geom.gl.webgl.constants :as glc]
   [thi.ng.geom.gl.webgl.animator :as anim]
   [thi.ng.geom.gl.buffers :as buf]
   [thi.ng.geom.gl.shaders :as sh]
   [thi.ng.geom.gl.utils :as glu]
   [thi.ng.geom.gl.glmesh :as glm]
   [thi.ng.geom.gl.camera :as cam]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.vector :as v :refer [vec2 vec3]]
   [thi.ng.geom.matrix :as mat :refer [M44]]
   [thi.ng.geom.aabb :as a]
   [thi.ng.geom.attribs :as attr]
   [thi.ng.glsl.core :as glsl :include-macros true]
   [thi.ng.geom.sphere :as s]
   [thi.ng.geom.gl.shaders.lambert :as lambert]
   [thi.ng.geom.gl.shaders.image :as image]
   [thi.ng.geom.gl.fx :as fx]
   [thi.ng.glsl.vertex :as vertex]))

(def shader-spec
  {:vs (->> "void main() {
               vUV = uv;
               vec3 p = rotateZ(position, position.z * sin(time) + time);
               gl_Position = proj * view * model * vec4(p, 1.0);
             }"
            (glsl/minified)
            (glsl/glsl-spec-plain [vertex/rotate-z])
            (glsl/assemble))
   :fs (->> "void main(){
               gl_FragColor = texture2D(tex, vUV);
             }"
            (glsl/minified))
   :uniforms {:model    [:mat4 M44]
              :view     :mat4
              :proj     :mat4
              :tex      [:sampler2D 0]
              :time     :float}
   :attribs  {:position :vec3
              :uv       :vec2}
   :varying  {:vUV      :vec2}
   :state    {:depth-test true}})

(defn ^:export demo
  []
  (enable-console-print!)
  (let [gl             (gl/gl-context "main")
        view-rect      (gl/get-viewport-rect gl)
        main-shader    (sh/make-shader-from-spec gl shader-spec)
        lambert-shader (sh/make-shader-from-spec gl lambert/shader-spec-attrib)
        fbo-size       512
        fbo-tex        (buf/make-texture
                        gl {:width  fbo-size
                            :height fbo-size
                            :filter glc/linear
                            :wrap   glc/clamp-to-edge})
        fbo            (buf/make-fbo-with-attachments
                        gl {:tex    fbo-tex
                            :width  fbo-size
                            :height fbo-size
                            :depth? true})
        quad           (-> (fx/init-fx-quad gl)
                           (assoc :shader (sh/make-shader-from-spec gl fx/shader-spec))
                           (assoc-in [:shader :state :tex] fbo-tex))
        model1         (-> (s/sphere 1)
                           (g/as-mesh
                            {:mesh    (glm/gl-mesh 64 #{:col :fnorm})
                             :attribs {:col (fn [_ _ v _] (col/rgba (m/madd (m/normalize v) 0.5 0.5)))}
                             :res     6})
                           (gl/as-gl-buffer-spec {})
                           (cam/apply (cam/perspective-camera {:eye (vec3 0 0 3) :aspect 1.0}))
                           (assoc :shader lambert-shader)
                           (gl/make-buffers-in-spec gl glc/static-draw))
        model2         (-> (s/sphere 2.5)
                           (g/as-mesh
                            {:mesh    (glm/gl-mesh 2048 #{:uv})
                             :attribs {:uv attr/uv-faces}
                             :res     32})
                           (gl/as-gl-buffer-spec {})
                           (cam/apply (cam/perspective-camera {:fov 90 :aspect view-rect}))
                           (assoc :shader main-shader)
                           (gl/make-buffers-in-spec gl glc/static-draw))]

    (anim/animate
     (fn [t frame]
       ;; render pass #1: Render to FBO
       (gl/bind fbo)
       (doto gl
         (gl/set-viewport 0 0 fbo-size fbo-size)
         (gl/clear-color-and-depth-buffer col/BLACK 1)
         (gl/draw-with-shader
          (assoc-in model1 [:uniforms :model]
                    (-> M44 (g/rotate-x t) (g/rotate-y (* t 2))))))
       (gl/unbind fbo)

       ;; render pass #2: Render with FBO as texture
       (gl/bind fbo-tex)
       (doto gl
         (gl/set-viewport view-rect)
         (gl/clear-color-and-depth-buffer 0.8 0.8 0.8 1 1)
         (gl/draw-with-shader
          (update model2 :uniforms merge
                  {:model (-> M44 (g/rotate-x (* t -0.25)) (g/rotate-y (* t -0.45)))
                   :time  t})))
       (gl/unbind fbo-tex)

       ;; render pass #3: Draw FBO texture as 2D image
       (doto gl
         (gl/set-viewport 0 0 128 128)
         (gl/draw-with-shader quad))

       true))))
