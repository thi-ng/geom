(ns thi.ng.geom.examples.gl.textured-cube
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
   [thi.ng.glsl.core :as glsl :include-macros true]))

(enable-console-print!)

(def shader-spec
  {:vs (glsl/minified
        "void main() {
           vUV = uv;
           gl_Position = proj * view * model * vec4(position, 1.0);
         }")
   :fs (glsl/minified
        "void main() {
           gl_FragColor = texture2D(tex, vUV);
         }")
   :uniforms {:model    [:mat4 M44]
              :view     :mat4
              :proj     :mat4
              :tex      :sampler2D}
   :attribs  {:position :vec3
              :uv       :vec2}
   :varying  {:vUV      :vec2}
   :state    {:depth-test true}})

(defn ^:export demo
  []
  (let [gl        (gl/gl-context "main")
        view-rect (gl/get-viewport-rect gl)
        model     (-> (a/aabb 1)
                      (g/center)
                      (g/as-mesh
                       {:mesh    (glm/indexed-gl-mesh 12 #{:uv})
                        :attribs {:uv (attr/face-attribs (attr/uv-cube-map-v 256 false))}})
                      (gl/as-gl-buffer-spec {})
                      (cam/apply (cam/perspective-camera {:eye (vec3 0 0 0.5) :fov 90 :aspect view-rect}))
                      (assoc :shader (sh/make-shader-from-spec gl shader-spec))
                      (gl/make-buffers-in-spec gl glc/static-draw))
        tex-ready (volatile! false)
        tex       (buf/load-texture
                   gl {:callback (fn [tex img] (vreset! tex-ready true))
                       :src      "assets/cubev.png"
                       :flip     false})]
    (anim/animate
     (fn [t frame]
       (when @tex-ready
         (gl/bind tex 0)
         (doto gl
           (gl/set-viewport view-rect)
           (gl/clear-color-and-depth-buffer col/WHITE 1)
           (gl/draw-with-shader
            (assoc-in model [:uniforms :model]
                      (-> M44 (g/rotate-x PI) (g/rotate-y (* t 2)))))))
       true))))
