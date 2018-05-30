(ns thi.ng.geom.examples.gl.ex05a
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
   [thi.ng.geom.circle :as c]
   [thi.ng.geom.ptf :as ptf]
   [thi.ng.glsl.vertex :as vertex]
   [thi.ng.glsl.lighting :as light]
   [thi.ng.glsl.fog :as fog]
   [thi.ng.color.gradients :as grad]
   ))

(enable-console-print!)

(def shader-spec
  {:vs (->> "void main() {
                vUV = uv + vec2(0, time * 0.025);
                vPos = (view * model * vec4(position, 1.0)).xyz;
                vNormal = surfaceNormal(normal, normalMat);
                vLightDir = (view * vec4(lightPos, 1.0)).xyz - vPos;
                gl_Position = proj * vec4(vPos, 1.0);
              }"
            (glsl/glsl-spec [vertex/surface-normal])
            (glsl/assemble))
   :fs (->> "void main() {
                vec3 n = normalize(vNormal);
                vec3 v = normalize(-vPos);
                vec3 l = normalize(vLightDir);
                float NdotL = max(0.0, dot(n, l));
                vec3 specular = Ks * beckmannSpecular(l, v, n, m);
                vec3 att = lightCol / pow(length(vLightDir), lightAtt);
                vec3 diff = texture2D(tex, vUV).xyz;
                vec3 col = att * NdotL * ((1.0 - s) * diff + s * specular) + Ka * diff;
                float fog = fogLinear(length(vPos), 1.0, 7.5);
                col = mix(col, Kf, fog);
                gl_FragColor = vec4(col, 1.0);
              }"
            (glsl/glsl-spec [fog/fog-linear light/beckmann-specular])
            (glsl/assemble))
   :uniforms {:model     :mat4
              :view      :mat4
              :proj      :mat4
              :normalMat :mat4
              :tex       :sampler2D
              :Ks        [:vec3 [1 1 1]]
              :Ka        [:vec3 [0.0 0.0 0.3]]
              :Kf        [:vec3 [0.0 0.0 0.1]]
              :m         [:float 0.1]
              :s         [:float 0.9]
              :lightCol  [:vec3 [200 80 40]]
              :lightPos  [:vec3 [0 0 5]]
              :lightAtt  [:float 3.0]
              :time      :float}
   :attribs  {:position :vec3
              :normal   :vec3
              :uv       :vec2}
   :varying  {:vUV      :vec2
              :vPos     :vec3
              :vNormal  :vec3
              :vLightDir :vec3}
   :state    {:depth-test true}})

(defn cinquefoil
  [t]
  (let [t  (* t m/TWO_PI)
        pt (* 2.0 t)
        qt (* 5.0 t)
        qc (+ 3.0 (Math/cos qt))]
    (v/vec3 (* qc (Math/cos pt)) (* qc (Math/sin pt)) (Math/sin qt))))

(defn knot-simple
  []
  (-> (mapv cinquefoil (butlast (m/norm-range 400)))
      (ptf/sweep-mesh
       (g/vertices (c/circle 0.5) 20)
       {:mesh    (glm/gl-mesh 16800 #{:fnorm :uv})
        :attribs {:uv attr/uv-tube}
        :align?  true
        :loop?   true})))

(defn knot-nested
  [nums numt]
  (-> (mapv cinquefoil (m/norm-range 400))
      (ptf/compute-frames)
      (ptf/align-frames)
      (ptf/sweep-strand-mesh
       #(+ 0.5 (* 0.5 (Math/sin (* 8 TWO_PI (/ % 400.0)))))
       8 7 (g/vertices (c/circle 0.05) 8)
       {:mesh    (glm/gl-mesh 65536 #{:fnorm :uv})
        :attribs {:uv attr/uv-tube}
        :align?  true})))

(defn gradient-texture
  [gl w h opts]
  (let [canv (.createElement js/document "canvas")
        ctx  (.getContext canv "2d")
        cols (grad/cosine-gradient h (:rainbow1 grad/cosine-schemes))]
    (set! (.-width canv) w)
    (set! (.-height canv) h)
    (set! (.-strokeStyle ctx) "none")
    (loop [y 0, cols cols]
      (if cols
        (let [c (first cols)
              c (if (< (mod y 16) 8)
                  (col/adjust-brightness c -0.75)
                  c)]
          (set! (.-fillStyle ctx) @(col/as-css c))
          (.fillRect ctx 0 y w 1)
          (recur (inc y) (next cols)))
        (buf/make-canvas-texture gl canv opts)))))

(defn ^:export demo
  []
  (let [gl        (gl/gl-context "main")
        view-rect (gl/get-viewport-rect gl)
        model     (-> (knot-simple)
                      #_(knot-nested 8 7)
                      (gl/as-gl-buffer-spec {})
                      (cam/apply (cam/perspective-camera {:eye (vec3 0 0 5) :fov 90 :aspect view-rect}))
                      (assoc :shader (sh/make-shader-from-spec gl shader-spec))
                      (gl/make-buffers-in-spec gl glc/static-draw)
                      (time))
        tex       (gradient-texture gl 4 1024 {:wrap [glc/clamp-to-edge glc/repeat]})]
    (anim/animate
     (fn [t frame]
       (gl/bind tex 0)
       (doto gl
         (gl/set-viewport view-rect)
         (gl/clear-color-and-depth-buffer 0.0 0.0 0.1 1 1)
         (gl/draw-with-shader
          (-> model
              (update :uniforms assoc
                      :time t
                      :m (+ 0.21 (* 0.2 (Math/sin (* t 0.5))))
                      :model (-> M44 (g/rotate-x (* t 0.36)) (g/rotate-y t)))
              (gl/inject-normal-matrix :model :view :normalMat))))
       true))))
