(ns thi.ng.geom.gl.fx.bloom
  (:require
   [thi.ng.dstruct.core :as d]
   [thi.ng.geom.rect :as r]
   [thi.ng.geom.gl.fx :as fx]
   [thi.ng.glsl.core :as glsl :include-macros true]
   [thi.ng.glsl.color :as col]
   [thi.ng.glsl.texture :as tex]))

;; Bloom filter FX pipeline preset

;; Threshold pass

(def threshold-shader-spec
  (d/merge-deep
   fx/shader-spec
   {:fs (->> "
#if __VERSION__ >= 300
out vec4 fragColor;
void main() {
  float c = threshold(texture(tex, vUV).rgb, thresh * 0.5, thresh);
  fragColor = vec4(c, c, c, 1.0);
}
#else
void main() {
  float c = threshold(texture2D(tex, vUV).rgb, thresh * 0.5, thresh);
  gl_FragColor = vec4(c, c, c, 1.0);
}
#endif"
             (glsl/glsl-spec-plain [col/threshold])
             (glsl/assemble))
    :uniforms {:thresh [:float 0.8]}}))

;; Blur pass

(def blur-shader-spec
  (d/merge-deep
   fx/shader-spec
   {:fs (->> "
#if __VERSION__ >= 300
out vec4 fragColor;
void main() {
  fragColor = vec4((horizontal ? blur5H(tex, vUV) : blur5V(tex, vUV)), 1.0);
}
#else
void main() {
  gl_FragColor = vec4((horizontal ? blur5H(tex, vUV) : blur5V(tex, vUV)), 1.0);
}
#endif"
             (glsl/glsl-spec-plain [tex/blur5-h tex/blur5-v])
             (glsl/assemble))
    :uniforms {:horizontal :bool}}))

;; Composite pass

(def comp-shader-spec
  (d/merge-deep
   fx/shader-spec
   {:fs "
#if __VERSION__ >= 300
out vec4 fragColor;
void main() {
  fragColor = pow(vec4((texture(tex, vUV).rgb * (1.0 - blend) + texture(tex2, vUV).rgb * blend) * exposure, 1.0), vec4(gamma));
}
#else
void main() {
  gl_FragColor = pow(vec4((texture2D(tex, vUV).rgb * (1.0 - blend) + texture2D(tex2, vUV).rgb * blend) * exposure, 1.0), vec4(gamma));
}
#endif"
    :uniforms {:tex2     [:sampler2D 1]
               :blend    [:float 0.35]
               :exposure [:float 1.3]
               :gamma    [:float 1.25]}}))

;; Constructor / factory

(defn make-pipeline-spec
  [w h scale version]
  {:width   w
   :height  h
   :version version
   :fbos    {:src  {:scale 1}
             :ping {:scale scale}
             :pong {:scale scale}}
   :shaders {:threshold threshold-shader-spec
             :blur      blur-shader-spec
             :final     comp-shader-spec}
   :passes  [{:id       :threshold
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
              :uniforms {:horizontal false}}
             {:id       :final
              :shader   :final
              :tex      [:src :ping]
              :viewport (r/rect w h)}]})
