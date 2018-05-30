(ns thi.ng.geom.gl.shaders.shadow
  (:require
   [thi.ng.geom.matrix :refer [M44]]
   [thi.ng.geom.gl.core :as gl]
   [thi.ng.geom.gl.webgl.constants :as glc]
   [thi.ng.geom.gl.buffers :as buf]
   [thi.ng.glsl.core :as glsl :include-macros true]
   [thi.ng.glsl.lighting :as light]))

(def light-pass-spec
  {:vs (glsl/minified "
void main() {
  vWorldPos = model * vec4(position, 1.0);
  gl_Position = proj * view * vWorldPos;
}")
   :fs (glsl/minified "
void main() {
  vec3 lightPos = (view * vWorldPos).xyz;
  float depth = clamp(length(lightPos) / 10.0, 0.0, 1.0);
  gl_FragColor = vec4(vec3(depth), 1.0);
}")
   :uniforms {:view     :mat4
              :proj     :mat4
              :model    [:mat4 M44]}
   :attribs {:position  :vec3}
   :varying {:vWorldPos :vec4}})

(def cam-pass-spec
  {:vs (glsl/minified "
void main(){
  vNormal = normal;
  vWorldPos = model * vec4(position, 1.0);
  gl_Position = proj * view * vWorldPos;
}")
   :fs (glsl/assemble
        (glsl/glsl-spec
         [light/spotlight-attenuation light/spotlight-influence
          light/lambert light/skylight]
         "
vec3 gamma(vec3 color) {
  return pow(color, vec3(2.2));
}

float texture2DCompare(sampler2D depths, vec2 uv, float compare) {
  float depth = texture2D(depths, uv).r;
  return step(compare, depth);
}

float texture2DShadowLerp(sampler2D depths, vec2 size, vec2 uv, float compare) {
  vec2 texelSize = vec2(1.0) / size;
  vec2 f = fract(uv * size + 0.5);
  vec2 centroidUV = floor(uv * size + 0.5) / size;

  float lb = texture2DCompare(depths, centroidUV + texelSize * vec2(0.0, 0.0), compare);
  float lt = texture2DCompare(depths, centroidUV + texelSize * vec2(0.0, 1.0), compare);
  float rb = texture2DCompare(depths, centroidUV + texelSize * vec2(1.0, 0.0), compare);
  float rt = texture2DCompare(depths, centroidUV + texelSize * vec2(1.0, 1.0), compare);
  return mix(mix(lb, lt, f.y), mix(rb, rt, f.y), f.x);
}

void main() {
  vec3 worldNormal = normalize(vNormal);

  vec3 camPos = (view * vWorldPos).xyz;
  vec3 lightPos = (lightView * vWorldPos).xyz;
  vec3 lightPosNormal = normalize(lightPos);
  vec3 lightSurfaceNormal = lightRot * worldNormal;

  vec4 lightDevice = lightProj * vec4(lightPos, 1.0);
  vec2 lightDeviceNormal = lightDevice.xy / lightDevice.w;
  vec2 lightUV = lightDeviceNormal * 0.5 + 0.5;

  // shadow calculation
  float bias = 1e-4;
  float lightDepth2 = clamp(length(lightPos) / 10.0, 0.0, 1.0) - bias;
  float illuminated = texture2DShadowLerp(lightDepthTex, lightDepthSize, lightUV, lightDepth2);

  vec3 excident = vec3(0.4 + skylight(worldNormal.y) +
                   lambert(lightSurfaceNormal, -lightPosNormal) *
                   spotlightInfluence(lightPosNormal, coneAngle, 10.0) *
                   spotlightAttenuation(lightPos, 10.0) *
                   illuminated);

  //excident *=  worldNormal * 0.5 + 0.5; // TODO remove, debug only

  gl_FragColor = vec4(gamma(excident), 1.0);
}"))
   :uniforms {:view           :mat4
              :proj           :mat4
              :model          [:mat4 M44]
              :lightView      :mat4
              :lightProj      :mat4
              :lightRot       :mat3
              :lightDepthTex  [:sampler2D 0]
              :lightDepthSize :vec2
              :coneAngle      [:float 15]}
   :attribs  {:position       :vec3
              :normal         :vec3}
   :varying  {:vNormal        :vec3
              :vWorldPos      :vec4}})

(defn init-light-fbo
  [^WebGLRenderingContext gl size]
  (let [float-ext (.call (aget gl "getFloatExtension")
                         gl #js {:require ["renderable"]
                                 :prefer  ["filterable" "half"]})
        tex (buf/make-texture
             gl {:type   (.-type float-ext)
                 :width  size
                 :height size
                 :filter glc/linear
                 :wrap   glc/clamp-to-edge})]
    {:tex tex
     :fbo (-> (buf/make-fbo gl)
              (gl/bind)
              (gl/set-fbo-color-texture tex)
              (gl/set-fbo-depth-buffer (buf/make-depth-buffer gl size))
              (gl/unbind))}))

(defn draw-light-pass
  [^WebGLRenderingContext gl {:keys [fbo tex]} draw-fn]
  (gl/bind fbo)
  (doto gl
    (gl/set-viewport 0 0 (get tex :width) (get tex :height))
    (gl/clear-color-buffer 1 1 1 1)
    (gl/clear-depth-buffer 1)
    (gl/enable glc/depth-test)
    (gl/cull-faces glc/front))
  (draw-fn)
  (gl/unbind fbo))

(defn draw-cam-pass
  [^WebGLRenderingContext gl tex draw-fn]
  (doto gl
    (gl/clear-depth-buffer 1)
    (gl/enable glc/depth-test)
    (gl/cull-faces glc/back))
  (gl/bind tex 0)
  (draw-fn {:lightDepthTex 0
            :lightDepthSize [(get tex :width) (get tex :height)]}))
