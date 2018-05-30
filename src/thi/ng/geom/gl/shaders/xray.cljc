(ns thi.ng.geom.gl.shaders.xray
  (:require
   [thi.ng.geom.matrix :refer [M44]]
   [thi.ng.geom.gl.core :as gl]
   [thi.ng.glsl.core :as glsl :include-macros true]
   [thi.ng.glsl.vertex :as vert]
   #?(:clj  [thi.ng.geom.gl.jogl.constants :as glc]
      :cljs [thi.ng.geom.gl.webgl.constants :as glc])))

(def shader-spec
  {:vs (glsl/assemble
        (glsl/glsl-spec
         [vert/surface-normal]
         "
void main() {
  vIncident = view * model * vec4(position, 1.0);
  vNormal = surfaceNormal(normal, normalMat);
  gl_Position = proj * vIncident;
}"))
   :fs (glsl/minified
        "
#if __VERSION__ >= 300
out vec4 fragColor;
#endif

void main() {
  float opac = abs(dot(normalize(-vNormal), normalize(-vIncident.xyz)));
  opac = 1.0 - pow(opac, alpha);
  #if __VERSION__ >= 300
  fragColor = vec4(lightCol * opac, opac);
  #else
  gl_FragColor = vec4(lightCol * opac, opac);
  #endif
}")
   :uniforms {:model      [:mat4 M44]
              :view       :mat4
              :normalMat  [:mat4 (gl/auto-normal-matrix :model :view)]
              :proj       :mat4
              :lightCol   [:vec3 [1 1 1]]
              :alpha      [:float 0.5]}
   :attribs  {:position   :vec3
              :normal     :vec3}
   :varying  {:vIncident  :vec4
              :vNormal    :vec3}
   :state    {:depth-test false
              :blend      true
              :blend-func [glc/src-alpha glc/one]}})
