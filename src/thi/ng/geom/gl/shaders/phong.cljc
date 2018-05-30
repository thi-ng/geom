(ns thi.ng.geom.gl.shaders.phong
  (:require
   [thi.ng.glsl.core :as glsl :include-macros true]
   [thi.ng.glsl.vertex :as vertex]
   [thi.ng.glsl.lighting :as light]
   [thi.ng.geom.gl.core :as gl]
   [thi.ng.geom.matrix :refer [M44]]))

(def shader-spec
  {:vs (glsl/assemble
        (glsl/glsl-spec
         [vertex/surface-normal]
         "
void main(){
  vec4 worldPos = model * vec4(position, 1.0);
  vec4 eyePos = view * worldPos;
  vEyePos = eyePos.xyz;
  vNormal = surfaceNormal(normal, normalMat);
  vLightPos = (view * vec4(lightPos, 1.0)).xyz;
  gl_Position = proj * eyePos;
}"))
   :fs (glsl/assemble
        (glsl/glsl-spec
         [light/phong light/blinn-phong]
         "
#if __VERSION__ >= 300
out vec4 fragColor;
#endif

void main() {
  vec3 L = normalize(vLightPos - vEyePos);
  vec3 E = normalize(-vEyePos);
  vec3 N = normalize(vNormal);

  float NdotL = max(0.0, (dot(N, L) + wrap) / (1.0 + wrap));
  vec3 color = ambientCol + NdotL * diffuseCol;

  float specular = 0.0;
  if (useBlinnPhong) {
    specular = blinnPhong(L, E, N);
  } else {
    specular = phong(L, E, N);
  }
  color += max(pow(specular, shininess), 0.0) * specularCol;
  #if __VERSION__ >= 300
  fragColor = vec4(color, 1.0);
  #else
  gl_FragColor = vec4(color, 1.0);
  #endif
}"))
   :uniforms {:view          :mat4
              :proj          :mat4
              :model         [:mat4 M44]
              :normalMat     [:mat4 (gl/auto-normal-matrix :model :view)]
              :shininess     [:float 32]
              :ambientCol    [:vec3 [0 0 0]]
              :diffuseCol    [:vec3 [0.8 0.8 0.8]]
              :specularCol   [:vec3 [1 1 1]]
              :lightPos      [:vec3 [0 0 2]]
              :useBlinnPhong [:bool true]
              :wrap          [:float 0]}
   :attribs {:position       [:vec3 0]
             :normal         [:vec3 1]}
   :varying {:vNormal        :vec3
             :vEyePos        :vec3
             :vLightPos      :vec3}
   :state    {:depth-test    true}})
