(ns thi.ng.geom.gl.shaders.lambert
  (:require
   [thi.ng.glsl.core :as glsl :include-macros true]
   [thi.ng.glsl.vertex :as vertex]
   [thi.ng.glsl.lighting :as light]
   [thi.ng.geom.gl.core :as gl]
   [thi.ng.geom.matrix :refer [M44]]))

(defn- make-shader-spec
  [vs-src]
  {:vs (->> vs-src
            (glsl/glsl-spec-plain
             [vertex/mvp vertex/surface-normal light/lambert light/lambert-abs])
            (glsl/assemble))
   :fs #?(:clj  "out vec4 fragColor;void main(){fragColor=vCol;}"
          :cljs "void main(){gl_FragColor=vCol;}")
   :uniforms {:model      [:mat4 M44]
              :view       :mat4
              :proj       :mat4
              :normalMat  [:mat4 (gl/auto-normal-matrix :model :view)]
              :ambientCol [:vec3 [0 0 0]]
              :diffuseCol [:vec3 [1 1 1]]
              :lightCol   [:vec3 [1 1 1]]
              :lightDir   [:vec3 [0 0 1]]
              :alpha      [:float 1]}
   :attribs  {:position   [:vec3 0]
              :normal     [:vec3 1]}
   :varying  {:vCol       :vec4}
   :state    {:depth-test true}})

(def shader-spec
  (make-shader-spec
   (glsl/minified "
void main() {
  float lam = lambert(surfaceNormal(normal, normalMat), lightDir);
  vCol = vec4(ambientCol + diffuseCol * lightCol * lam, alpha);
  gl_Position = mvp(position, model, view, proj);
}")))

(def shader-spec-attrib
  (-> (make-shader-spec
       (glsl/minified "
void main() {
  float lam = lambert(surfaceNormal(normal, normalMat), lightDir);
  vCol = vec4(ambientCol + color.rgb * lightCol * lam, alpha);
  gl_Position = mvp(position, model, view, proj);
}"))
      (assoc-in [:attribs :color] [:vec4 2])))

(def shader-spec-two-sided
  (make-shader-spec
   (glsl/minified "
void main() {
  float lam = lambertAbs(surfaceNormal(normal, normalMat), lightDir);
  vCol = vec4(ambientCol + diffuseCol * lightCol * lam, alpha);
  gl_Position = mvp(position, model, view, proj);
}")))

(def shader-spec-two-sided-attrib
  (-> (make-shader-spec
       (glsl/minified "
void main() {
  float lam = lambertAbs(surfaceNormal(normal, normalMat), lightDir);
  vCol = vec4(ambientCol + color.rgb * lightCol * lam, alpha);
  gl_Position = mvp(position, model, view, proj);
}"))
      (assoc-in [:attribs :color] [:vec4 2])))
