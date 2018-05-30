(ns thi.ng.geom.gl.shaders
  #?(:clj
     (:import
      [com.jogamp.opengl GL GL2 GL3 GL4]
      [com.jogamp.opengl.util.glsl ShaderCode ShaderProgram ShaderUtil]
      [java.nio Buffer FloatBuffer IntBuffer ShortBuffer])
     :cljs
     (:require-macros
      [cljs-log.core :refer [debug warn]]))
  (:require
   [thi.ng.math.core :as m]
   [thi.ng.geom.core :as g]
   [thi.ng.dstruct.streams :as streams]
   [thi.ng.color.core :as col]
   [thi.ng.xerror.core :as err]
   [clojure.string :as str]
   #?@(:clj
       [[thi.ng.geom.gl.jogl.buffers :as native]
        [thi.ng.geom.gl.jogl.constants :as glc]
        [thi.ng.glsl.core :as glsl]]
       :cljs
       [[thi.ng.geom.gl.webgl.constants :as glc]
        [thi.ng.geom.gl.utils :as glu]
        [thi.ng.glsl.core :as glsl :include-macros true]
        [thi.ng.typedarrays.core :as ta]])))

;; Shader spec format
;;
;; | *Key*       | *Description*                                           |
;; |-------------+---------------------------------------------------------|
;; | `:vs`       | Vertex shader source                                    |
;; | `:fs`       | Fragment shader source                                  |
;; | `:attribs`  | User attributes                                         |
;; | `:uniforms` | Shader uniform specification & defaults                 |
;; | `:varying`  | Shader internal bridge variables                        |
;; | `:prelude`  | GLSL source code to prepend (e.g. =#define= directives) |
;; | `:version`  | GLSL version number (generates =#version= directive)    |
;; | `:state`    | OpenGL state flags to initialize when shader is used    |
;;
;; TODO sample shader spec

(defn bool->int [x] (if x 1 0))

(defn bool->ivec
  [coll] (#?(:clj native/int-buffer :cljs ta/int32) (mapv bool->int coll)))

;; Shader uniforms
;; Supported types

(def uniform-types
  #?(:clj  {:float       [#(.glUniform1f ^GL3 % %2 %3) float 1]
            :int         [#(.glUniform1i ^GL3 % %2 %3) int 1]
            :vec2        [#(.glUniform2fv ^GL3 % %2 1 ^FloatBuffer %3) native/float-buffer 2]
            :vec3        [#(.glUniform3fv ^GL3 % %2 1 ^FloatBuffer %3) native/float-buffer 3]
            :vec4        [#(.glUniform4fv ^GL3 % %2 1 ^FloatBuffer %3) native/float-buffer 4]
            :ivec2       [#(.glUniform2iv ^GL3 % %2 1 ^IntBuffer %3) native/int-buffer 2]
            :ivec3       [#(.glUniform3iv ^GL3 % %2 1 ^IntBuffer %3) native/int-buffer 3]
            :ivec4       [#(.glUniform4iv ^GL3 % %2 1 ^IntBuffer %3) native/int-buffer 4]
            :bool        [#(.glUniform1i ^GL3 % %2 %3) bool->int 1]
            :bvec2       [#(.glUniform2iv ^GL3 % %2 1 ^IntBuffer %3) bool->ivec 2]
            :bvec3       [#(.glUniform3iv ^GL3 % %2 1 ^IntBuffer %3) bool->ivec 3]
            :bvec4       [#(.glUniform4iv ^GL3 % %2 1 ^IntBuffer %3) bool->ivec 4]
            :mat2        [#(.glUniformMatrix2fv ^GL3 % %2 1 %3 ^FloatBuffer %4) native/float-buffer 4]
            :mat3        [#(.glUniformMatrix3fv ^GL3 % %2 1 %3 ^FloatBuffer %4) native/float-buffer 9]
            :mat4        [#(.glUniformMatrix4fv ^GL3 % %2 1 %3 ^FloatBuffer %4) native/float-buffer 16]
            :sampler2D   [#(.glUniform1i ^GL3 % %2 %3) int 1]
            :samplerCube [#(.glUniform1i ^GL3 % %2 %3) int 1]}
     :cljs {:float       ["1f" float]
            :int         ["1i" int]
            :vec2        ["2fv" ta/float32 2]
            :vec3        ["3fv" ta/float32 3]
            :vec4        ["4fv" ta/float32 4]
            :ivec2       ["2iv" ta/int32 2]
            :ivec3       ["3iv" ta/int32 3]
            :ivec4       ["4iv" ta/int32 4]
            :bool        ["1i" int]
            :bvec2       ["2iv" ta/int32 2]
            :bvec3       ["3iv" ta/int32 3]
            :bvec4       ["4iv" ta/int32 4]
            :mat2        ["Matrix2fv" ta/float32 4]
            :mat3        ["Matrix3fv" ta/float32 9]
            :mat4        ["Matrix4fv" ta/float32 16]
            :sampler2D   ["1i" int]
            :samplerCube ["1i" int]}))

;; Uniform setup & handling
;;
;; Shader specs define uniforms as a map under the `:uniforms` key. In
;; this map, each of the shader's uniform names must be stated as keys
;; and their types as values. Default values can be given by using a
;; vector of `[type default]`. Default values are only used when a user
;; doesn't specify a uniform in their model spec. The example below
;; defines default values for the `:ambient`, `:model` and `:texture`
;; uniforms:
;;
;; ```
;; :uniforms {:alpha    :float
;;            :diffuse  :vec3
;;            :ambient  [:vec3 [0.1 0.1 0.1]]
;;            :texture  [:sampler2D 0]
;;            :modelMat [:mat4 M44]
;;            :viewMat  :mat4
;;            :projMat  :mat4}
;; ```
;;
;; Special cases:
;;
;; - Setters for `:vec2` uniforms expect a 2-element vector or a number.
;;   If the latter, it is interpreted as `[n n]`.
;; - Setters for `:vec3` uniforms expect a 3-element vector, a
;;   thi.ng/color type or an integer. If the latter, it is interpreted as
;;   24bit RGB value and converted into a normalized RGB vector (`[r g b]`).
;; - Setters for `:vec4` uniforms expect a 4-element vector, color type,
;;   an integer or a CSS color string in `#hex`, `rgba()` or `hsla()`
;;   form, which is then converted into a normalized RGBA vector (`[r g b
;;   a]`)
;; - Values for Matrix uniforms can be specified as vector `[mat default transpose?]`
;;   to indicate given matrix should be transposed

(defn init-shader-uniforms
  #?(:clj  [^GL3 gl prog uniforms]
     :cljs [^WebGLRenderingContext gl prog uniforms])
  (reduce-kv
   (fn [umap id type]
     (let [loc                   #?(:clj  (.glGetUniformLocation gl prog (name id))
                                    :cljs (.getUniformLocation gl prog (name id)))
           [type default opt]    (if (sequential? type) type [type])
           [setter u-cast u-len] (uniform-types type)
           #?@(:cljs [setter (aget gl (str "uniform" setter))])]
       #?(:clj (prn :uniform-loc id loc))
       (assoc
        umap id
        {:type    type
         :default default
         :setter  (cond
                    (#{:float :int :bool :sampler2D} type)
                    (fn [x]
                      #?(:clj  (setter gl loc (u-cast x))
                         :cljs (.call setter gl loc (u-cast x))))

                    (#{:mat2 :mat3 :mat4} type)
                    (fn [x]
                      #?(:clj
                         (setter gl loc (boolean opt)
                                 (if (instance? FloatBuffer x)
                                   x (u-cast x)))
                         :cljs
                         (.call setter gl loc (boolean opt)
                                (if (ta/typed-array? x)
                                  x (u-cast x)))))

                    (= :vec2 type)
                    (fn [x]
                      (let [x (cond
                                #?@(:clj  [(instance? FloatBuffer x) x]
                                    :cljs [(ta/typed-array? x) x])
                                (satisfies? streams/IBuffer x) (streams/get-float-buffer x)
                                (number? x) (u-cast [x x])
                                :else (u-cast x))]
                        #?(:clj  (setter gl loc x)
                           :cljs (.call setter gl loc x))))

                    (= :vec3 type)
                    (fn [x]
                      (let [x (cond
                                #?@(:clj  [(instance? FloatBuffer x) x]
                                    :cljs [(ta/typed-array? x) x])
                                (satisfies? streams/IBuffer x) (streams/get-float-buffer x)
                                (number? x) (-> x col/int24 col/as-rgba deref (subvec 0 3) u-cast)
                                :else (u-cast x))]
                        #?(:clj  (setter gl loc x)
                           :cljs (.call setter gl loc x))))

                    (= :vec4 type)
                    (fn [x]
                      (let [x (cond
                                #?@(:clj  [(instance? FloatBuffer x) x]
                                    :cljs [(ta/typed-array? x) x])
                                (satisfies? streams/IBuffer x) (streams/get-float-buffer x)
                                (number? x) (-> x col/int24 col/as-rgba streams/get-float-buffer)
                                (string? x) (-> x col/css col/as-rgba streams/get-float-buffer)
                                :else (u-cast x))]
                        #?(:clj  (setter gl loc x)
                           :cljs (.call setter gl loc x))))

                    :else
                    (fn [x]
                      #?(:clj
                         (setter gl loc
                                 (if (instance? FloatBuffer x)
                                   x (u-cast (if (not (sequential? x)) [x] x))))
                         :cljs
                         (.call setter gl loc
                                (if (ta/typed-array? x)
                                  x (u-cast (if (not (sequential? x)) [x] x)))))))
         :loc     loc})))
   {} uniforms))

(defn set-uniform
  [shader uniforms id val]
  ;;(#?(:clj prn :cljs debug) :uniform id (get-in shader [:uniforms id :loc]) val)
  (if-let [u-spec (-> shader (get :uniforms) (get id))]
    ((get u-spec :setter) (if (fn? val) (val shader uniforms) val))
    (#?(:clj prn :cljs warn) "Unknown shader uniform: " id)))

(defn apply-default-uniforms
  [shader uniforms]
  (reduce-kv
   (fn [_ id val]
     (if-not (get uniforms id)
       (if-let [d (get val :default)]
         (set-uniform shader uniforms id d))))
   nil (get shader :uniforms)))

;; Shader attributes

(defn init-shader-attribs
  #?(:clj  [^GL3 gl prog attribs]
     :cljs [^WebGLRenderingContext gl prog attribs])
  (reduce-kv
   (fn [acc id _]
     (assoc acc id
            (#?(:clj .glGetAttribLocation :cljs .getAttribLocation) gl prog (name id))))
   {} attribs))

(defn set-attribute
  #?(:clj  [^GL3 gl shader id attrib-spec]
     :cljs [^WebGLRenderingContext gl shader id attrib-spec])
  (let [{:keys [buffer stride size type normalized? offset loc]} attrib-spec]
    ;;(#?(:clj prn :cljs debug) :loc id (-> shader (get :attribs) (get id)) :size size (or stride (* 4 size)))
    (if-let [loc (int (-> shader (get :attribs) (get id)))]
      (doto gl
        #?(:clj  (.glBindBuffer glc/array-buffer (.get ^IntBuffer buffer 0))
           :cljs (.bindBuffer glc/array-buffer buffer))
        (#?(:clj .glEnableVertexAttribArray :cljs .enableVertexAttribArray) loc)
        (#?(:clj .glVertexAttribPointer :cljs .vertexAttribPointer)
         loc
         (int size)
         (or type glc/float)
         (boolean normalized?)
         (or stride 0)
         (or offset 0)))
      (#?(:clj println :cljs warn) (str "Unknown shader attribute: " id)))))

(defn disable-attribute
  #?(:clj  [^GL3 gl shader id]
     :cljs [^WebGLRenderingContext gl shader id])
  (if-let [loc (-> shader (get :attribs) (get id))]
    (do (#?(:clj .glDisableVertexAttribArray :cljs .disableVertexAttribArray) gl loc) gl)
    (#?(:clj println :cljs warn) (str "Unknown shader attribute: " id))))

;; Shader creation
;;
;; Header injection
;;
;; These boilerplate `#define`s are prepended by default to any given
;; shader source before compilation with `compile-shader` below.

(def default-prelude
  "#ifdef GL_FRAGMENT_PRECISION_HIGH
  precision highp int;
  precision highp float;
  #else
  precision mediump int;
  precision mediump float;
  #endif
  #ifndef PI
  #define PI      3.141592653589793
  #endif
  #ifndef TWO_PI
  #define TWO_PI  6.283185307179586
  #endif
  #ifndef HALF_PI
  #define HALF_PI 1.570796326794896
  #endif
  #ifndef RAD
  #define RAD     0.008726646259972
  #endif
  ")

;; Creation, compilation & linking

(defn compile-glsl-vars
  [qualifier coll]
  (->> coll
       (map
        (fn [[id type]]
          (str qualifier " "
               (name (if (sequential? type) (first type) type)) " "
               (name id) ";\n")))
       (apply str)))

(defn compile-glsl3-attribs
  [coll]
  (->> coll
       (map
        (fn [[id type]]
          (if (sequential? type)
            (str "layout(location=" (nth type 1) ") in " (name (first type)) " " (name id) ";\n")
            (str "in " (name type) " " (name id) ";\n"))))
       (apply str)))

(defn prepare-shader-sources
  [{:keys [vs fs uniforms attribs varying prelude version]}]
  (let [gl3?     (and version (>= version 300))
        u-src    (compile-glsl-vars "uniform" uniforms)
        a-src    (if gl3? (compile-glsl3-attribs attribs) (compile-glsl-vars "attribute" attribs))
        v-src-vs (compile-glsl-vars (if gl3? "out" "varying") varying)
        v-src-fs (compile-glsl-vars (if gl3? "in" "varying") varying)
        src      (if version (str "#version " version "\n") "")
        src      (str src (or prelude default-prelude) u-src)]
    {:vs-src (str src v-src-vs a-src vs)
     :fs-src (str src v-src-fs fs)}))

#?(:cljs
   (defn parse-and-throw-error
     [^WebGLRenderingContext gl shader src]
     (let [src-lines (vec (str/split-lines src))
           errors (->> shader
                       (.getShaderInfoLog gl)
                       (str/split-lines)
                       (map
                        (fn [line]
                          (let [[_ ln msg] (re-find #"ERROR: \d+:(\d+): (.*)" line)]
                            (when ln
                              (str "line " ln ": " msg "\n"
                                   (nth src-lines (js/parseInt ln 10)))))))
                       (filter identity)
                       (str/join "\n"))]
       (.deleteShader gl shader)
       (err/throw! (str "Error compiling shader:\n" errors)))))

#?(:cljs
   (defn compile-shader
     [^WebGLRenderingContext gl src type]
     (if-let [shader (.createShader gl type)]
       (do
         (.shaderSource gl shader src)
         (.compileShader gl shader)
         (if (.getShaderParameter gl shader glc/compile-status)
           shader
           (parse-and-throw-error gl shader src)))
       (err/throw! "Can't create shader"))))

(defn bind-attrib-locations
  #?(:clj  [^GL3 gl program attribs]
     :cljs [^WebGLRenderingContext gl program attribs])
  (reduce-kv
   (fn [_ id att]
     (when (sequential? att)
       (#?(:clj prn :cljs debug) :bind-attr id (nth att 1))
       (#?(:clj .glBindAttribLocation :cljs .bindAttribLocation) gl program (nth att 1) (name id))))
   nil attribs))

#?(:clj
   (defn make-shader-from-spec
     ([^GL3 gl spec version]
      (make-shader-from-spec gl (assoc spec :version version)))
     ([^GL3 gl spec]
      (let [{:keys [vs-src fs-src]} (prepare-shader-sources spec)
            ^{:tag "[[Ljava.lang.CharSequence;"} vs' (make-array CharSequence 1 1)
            ^{:tag "[[Ljava.lang.CharSequence;"} fs' (make-array CharSequence 1 1)
            _       (aset ^{:tag "[Ljava.lang.CharSequence;"} (aget vs' 0) 0 vs-src)
            _       (aset ^{:tag "[Ljava.lang.CharSequence;"} (aget fs' 0) 0 fs-src)
            vs      (ShaderCode. GL3/GL_VERTEX_SHADER 1 vs')
            fs      (ShaderCode. GL3/GL_FRAGMENT_SHADER 1 fs')
            prog    (doto (ShaderProgram.)
                      (.add vs)
                      (.add fs)
                      (.init gl))
            prog-id (.program prog)]
        (bind-attrib-locations gl prog-id (:attribs spec))
        (if (.link prog gl System/out)
          (let [attribs  (init-shader-attribs gl prog-id (:attribs spec))
                uniforms (init-shader-uniforms gl prog-id (:uniforms spec))]
            (merge spec
                   {:program  prog-id
                    :uniforms uniforms
                    :attribs  attribs
                    :vs       vs-src
                    :fs       fs-src}))
          (err/throw! (str "Shader failed to link:" (ShaderUtil/getProgramInfoLog gl prog-id)))))))

   :cljs
   (defn make-shader-from-spec
     [^WebGLRenderingContext gl spec]
     (let [{:keys [uniforms attribs]} spec
           {:keys [vs-src fs-src]} (prepare-shader-sources spec)
           vs       (compile-shader gl vs-src glc/vertex-shader)
           fs       (compile-shader gl fs-src glc/fragment-shader)
           prog     (.createProgram gl)]
       (doto gl
         (.attachShader prog vs)
         (.attachShader prog fs)
         (bind-attrib-locations prog attribs)
         (.linkProgram prog))
       (if (.getProgramParameter gl prog glc/link-status)
         (let [attribs  (init-shader-attribs gl prog attribs)
               uniforms (init-shader-uniforms gl prog uniforms)]
           (doto gl
             (.deleteShader vs)
             (.deleteShader fs))
           (merge spec
                  {:program  prog
                   :uniforms uniforms
                   :attribs  attribs
                   :vs       vs-src
                   :fs       fs-src}))
         (err/throw! (str "Shader failed to link:" (.getProgramInfoLog gl prog)))))))

#?(:cljs
   (defn make-shader-from-dom
     [^WebGLRenderingContext gl {:keys [vs fs] :as spec}]
     (make-shader-from-spec
      gl (assoc spec
                :vs (glu/get-script-text vs)
                :fs (glu/get-script-text fs)))))
