(ns thi.ng.geom.gl.shaders.basic
  (:require
   [thi.ng.geom.matrix :refer [M44]]))

(defn make-shader-spec
  ([]
   (make-shader-spec nil))
  ([{attr? :color-attrib? is3d? :3d :or {is3d? true}}]
   (let [spec {:vs       (str "void main(){"
                              (if attr? "vCol=color;")
                              "gl_Position=proj*"
                              (if is3d?
                                "view*model*vec4(position,1.0)"
                                "view*model*vec4(position,0.0,1.0)")
                              ";}")
               :fs       #?(:clj
                            (str "out vec4 fragColor;void main(){fragColor="
                                 (if attr? "vCol" "color")
                                 ";}")
                            :cljs
                            (str "void main(){gl_FragColor="
                                 (if attr? "vCol" "color")
                                 ";}"))
               :uniforms {:proj  :mat4
                          :model [:mat4 M44]
                          :view  [:mat4 M44]}
               :attribs  {:position (if is3d? :vec3 :vec2)}}
         spec (if attr?
                (-> spec
                    (assoc-in [:attribs :color] :vec4)
                    (assoc :varying {:vCol :vec4}))
                (assoc-in spec [:uniforms :color] :vec4))]
     spec)))

(defn make-shader-spec-2d
  [color-attrib?] (make-shader-spec {:color-attrib? color-attrib? :3d false}))

(defn make-shader-spec-3d
  [color-attrib?] (make-shader-spec {:color-attrib? color-attrib? :3d true}))
