(ns thi.ng.geom.gl.shaders.image
  (:require
   [thi.ng.geom.core :as g]
   [thi.ng.geom.vector :refer [vec2 vec3]]
   [thi.ng.geom.matrix :refer [M44]]
   [thi.ng.geom.rect :as r]
   [thi.ng.geom.gl.core :as gl]
   [thi.ng.geom.gl.shaders :as sh]
   [thi.ng.dstruct.core :as d]
   [thi.ng.dstruct.streams :as streams]
   [thi.ng.math.core :as m]
   [thi.ng.glsl.core :as glsl :include-macros true]
   #?@(:clj
       [[thi.ng.geom.gl.jogl.constants :as glc]
        [thi.ng.geom.gl.jogl.buffers :as native]]
       :cljs
       [[thi.ng.geom.gl.webgl.constants :as glc]
        [thi.ng.typedarrays.core :as ta]])))

(def shader-spec
  {:vs "void main(){vUV=uv;gl_Position=proj*model*vec4(position,0.0,1.0);}"
   :fs #?(:clj  "out vec4 fragColor;void main(){fragColor=texture(tex,vUV)*tint;}"
          :cljs "void main(){gl_FragColor=texture2D(tex,vUV)*tint;}")
   :uniforms {:proj       :mat4
              :model      [:mat4 M44]
              :tex        [:sampler2D 0]
              :tint       [:vec4 [1 1 1 1]]}
   :attribs  {:position   :vec2
              :uv         :vec2}
   :varying  {:vUV        :vec2}
   :state    {:depth-test false
              :blend      true
              :blend-func [glc/src-alpha glc/one-minus-src-alpha]}})

(defn make-shader-spec
  ([gl]
   (make-shader-spec gl nil))
  ([gl opts]
   (let [r         (or (get opts :rect) (r/rect 1))
         [a b c d] (g/vertices r)
         verts     (#?(:clj native/float-buffer :cljs ta/float32) 8)]
     (streams/into-float-buffer [d c a b] verts 2 0)
     {:attribs      (gl/make-attribute-buffers
                     gl glc/static-draw
                     {:position {:data verts
                                 :size 2}
                      :uv       {:data (#?(:clj native/float-buffer :cljs ta/float32) [0 0, 1 0, 0 1, 1 1])
                                 :size 2}})
      :uniforms     {:tex  0
                     :proj (gl/ortho)}
      :shader       (d/merge-deep
                     (or (get opts :shader) (sh/make-shader-from-spec gl shader-spec))
                     {:state (get opts :state)})
      :view-port    (get opts :viewport)
      :pos          (get opts :pos (vec2))
      :width        (get opts :width 128)
      :height       (get opts :height 128)
      :mode         glc/triangle-strip
      :num-vertices 4})))

(defn draw
  [gl {:keys [viewport pos width height] :as spec}]
  (let [[vw vh] (get (or viewport (gl/get-viewport-rect gl)) :size)
        x       (m/map-interval (nth pos 0) 0 vw -1 1)
        y       (m/map-interval (nth pos 1) 0 vh -1 1)
        s       (vec3 (* 2.0 (/ width vw)) (* 2.0 (/ height vh)) 1.0)
        spec    (assoc-in spec [:uniforms :model] (-> M44 (g/translate (vec3 x y 0)) (g/scale s)))]
    (gl/draw-with-shader gl spec)))
