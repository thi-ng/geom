(ns thi.ng.geom.examples.gl.ex04
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
   [thi.ng.geom.plane :as pl]
   [thi.ng.geom.gl.shaders.phong :as phong]))

(enable-console-print!)

(defn raycast
  [p eye ground back]
  (let [dir (m/- p eye)
        i1  (:p (g/intersect-ray ground eye dir))
        i2  (:p (g/intersect-ray back eye dir))]
    (if (< (g/dist-squared eye i1) (g/dist-squared eye i2)) i1 i2)))

(defn ^:export demo
  []
  (let [gl         (gl/gl-context "main")
        view-rect  (gl/get-viewport-rect gl)
        shader     (sh/make-shader-from-spec gl phong/shader-spec)
        cam        (cam/perspective-camera
                    {:eye (vec3 1 2 6)
                     :target (vec3 0 0.6 0)
                     :aspect view-rect
                     :far 10})
        size       3
        ground-y   -0.55
        uniforms   {:model     M44
                    :shininess 1000
                    :lightPos  (vec3 -1 2 0)}
        box        (-> (a/aabb 1)
                       (g/center)
                       (g/as-mesh {:mesh (glm/indexed-gl-mesh 12 #{:fnorm})})
                       (gl/as-gl-buffer-spec {})
                       (gl/make-buffers-in-spec gl glc/static-draw)
                       (assoc :shader shader :uniforms (assoc uniforms :diffuseCol [1 0 1])))
        ground     (pl/plane-with-point (vec3 0 ground-y 0) v/V3Y)
        back       (pl/plane-with-point (vec3 0 0 (* -0.5 size)) v/V3Z)
        planes     (-> (g/as-mesh back {:mesh (glm/indexed-gl-mesh 4 #{:fnorm}) :size size})
                       (g/translate (vec3 0 (+ (* 0.5 size) ground-y) 0))
                       (g/into (g/as-mesh ground {:size size}))
                       (gl/as-gl-buffer-spec {})
                       (gl/make-buffers-in-spec gl glc/static-draw)
                       (assoc :shader shader :uniforms uniforms))
        state      (volatile! {:mpos (g/centroid view-rect) :update-ray true})
        update-pos #(vswap! state assoc
                            :mpos (vec2 (.-clientX %) (.-clientY %))
                            :update-ray true)]
    (.addEventListener js/window "mousemove" update-pos)
    (.addEventListener js/window "touchmove" #(do (.preventDefault %) (update-pos (aget (.-touches %) 0))))
    (anim/animate
     (fn [t frame]
       (let [cam  (cam/set-view cam {:eye #(g/rotate-y % (Math/sin t))})
             isec (if (:update-ray @state)
                    (let [p (-> (vec3 (:mpos @state) 0)
                                (mat/unproject-point (m/invert (m/* (:proj cam) (:view cam))) view-rect)
                                (raycast (:eye cam) ground back))]
                      (vswap! state assoc :isec p :update-ray false) p)
                    (:isec @state))]
         (doto gl
           (gl/set-viewport view-rect)
           (gl/clear-color-and-depth-buffer 0.52 0.5 0.5 1 1)
           (gl/draw-with-shader (cam/apply planes cam))
           (gl/draw-with-shader
            (-> box
                (cam/apply cam)
                (assoc-in [:uniforms :model] (g/translate M44 isec))))))
       true))
    state))
