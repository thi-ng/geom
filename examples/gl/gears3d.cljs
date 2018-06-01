(ns thi.ng.geom.examples.gl.gears3d
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
   [thi.ng.geom.gl.shaders.phong :as phong]
   [thi.ng.geom.circle :as c]
   [thi.ng.geom.polygon :as poly]))

(defn ^:export demo
  []
  (enable-console-print!)
  (let [gl        (gl/gl-context "main")
        view-rect (gl/get-viewport-rect gl)
        teeth     20
        model     (-> (poly/cog 0.5 teeth [0.9 1 1 0.9])
                      (g/extrude-shell {:mesh    (glm/indexed-gl-mesh 1000 #{:fnorm})
                                        :depth   0.1
                                        :inset   0.025
                                        :wall    0.015
                                        :bottom? true})
                      (gl/as-gl-buffer-spec {})
                      (cam/apply (cam/perspective-camera {:aspect view-rect}))
                      (assoc :shader (sh/make-shader-from-spec gl phong/shader-spec))
                      (update :uniforms merge
                              {:lightPos      (vec3 0.1 0 1)
                               :ambientCol    0x0e1a4c
                               :diffuseCol    0xff3310
                               :specularCol   0x99ffff
                               :shininess     100
                               :wrap          0
                               :useBlinnPhong true})
                      (gl/make-buffers-in-spec gl glc/static-draw)
                      (time))]
    (anim/animate
     (fn [t frame]
       (let [rot (g/rotate-y M44 (* t 0.5))
             tx1 (m/* rot (-> M44
                              (g/translate (vec3 -0.46 0 0))
                              (g/rotate-y 0.3)
                              (g/rotate-z t)))
             tx2 (m/* rot (-> M44
                              (g/translate (vec3 0.46 0 0))
                              (g/rotate-y -0.3)
                              (g/rotate-z (- (+ t (/ HALF_PI teeth))))))]
         (doto gl
           (gl/set-viewport view-rect)
           (gl/clear-color-and-depth-buffer 1 0.98 0.95 1 1)
           (gl/draw-with-shader (assoc-in model [:uniforms :model] tx1))
           (gl/draw-with-shader
            (-> model
                (assoc-in [:uniforms :model] tx2)
                (assoc-in [:uniforms :diffuseCol] 0x33ff80))))
         true)))))
