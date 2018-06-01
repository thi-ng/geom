(ns thi.ng.geom.examples.gl.stl-mesh
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
   [thi.ng.geom.mesh.io :as mio]
   [thi.ng.geom.quaternion :as q]
   [thi.ng.geom.gl.arcball :as arc]
   [thi.ng.geom.gl.shaders.phong :as phong]))

(enable-console-print!)

(def state (atom {}))

(defn load-binary
  [uri onload onerror]
  (let [xhr (js/XMLHttpRequest.)]
    (set! (.-responseType xhr) "arraybuffer")
    (set! (.-onload xhr)
          (fn [e]
            (if-let [buf (.-response xhr)]
              (onload buf)
              (when onerror (onerror xhr e)))))
    (doto xhr
      (.open "GET" uri true)
      (.send))))

(defn init-model
  [gl vrect buf]
  (let [model (-> (mio/read-stl (mio/wrapped-input-stream buf) #(glm/gl-mesh % #{:fnorm}))
                  (gl/as-gl-buffer-spec {})
                  (assoc :shader (sh/make-shader-from-spec gl phong/shader-spec))
                  (update :uniforms merge
                          {:proj          (mat/perspective 60 vrect 0.1 10)
                           :view          (mat/look-at (v/vec3 0 0 1) (v/vec3) v/V3Y)
                           :lightPos      (vec3 0.1 0 1)
                           :ambientCol    0x000011
                           :diffuseCol    0x0033ff
                           :specularCol   0xffffff
                           :shininess     100
                           :wrap          0
                           :useBlinnPhong true})
                  (gl/make-buffers-in-spec gl glc/static-draw)
                  (time))]
    (swap! state assoc :model model)))

(defn init-arcball
  [el vrect]
  (swap! state assoc :cam
         (-> (arc/arcball {:init (m/normalize (q/quat 0.0 0.707 0.707 0))})
             (arc/resize (g/width vrect) (g/height vrect))))
  (doto el
    (.addEventListener
     "mousedown"
     (fn [e]
       (doto state
         (swap! assoc :mouse-down true)
         (swap! update :cam arc/down (.-clientX e) (.-clientY e)))))
    (.addEventListener
     "mouseup"
     (fn [e] (swap! state assoc :mouse-down false)))
    (.addEventListener
     "mousemove"
     (fn [e]
       (when (:mouse-down @state)
         (swap! state update :cam arc/drag (.-clientX e) (.-clientY e)))))))

(defn ^:export demo
  []
  (let [gl    (gl/gl-context "main")
        vrect (gl/get-viewport-rect gl)]
    (load-binary
     "assets/suzanne.stl"
     (fn [buf] (init-model gl vrect buf))
     (fn [req e] (prn "error loading model")))
    (init-arcball (.getElementById js/document "main") vrect)
    (anim/animate
     (fn [t frame]
       (when-let [model (:model @state)]
         (doto gl
           (gl/set-viewport vrect)
           (gl/clear-color-and-depth-buffer col/WHITE 1)
           (gl/draw-with-shader
            (assoc-in model [:uniforms :model] (arc/get-view (:cam @state))))))
       true))))
