(ns thi.ng.geom.examples.jogl.stl-mesh
  (:import
   [com.jogamp.opengl GL3 GLAutoDrawable]
   [com.jogamp.newt.event MouseEvent KeyEvent])
  (:require
   [thi.ng.math.core :as m]
   [thi.ng.color.core :as col]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.aabb :as a]
   [thi.ng.geom.attribs :as attr]
   [thi.ng.geom.vector :as v]
   [thi.ng.geom.matrix :as mat]
   [thi.ng.geom.quaternion :as q]
   [thi.ng.geom.utils :as gu]
   [thi.ng.geom.mesh.io :as mio]
   [thi.ng.geom.gl.core :as gl]
   [thi.ng.geom.gl.arcball :as arc]
   [thi.ng.geom.gl.shaders :as sh]
   [thi.ng.geom.gl.shaders.phong :as phong]
   [thi.ng.geom.gl.glmesh :as glm]
   [thi.ng.geom.gl.jogl.core :as jogl]
   [thi.ng.geom.gl.jogl.constants :as glc]
   [thi.ng.glsl.core :as glsl]
   [clojure.java.io :as io]))

(def app (atom nil))

(defn load-mesh
  "Loads STL mesh from given path and fits it into centered bounding box."
  [path bounds]
  (with-open [in (io/input-stream path)]
    (->> (mio/read-stl (mio/wrapped-input-stream in) #(glm/gl-mesh % #{:fnorm}))
         vector
         (gu/fit-all-into-bounds (g/center bounds))
         first)))

(defn init
  [^GLAutoDrawable drawable]
  (let [^GL3 gl   (.. drawable getGL getGL3)
        view-rect (gl/get-viewport-rect gl)
        shader    (sh/make-shader-from-spec gl (assoc phong/shader-spec :version 330))
        model     (-> (load-mesh "assets/suzanne.stl" (a/aabb 2))
                      (gl/as-gl-buffer-spec {})
                      (update :uniforms merge
                              {:lightPos [0 2 2]
                               :view (mat/look-at (v/vec3 0 0 1) (v/vec3) v/V3Y)
                               :shininess 50
                               :wrap 1
                               :ambientCol [0.0 0.1 0.4 0.0]
                               :diffuseCol [0.1 0.5 0.6]
                               :specularCol [0.8 0.3 0.3]})
                      (assoc :shader shader)
                      (gl/make-buffers-in-spec gl glc/static-draw))]
    (swap! app assoc
           :model     model
           :wireframe false
           :arcball   (arc/arcball {:init (m/normalize (q/quat 0.0 0.707 0.707 0))}))))

(defn display
  [^GLAutoDrawable drawable t]
  (let [^GL3 gl (.. drawable getGL getGL3)
        {:keys [model wireframe arcball]} @app
        view    (arc/get-view arcball)]
    (doto gl
      (gl/clear-color-and-depth-buffer col/GRAY 1)
      (.glPolygonMode glc/front-and-back (if wireframe glc/line glc/fill))
      (gl/draw-with-shader (assoc-in model [:uniforms :model] view)))))

(defn resize
  [_ x y w h]
  (swap! app assoc-in [:model :uniforms :proj] (mat/perspective 45 (/ w h) 0.1 10))
  (swap! app update :arcball arc/resize w h))

(defn dispose [_] (jogl/stop-animator (:anim @app)))

(defn key-pressed
  [^KeyEvent e]
  (condp = (.getKeyCode e)
    KeyEvent/VK_ESCAPE (jogl/destroy-window (:window @app))
    (case (.getKeyChar e)
      \w (swap! app update :wireframe not)
      nil)))

(defn mouse-pressed [^MouseEvent e] (swap! app update :arcball arc/down (.getX e) (.getY e)))

(defn mouse-dragged [^MouseEvent e] (swap! app update :arcball arc/drag (.getX e) (.getY e)))

(defn wheel-moved [^MouseEvent e deltas] (swap! app update :arcball arc/zoom-delta (nth deltas 1)))

(defn -main
  [& args]
  (reset!
   app
   (jogl/gl-window
    {:profile       :gl3
     :samples       4
     :double-buffer true
     :fullscreen    false
     :events        {:init    init
                     :display display
                     :dispose dispose
                     :resize  resize
                     :keys    {:press key-pressed}
                     :mouse   {:press mouse-pressed
                               :drag  mouse-dragged
                               :wheel wheel-moved}}}))
  nil)

(-main)
