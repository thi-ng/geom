(ns thi.ng.geom.examples.jogl.ex01
  (:import
   [com.jogamp.opengl GL3 GLAutoDrawable]
   [com.jogamp.newt.event MouseEvent KeyEvent])
  (:require
   [thi.ng.math.core :as m]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.aabb :as a]
   [thi.ng.geom.attribs :as attr]
   [thi.ng.geom.vector :as v]
   [thi.ng.geom.matrix :as mat]
   [thi.ng.geom.gl.core :as gl]
   [thi.ng.geom.gl.arcball :as arc]
   [thi.ng.geom.gl.buffers :as buf]
   [thi.ng.geom.gl.shaders :as sh]
   [thi.ng.geom.gl.glmesh :as glm]
   [thi.ng.geom.gl.jogl.core :as jogl]
   [thi.ng.geom.gl.jogl.constants :as glc]
   [clojure.pprint :refer [pprint]]
   [clojure.java.io :as io]))

(def app (atom nil))

(def shader
  {:vs "
  void main() {
    vCol = vec4(position.xy * 0.5 + 0.5, fract(time), 1.0);
    vUV = uv;
    gl_Position = proj * view * model * vec4(position, 1.0);
  }"
   :fs "out vec4 fragColor;
  void main() {
    fragColor = vCol * texture(tex, vUV);
  }"
   :version  330
   :attribs  {:position :vec3
              :uv       :vec2}
   :varying  {:vCol     :vec4
              :vUV      :vec2}
   :uniforms {:model [:mat4 mat/M44]
              :view  :mat4
              :proj  :mat4
              :tex   [:sampler2D 0]
              :time  :float}
   :state    {:depth-test false
              :blend      true
              :blend-fn   [glc/src-alpha glc/one]}})

(defn init
  [^GLAutoDrawable drawable]
  (let [^GL3 gl (.. drawable getGL getGL3)
        tex     (buf/load-texture gl {:src (io/file "dev-resources/cubev.png")})
        model   (-> (a/aabb 1)
                    (g/center)
                    (g/as-mesh {:mesh    (glm/gl-mesh 12 #{:uv})
                                :attribs {:uv (attr/face-attribs (attr/uv-cube-map-v 256 false))}})
                    (gl/as-gl-buffer-spec {})
                    (assoc :shader (sh/make-shader-from-spec gl shader))
                    (assoc-in [:shader :state :tex] tex)
                    (gl/make-buffers-in-spec gl glc/static-draw))]
    (swap! app assoc :model model :arcball (arc/arcball {}))))

(defn display
  [^GLAutoDrawable drawable t]
  (let [{:keys [model arcball]} @app
        ^GL3 gl (.. drawable getGL getGL3)]
    (doto gl
      (gl/clear-color-and-depth-buffer 0.3 0.3 0.3 1.0 1.0)
      (gl/draw-with-shader
       (update model :uniforms assoc
               :view (arc/get-view arcball)
               :time (* 0.25 t))))))

(defn dispose [_] (jogl/stop-animator (:anim @app)))

(defn resize
  [_ x y w h]
  (swap! app assoc-in [:model :uniforms :proj] (mat/perspective 45 (/ w h) 0.1 10))
  (swap! app update :arcball arc/resize w h))

(defn key-pressed
  [^KeyEvent e]
  (condp = (.getKeyCode e)
    KeyEvent/VK_ESCAPE (jogl/destroy-window (:window @app))
    nil))

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
                     :resize  resize
                     :keys    {:press key-pressed}
                     :mouse   {:press mouse-pressed
                               :drag  mouse-dragged
                               :wheel wheel-moved}}}))
  nil)
