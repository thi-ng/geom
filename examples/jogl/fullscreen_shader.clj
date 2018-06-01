(ns thi.ng.geom.examples.jogl.fullscreen-shader
  (:import
   [com.jogamp.opengl GL3 GLAutoDrawable]
   [com.jogamp.newt.event MouseEvent KeyEvent])
  (:require
   [thi.ng.math.core :as m]
   [thi.ng.color.core :as col]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.vector :as v]
   [thi.ng.geom.matrix :as mat]
   [thi.ng.geom.gl.core :as gl]
   [thi.ng.geom.gl.fx :as fx]
   [thi.ng.geom.gl.shaders :as sh]
   [thi.ng.geom.gl.jogl.core :as jogl]
   [thi.ng.geom.gl.jogl.constants :as glc]
   [clojure.string :as str]))

(def app
  (atom {:version 330
         :example-id :sky
         :mpos (v/vec2)}))

(def shader-examples
  {:basic "
void mainImage(vec2 pos, vec2 aspect) {
  vec2 mp = vec2(mpos.x, 1.0 - mpos.y);
  float d = length((mp - pos) * aspect);
  float amp = sin(time) * 2.0 + 4.0;
  float l = 1.0 - d * amp;
  vec3 col = vec3(l);
  fragColor = vec4(col, 1.0);
}"
   :sky "
// based on http://glslsandbox.com/e#31148.0
float hash(float n) { return fract(sin(n) * 758.5453); }

float noise(vec3 x) {
    vec3 p = floor(x);
    vec3 f = fract(x);
    // f = f * f * (3.0 - 2.0 * f);
    float n = p.x + p.y * 57.0 + p.z * 800.0;
    return mix(mix(mix(hash(n), hash(n + 1.0), f.x), mix(hash(n + 57.0), hash(n + 58.0), f.x), f.y),
        mix(mix(hash(n + 800.0), hash(n + 801.0), f.x), mix(hash(n + 857.0), hash(n + 858.0), f.x), f.y), f.z);
}

float fbm(vec3 p) {
    float f = 0.0;
    f += 0.50000 * noise(p); p *= 2.02;
    f -= 0.25000 * noise(p); p *= 2.03;
    f += 0.12500 * noise(p); p *= 2.01;
    f += 0.06250 * noise(p); p *= 2.04;
    f -= 0.03125 * noise(p);
    return f / 0.984375;
}

float cloud(vec3 p) {
  p -= fbm(vec3(p.x, p.y, 0.0) * 0.5) * 2.25;
  float a = max(0.0, -(fbm(p * 3.0) * 2.2 - 1.1));
  return a * a;
}

vec3 f2(vec3 c) {
  vec2 mp = vec2(mpos.x, 1.0 - mpos.y);
  c += hash(gl_FragCoord.x + gl_FragCoord.y * 9.9) * 0.01;
  c *= 0.7 - length(gl_FragCoord.xy / resolution.xy - mp) * 0.5;
  float w = length(c);
  return mix(c * vec3(1.0, 1.0, 1.6), vec3(1.4, 1.2, 1.0) * w, w * 1.1 - 0.2);
}

void mainImage(vec2 pos, vec2 aspect) {
  pos.y += 0.2;
  vec2 coord = vec2((pos.x - 0.5) / pos.y, 1.0 / (pos.y + 0.2));
  // coord += fbm(vec3(coord * 18.0, time * 0.001)) * 0.07;
  coord += time * 0.1;
  float q = cloud(vec3(coord, 0.222));
  vec3 col = vec3(0.2, 0.7, 0.8) + vec3(0.2, 0.4, 0.1) * q;
  fragColor = vec4(f2(col), 1.0);
}"})

(def shader-spec
  {:vs fx/passthrough-vs
   :fs "
//layout(origin_upper_left) in vec4 gl_FragCoord;
out vec4 fragColor;

{{user-code}}

void main() {
  vec2 aspect = vec2(1.0, resolution.y / resolution.x);
  vec2 pos = gl_FragCoord.xy / resolution;
  mainImage(pos, aspect);
}"
   :uniforms {:tex        [:sampler2D 0]
              :time       [:float 0]
              :resolution [:vec2 [1280 720]]
              :mpos       [:vec2 [0 0]]
              :model      [:mat4 mat/M44]}
   :varying  {:vUV :vec2}
   :attribs  {:position [:vec2 0]
              :uv       [:vec2 1]}
   :state    {:depth-test false}})

(defn prepare-example
  [id]
  (update shader-spec :fs str/replace "{{user-code}}" (shader-examples id)))

(defn init
  [^GLAutoDrawable drawable]
  (let [{:keys [example-id version]} @app
        ^GL3 gl   (.. drawable getGL getGL3)
        view-rect (gl/get-viewport-rect gl)
        shader    (sh/make-shader-from-spec gl (prepare-example example-id) version)
        quad      (assoc (fx/init-fx-quad gl) :shader shader)]
    (swap! app merge
           {:quad   quad
            :shader shader})))

(defn display
  [^GLAutoDrawable drawable t]
  (let [^GL3 gl (.. drawable getGL getGL3)
        {:keys [quad width height mpos]} @app]
    (doto gl
      (gl/set-viewport 0 0 width height)
      (gl/draw-with-shader
       (update quad :uniforms merge
               {:time       t
                :mpos       (m/div mpos width height)
                :resolution [width height]})))))

(defn key-pressed
  [^KeyEvent e]
  (condp = (.getKeyCode e)
    KeyEvent/VK_ESCAPE (jogl/destroy-window (:window @app))
    nil))

(defn mouse-moved [^MouseEvent e] (swap! app assoc :mpos (v/vec2 (.getX e) (.getY e))))

(defn resize [_ x y w h] (swap! app assoc :width w :height h))

(defn dispose [_] (jogl/stop-animator (:anim @app)))

(defn -main
  [& args]
  (swap! app merge
         (jogl/gl-window
          {:profile       :gl3
           :samples       4
           :double-buffer true
           :fullscreen    true
           :events        {:init    init
                           :display display
                           :dispose dispose
                           :resize  resize
                           :keys    {:press key-pressed}
                           :mouse   {:move mouse-moved}}}))
  nil)

(-main)
