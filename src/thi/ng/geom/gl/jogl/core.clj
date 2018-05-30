(ns thi.ng.geom.gl.jogl.core
  (:import
   [com.jogamp.opengl
    GL GL2 GL3 GL4 GLProfile GLCapabilities GLAutoDrawable GLEventListener]
   [com.jogamp.opengl.util Animator GLBuffers]
   [com.jogamp.opengl.util.glsl ShaderCode ShaderProgram]
   [com.jogamp.newt NewtFactory]
   [com.jogamp.newt.opengl GLWindow]
   [com.jogamp.newt.event MouseEvent MouseListener KeyEvent KeyListener]
   [java.nio Buffer FloatBuffer])
  (:require
   [thi.ng.geom.core :as g]
   [thi.ng.geom.vector :as v]
   [thi.ng.geom.matrix :as mat]))

(defn gl-event-proxy
  [{:keys [init dispose resize display]}]
  (let [t0 (System/nanoTime)]
    (proxy [GLEventListener] []
      (init [^GLAutoDrawable drawable]
        (when init (init drawable)))
      (dispose [^GLAutoDrawable drawable]
        (when dispose (dispose drawable)))
      (reshape [^GLAutoDrawable drawable x y width height]
        (when resize (resize drawable x y width height)))
      (display [^GLAutoDrawable drawable]
        (when display
          (display drawable (* (- (System/nanoTime) t0) 1e-9)))))))

(defn mouse-event-proxy
  [{:keys [click drag enter exit move press release wheel]}]
  (proxy [MouseListener] []
    (mouseClicked [^MouseEvent e]
      (when click (click e)))
    (mouseDragged [^MouseEvent e]
      (when drag (drag e)))
    (mouseEntered [^MouseEvent e]
      (when enter (enter e)))
    (mouseExited [^MouseEvent e]
      (when exit (exit e)))
    (mouseMoved [^MouseEvent e]
      (when move (move e)))
    (mousePressed [^MouseEvent e]
      (when press (press e)))
    (mouseReleased [^MouseEvent e]
      (when release (release e)))
    (mouseWheelMoved [^MouseEvent e]
      (when wheel
        (let [s (.getRotationScale e)]
          (wheel e (map #(* % s) (.getRotation e))))))))

(defn key-event-proxy
  [{:keys [press release]}]
  (proxy [KeyListener] []
    (keyPressed [^KeyEvent e]
      (when press (press e)))
    (keyReleased [^KeyEvent e]
      (when release (release e)))))

(def ^:private gl-profiles
  {:gl2 GLProfile/GL2
   :gl3 GLProfile/GL3
   :gl4 GLProfile/GL4})

(defn gl-window
  [opts]
  (let [display (NewtFactory/createDisplay nil)
        screen  (NewtFactory/createScreen display (get opts :screen 0))
        profile (GLProfile/get (gl-profiles (get opts :profile :gl3)))
        caps    (doto (GLCapabilities. profile)
                  (.setSampleBuffers (if (pos? (get opts :samples 0)) true false))
                  (.setNumSamples (get opts :samples 1))
                  (.setDoubleBuffered (get opts :double-buffer false)))
        win     (GLWindow/create screen caps)]
    (doto win
      (.setSize (get opts :width 1280) (get opts :height 720))
      (.setPosition (get opts :x 50) (get opts :y 50))
      (.setUndecorated (not (get opts :chrome true)))
      (.setAlwaysOnTop (get opts :always-on-top false))
      (.setFullscreen (boolean (get opts :fullscreen false)))
      (.setPointerVisible (boolean (get opts :pointer-visible true)))
      (.confinePointer (boolean (get opts :pointer-locked false)))
      (.setTitle (get opts :title "GLWindow"))
      (.setVisible (boolean (get opts :visible true)))
      (.addGLEventListener (gl-event-proxy (get opts :events))))
    (when-let [m (get-in opts [:events :mouse])]
      (.addMouseListener win (mouse-event-proxy m)))
    (when-let [k (get-in opts [:events :keys])]
      (.addKeyListener win (key-event-proxy k)))
    {:display display
     :screen  screen
     :window  win
     :profile profile
     :caps    caps
     :anim    (doto (Animator. win) (.start))}))

(defn destroy-window
  [^GLWindow window] (.destroy window))

(defn stop-animator
  [^Animator animator] (.stop animator))
