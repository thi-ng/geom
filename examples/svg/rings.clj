(ns thi.ng.geom.examples.svg.rings
  (:require
   [thi.ng.geom.core :as g]
   [thi.ng.geom.vector :refer [vec3]]
   [thi.ng.geom.matrix :as mat]
   [thi.ng.geom.circle :as c]
   [thi.ng.geom.polygon :as p]
   [thi.ng.geom.gmesh :as gm]
   [thi.ng.geom.mesh.subdivision :as sd]
   [thi.ng.geom.svg.core :as svg]
   [thi.ng.geom.svg.shaders :as shader]
   [thi.ng.geom.svg.renderer :as render]
   [thi.ng.math.core :as m]))

(def width    640)
(def height   480)
(def model    (g/rotate-y (mat/matrix44) m/SIXTH_PI))
(def view     (apply mat/look-at (mat/look-at-vectors 0 1.75 0.75 0 0 0)))
(def proj     (mat/perspective 60 (/ width height) 0.1 10))
(def mvp      (->> model (m/* view) (m/* proj)))

(def diffuse  (shader/normal-rgb (g/rotate-y (mat/matrix44) m/PI)))
(def uniforms {:stroke "white" :stroke-width 0.25})

(def shader-diffuse
  (shader/shader
   {:fill     diffuse
    :uniforms uniforms
    :flags    {:solid true}}))

(def shader-lambert
  (shader/shader
   {:fill     (shader/lambert
               {:view      view
                :light-dir [-1 0 1]
                :light-col [1 1 1]
                :diffuse   diffuse
                :ambient   0.1})
    :uniforms uniforms
    :flags    {:solid true}}))

(def shader-phong
  (shader/shader
   {:fill     (shader/phong
               {:model     model
                :view      view
                :light-pos [-1 2 1]
                :light-col [1 1 1]
                :diffuse   diffuse
                :ambient   [0.05 0.05 0.2]
                :specular  0.8
                :shininess 8.0})
    :uniforms uniforms
    :flags    {:solid true}}))

(defn ring
  [res radius depth wall]
  (-> (c/circle radius)
      (g/as-polygon res)
      (g/extrude-shell {:depth depth :wall wall :inset -0.1 :mesh (gm/gmesh)})
      (g/center)))

(def mesh
  (->> [[1 0.25 0.15] [0.75 0.35 0.1] [0.5 0.5 0.05] [0.25 0.75 0.05]]
       (map (partial apply ring 40))
       (reduce g/into)
       (sd/catmull-clark)
       (sd/catmull-clark)))

;; 2d text label w/ projected anchor point
(defn label-3d
  [p mvp screen [l1 l2]]
  (let [p'  (mat/project-point p mvp screen)
        p2' (mat/project-point (m/+ p 0 0 0.2) mvp screen)
        p3' (m/+ p2' 100 0)]
    (svg/group
     {:fill "black"
      :font-family "Arial"
      :font-size 12
      :text-anchor "end"}
     (svg/circle p' 2 nil)
     (svg/line-strip [p' p2' p3'] {:stroke "black"})
     (svg/text (m/+ p3' 0 -5) l1 {})
     (svg/text (m/+ p3' 0 12) l2 {:font-weight "bold"}))))

(defn render-svg
  [path mesh mvp shader width height labels]
  (let [screen (mat/viewport-matrix width height)
        max-z  (/ 0.75 2)]
    (->> (svg/svg
          {:width width :height height}
          (render/mesh mesh mvp screen shader)
          (label-3d (vec3 0 0 max-z) mvp screen labels))
         (svg/serialize)
         (spit path))))

(render-svg "out/svg-rings-diffuse.svg"
            mesh mvp shader-diffuse width height
            ["Shader" "Normal/RGB"])
(render-svg "out/svg-rings-lambert.svg"
            mesh mvp shader-lambert width height
            ["Shader" "Lambert"])
(render-svg "out/svg-rings-phong.svg"
            mesh mvp shader-phong width height
            ["Shader" "Blinn-Phong"])
