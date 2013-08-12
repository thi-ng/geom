(ns thi.ng.geom.types)

;; 2d

(defrecord Line2 [p q])
(defrecord Circle [p r])
(defrecord Polygon [points])
(defrecord Rect [p w h])
(defrecord Triangle2 [a b c])

;; 3d

(defrecord Line3 [p q])
(defrecord Plane [p n])
(defrecord AABB [p size])
(defrecord Sphere [p r])
(defrecord Triangle3 [a b c])
(defrecord Mesh [vertices normals fnormals vnormals edges faces attribs fns])
