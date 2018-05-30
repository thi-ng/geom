(ns thi.ng.geom.types
  (:require
   [thi.ng.geom.vector])
  #?(:clj (:import [thi.ng.geom.vector Vec2 Vec3])))

(defrecord Graph [vertices edges])

(defrecord Bezier2 [points])

(defrecord Circle2 #?(:clj [^Vec2 p ^double r] :cljs [p r]))

(defrecord Ellipse2 #?(:clj [^Vec2 p ^double rx ^double ry] :cljs [p rx ry]))

(defrecord Line2 [points])

(defrecord LineStrip2 [points])

(defrecord Mesh2 [vertices normals fnormals vnormals edges faces attribs])

(defrecord Path2 [segments])

(defrecord Polygon2 [points])

(defrecord Rect2 #?(:clj [^Vec2 p ^Vec2 size] :cljs [p size]))

(defrecord Triangle2 [points])

;; 3D

(defrecord AABB [^Vec3 p size])

(defrecord Cuboid [points])

(defrecord Bezier3 [points])

(defrecord BasicMesh [vertices faces fnormals])

(defrecord GMesh [vertices normals fnormals vnormals edges faces])

(defrecord IndexedMesh [vertices faces attribs])

(defrecord Line3 [points])

(defrecord LineStrip3 [points])

(defrecord Plane #?(:clj [n ^double w] :cljs [n w]))

(defrecord Quad3 [points])

(defrecord Sphere #?(:clj [^Vec3 p ^double r] :cljs [p r]))

(defrecord Tetrahedron [points])

(defrecord Triangle3 [points])
