(ns thi.ng.geom.test.types.protocols
  #?(:cljs
     (:require-macros
      [cemerick.cljs.test :refer (is deftest with-test run-tests testing)]))
  (:require
   [thi.ng.math.core :as m]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.types]
   [thi.ng.geom.aabb]
   [thi.ng.geom.bezier]
   [thi.ng.geom.circle]
   [thi.ng.geom.cuboid]
   [thi.ng.geom.gmesh]
   [thi.ng.geom.line]
   [thi.ng.geom.path]
   [thi.ng.geom.plane]
   [thi.ng.geom.polygon]
   [thi.ng.geom.quad]
   [thi.ng.geom.rect]
   [thi.ng.geom.sphere]
   [thi.ng.geom.tetrahedron]
   [thi.ng.geom.triangle]
   #?(:clj
      [clojure.test :refer :all]
      :cljs
      [cemerick.cljs.test :as t])))

(def proto-ids
  {:area      g/IArea
   :bcircle   g/IBoundingCircle
   :boundary  g/IBoundary
   :bounds    g/IBounds
   :bsphere   g/IBoundingSphere
   :center    g/ICenter
   :chull     g/IConvexHull
   :circum    g/ICircumference
   :class     g/IClassify
   :clip      g/IClip
   :conj      m/IConjugate
   :cross     m/ICrossProduct
   :det       m/IDeterminant
   :dist      g/IDistance
   :dot       m/IDotProduct
   :edge      g/IEdgeAccess
   :ext       g/IExtrude
   :face      g/IFaceAccess
   :flip      g/IFlip
   :graph     g/IGraph
   :head      g/IHeading
   :inv       m/IInvert
   :isec      g/IIntersect
   :limit     m/ILimit
   :mag       m/IMagnitude
   :mat       g/IMatrixConvert
   :math      m/IMathOps
   :mesh      g/IMeshConvert
   :mimax     m/IMinMax
   :mix       m/IInterpolate
   :norm      m/INormalize
   :polar     g/IPolar
   :poly      g/IPolygonConvert
   :prox      g/IProximity
   :refl      g/IReflect
   :rot3d     g/IRotate3D
   :rotate    g/IRotate
   :sample    g/ISample
   :scale     g/IScale
   :shear     g/IShear
   :subdiv    g/ISubdivide
   :tess      g/ITessellate
   :translate g/ITranslate
   :tx        g/ITransform
   :vert      g/IVertexAccess
   :vol       g/IVolume
   :vtx       g/IVectorTransform
   })

(def shape-common
  #{:area :bounds :boundary :center :class :edge :graph :mesh
    :rotate :sample :scale :tess :translate :tx :vert :vol})

(def shape-common-2d
  (conj shape-common :bcircle :circum :ext :poly))

(def shape-common-3d
  (conj shape-common :bsphere))

#?(:clj
   (defn satisfies-all?
     [type & protos]
     (testing
         (.getName type)
       (doseq [p protos]
         (is (true? (extends? (proto-ids p) type))
             (str (get-in proto-ids [p :on-interface])))))))

#?(:clj
   (deftest proto-implementation-check
     (dorun
      (map
       (fn [[t protos]] (apply satisfies-all? t protos))
       {
        ;; 2d
        thi.ng.geom.types.Bezier2
        (-> shape-common-2d (disj :poly :tess) (conj :isec :prox))

        thi.ng.geom.types.Circle2
        (conj shape-common-2d :isec :prox)

        thi.ng.geom.types.Line2
        (-> shape-common-2d (disj :poly :mesh :tess) (conj :isec :prox))

        thi.ng.geom.types.Polygon2
        (conj shape-common-2d :chull :clip :flip :isec :prox)

        thi.ng.geom.types.Rect2
        (conj shape-common-2d :isec :prox :subdiv)

        thi.ng.geom.types.Triangle2
        (conj shape-common-2d :flip :isec :prox :subdiv)

        ;; 3d

        thi.ng.geom.types.Bezier3
        (-> shape-common-3d (disj :poly :tess) (conj :isec :prox))

        thi.ng.geom.types.AABB
        (conj shape-common-3d :isec :prox :subdiv)

        thi.ng.geom.types.Cuboid
        (conj shape-common-3d :isec :prox :subdiv)

        thi.ng.geom.types.Line3
        (-> shape-common-3d (disj :mesh :tess) (conj :isec :prox))

        thi.ng.geom.types.Plane
        (-> shape-common-3d (disj :edge :tess :vert) (conj :flip :isec :prox))

        thi.ng.geom.types.Quad3
        (conj shape-common-3d :flip :isec :prox :subdiv)

        thi.ng.geom.types.Sphere
        (conj shape-common-3d :isec :prox)

        thi.ng.geom.types.Tetrahedron
        (conj shape-common-3d :flip :isec :prox :subdiv)

        thi.ng.geom.types.Triangle3
        (conj shape-common-3d :flip :isec :prox :subdiv)

        }))))
