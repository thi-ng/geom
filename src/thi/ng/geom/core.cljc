(ns thi.ng.geom.core
  (:refer-clojure :exclude [into]))

(def ^:dynamic *resolution* 20)

(defprotocol IDistance
  (dist [_ a])
  (dist-squared [_ a]))

(defprotocol IHeading
  (heading [_])
  (heading-xy [_])
  (heading-xz [_])
  (heading-yz [_])
  (angle-between [_ a])
  (slope-xy [_])
  (slope-xz [_])
  (slope-yz [_]))

(defprotocol INormal
  (normal [_] [_ a]))

(defprotocol IPolar
  (as-polar [_])
  (as-cartesian [_]))

(defprotocol IVectorReduce
  (reduce-vector [_ f xs] [_ f f2 xs]))

(defprotocol IRotate
  (rotate [_ t]))

(defprotocol IRotate3D
  (rotate-x [_ t])
  (rotate-y [_ t])
  (rotate-z [_ t])
  (rotate-around-axis [_ a t]))

(defprotocol IReflect
  (reflect [_ r]))

(defprotocol IScale
  (scale [_ s])
  (scale-size [_ s]))

(defprotocol IShear
  (shear [_ s]))

(defprotocol ITransform
  (transform [_ tx]))

(defprotocol ITranslate
  (translate [_ t]))

(defprotocol IVectorTransform
  (transform-vector [_ v]))

(defprotocol IMatrixConvert
  (as-matrix [_] [_ opts]))

(defprotocol IGraph
  (connected-components [_])
  (cyclic? [_])
  (vertex-valence [_ v])
  (vertex-neighbors [_ v])
  (remove-vertex [_ v])
  (replace-vertex [_ a b])
  (merge-vertices [_ a b]))

(defprotocol IDirectedGraph
  (in-vertices [_ v])
  (out-vertices [_ v]))

(defprotocol IGraphConvert
  (as-graph [_] [_ opts]))

(defprotocol IVertexAccess
  (vertices [_] [_ opts]))

(defprotocol IEdgeAccess
  (edges [_] [_ opts]))

(defprotocol IFaceAccess
  (add-face [_ face]
    "Adds a new face to a mesh. The face arg is a 2-element vector
    of [vertices attributes], where attributes is a map of arbitrary
    vertex attributes. E.g. (g/add-face m [verts {:uv [[0 0] [1 0] [1 1]]}])")
  (faces [_] [_ opts]
    "Returns a mesh's face collection. If called without opts arg,
    returns faces directly as used internally by the mesh. If opts is
    truthy, returns each face as vector with a vector of face vertices
    as its first element. If the mesh supports vertex attributes,
    they're returned as the 2nd element in each face vector.")
  (remove-face [_ fverts]
    "Takes a vector of vertices defining a face in the mesh and
    returns updated mesh without that face.")
  (vertex-faces [_ v]
    "Returns seq of faces the given vertex is part of."))

(defprotocol INormalAccess
  (compute-face-normals [_])
  (compute-vertex-normals [_])
  (face-normals [_ force?])
  (face-normal [_ f])
  (vertex-normals [_ force?])
  (vertex-normal [_ v]))

(defprotocol IAttributeAccess
  (attribs [_ ctx] [_ ctx attr]))

(defprotocol IRawAccess
  (raw [_ ctx]))

(defprotocol IAlign
  (align-with [_ x opts]
    "Returns updated entity aligned with `x` in the specified manner"))

(defprotocol IArea
  (area [_]
    "Returns an entity's total surface area"))

(defprotocol IBoundary
  (contains-entity? [_ s])
  (contains-point? [_ p]))

(defprotocol IBoundingCircle
  (bounding-circle [_]))

(defprotocol IBoundingSphere
  (bounding-sphere [_]))

(defprotocol IBounds
  (bounds [_]
    "Returns bounding rect for 2d entities or box for 3d")
  (depth [_]
    "Returns entity's extent along Z (zero for 2d)")
  (height [_]
    "Returns entity's extent along Y axis")
  (width [_]
    "Returns entity's extent along X axis"))

(defprotocol ICenter
  (center [_] [_ p]
    "Returns updated entity centered around world origin or given point")
  (centroid [_]
    "Returns centroid of entity"))

(defprotocol ICircumference
  (circumference [_]
    "Returns an entity's circumference"))

(defprotocol IClassify
  (classify-point [_ p]))

(defprotocol IClear
  (clear* [_])
  (clear! [_]))

(defprotocol IClip
  (clip-with [_ s]))

(defprotocol IConvexHull
  (convex-hull [_]))

(defprotocol IExtrude
  (extrude [_ opts])
  (extrude-shell [_ opts]))

(defprotocol IFlip
  (flip [_]))

(defprotocol IGeomContainer
  (into [_ m]))

(defprotocol IInset
  (inset [_ inset]))

(defprotocol IIntersect
  (intersect-line [_ l] [_ p q])
  (intersect-ray [_ r] [_ p dir])
  (intersect-shape [_ s]))

(defprotocol IMeshConvert
  (as-mesh [_] [_ opts]
    "Transforms the current enitity into a mesh instance"))

(defprotocol IPointMap
  (map-point [_ p])
  (unmap-point [_ p]))

(defprotocol IPolygonConvert
  (as-polygon [_] [_ res]
    "Transforms current entity into a Polygon2 instance"))

(defprotocol IProximity
  (closest-point [_ p]))

(defprotocol ISample
  (point-at [_ t] [_ u v])
  (random-point [_])
  (random-point-inside [_])
  (sample-uniform [_ dist include-last?])
  (sample-with-resolution [_ res]))

(defprotocol ISlice
  (slice-with [_ e] [_ e classifier] [_ e classifier parts]))

(defprotocol ISubdivide
  (subdivide [_] [_ opts])
  (subdivide-edge [_ a b splits])
  (subdivide-face [_ f p displace splits]))

(defprotocol ITessellate
  (tessellate [_] [_ opts]))

(defprotocol IVolume
  (volume [_] "Returns an entity's inner volume.")
  (signed-volume [_]))

(defprotocol ISpatialTree
  (add-point [_ p d])
  (delete-point [_ p])
  (get-point [_])
  (get-point-data [_]))

(defprotocol IMutableProps
  (get-prop [_ k])
  (set-prop! [_ k v])
  (update-prop! [_ k f]))
