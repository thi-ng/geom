(ns thi.ng.geom.attribs
  #?(:cljs (:require-macros [thi.ng.math.macros :as mm]))
  (:require
   [thi.ng.geom.core :as g]
   [thi.ng.geom.utils :as gu]
   [thi.ng.geom.vector :as v :refer [vec2 vec3]]
   [thi.ng.geom.matrix :refer [M44]]
   [thi.ng.math.core :as m]
   #?(:clj [thi.ng.math.macros :as mm])))

;; This namespace provides vertex attribute generators and utilities used
;; by various other geom namespaces to simplify the creation of
;; attributed meshes.
;;
;; This project uses a functional approach for vertex attribute
;; generation and relies on attribute generator fns accepting these 4
;; arguments, in order:
;;
;; | *Arg*       | *Type*    | *Description*                                            |
;; |-------------+-----------+----------------------------------------------------------|
;; | `face-id`   | int       | ID of face currently being generated                     |
;; | `vertex-id` | int       | ID of vertex in current face (usually 0-3 or 0-4)        |
;; | `vertex`    | vec2/vec3 | the actual vertex                                        |
;; | `opts`      | map       | extra data supplied by type to help w/ attrib generation |
;;
;; ** Types supporting attribute generation
;;
;; Eventually, all shape types provided will support vertex attribute
;; generation as part of their `g/as-mesh` implementations. At current,
;; this is only supported by the following implementations, though. Since
;; UV texture coordinates are one of the most common applications of
;; vertex attributes, the table also lists which attribute generator
;; should/could be used for each type.
;;
;; *Note:* Vertex attributes are only supported by the following mesh types:
;;
;; - `thi.ng.geom.IndexedMesh`
;; - `thi.ng.geom.gl.GLMesh`
;; - `thi.ng.geom.gl.IndexedGLMesh`
;;
;; | *Type*          | *UV generation examples*                                              |
;; |-----------------+-----------------------------------------------------------------------|
;; | AABB            | `(g/as-mesh a {:mesh ... :attribs {:uv (attr/uv-cube-map-v)}})`       |
;; | Circle          | `(g/as-mesh a {:mesh ... :attribs {:uv attr/uv-flat-disc}})`          |
;; | Cuboid          | `(g/as-mesh a {:mesh ... :attribs {:uv (attr/uv-cube-map-v)}})`       |
;; | Polygon2        | `(g/as-mesh a {:mesh ... :attribs {:uv (attr/supplied-attrib :uv)}})` |
;; | Polyhedron mesh | `(polyhedron-mesh f {:mesh ... :attribs {:uv ...}})`                  |
;; | PTF             | `(g/as-mesh a {:mesh ... :attribs {:uv attr/uv-tube}})`               |
;; | Rect2           | `(g/as-mesh a {:mesh ... :attribs {:uv (attr/supplied-attrib :uv)}})` |
;; | Sphere          | `(g/as-mesh a {:mesh ... :attribs {:uv (attr/supplied-attrib :uv)}})` |
;;
;; ** Attribute generation & application

(defn face-attribs
  "Vertex attribute generator using given seq of attribs. The seq
  should contain at least the same number of elements as there are
  faces to be generated. Each item itself is a vector of attrib
  values (in vertex order) to be assigned to each vertex. Returns
  generator fn."
  [fattribs] (fn [fid vid _ _] (-> fattribs (nth fid) (nth vid))))

(defn const-face-attribs
  "Similar to face-attribs fn, but for attributes which are constant
  for all vertices of a single face. Takes a seq of attrib values and
  returns generator fn."
  [fattribs] (fn [fid _ _ _] (nth fattribs fid)))

(defn generate-face-attribs
  "Takes a vector of face vertices, face id, a map of vertex attribute
  generator fns and an options arg passed to the attribute generator
  fns. Returns 2-elem vector of [verts vert-attribs]. The generator
  fns themselves take these 4 args and should return the attribute for
  a single vertex: face-id, vertex-id, vertex, opts (a map)."
  [verts face-id attribs opts]
  [verts
   (when (seq attribs)
     (reduce-kv
      (fn [acc k f] (assoc acc k (map-indexed #(f face-id % %2 opts) verts)))
      {} attribs))])

;; ** Attribute generation
;; *** Generic
;;
;; Several types provide useful additional data to attribute generator
;; functions, which sometimes even is directly usable as attribute
;; values. E.g. the sphere type's =as-mesh= implementation internally
;; requires UV coordinates, which then are also supplied to attribute
;; generators and can directly be harnessed for the actual vertex
;; attributes.
;;
;; In general, if available, this kind of additional data is provided via
;; a map as the 4th argument to an attribute generator fn. The following
;; generic helper function can be used to select such a pre-computed
;; attribute value (and optionally transform it).

(defn supplied-attrib
  "Higher order helper, returns attribute generator fn for types which
  emit pre-computed values as part of their `as-mesh` impl. Takes
  attrib key and for each vertex looks up value in opts map (which is
  supplied by supporting types, e.g. sphere). If called via 2 args,
  the looked up values will also be passed to transformation fn (2nd
  arg)."
  ([attrib] (fn [_ id _ opts] (-> opts (get attrib) (nth id))))
  ([attrib tx] (fn [_ id _ opts] (-> opts (get attrib) (nth id) tx))))

;; *** UV texture coordinates
;; **** Cube

(defn uv-rect-for-size
  ([w] (uv-rect-for-size w w))
  ([w h] (uv-rect-for-size w w 0.0 0.0 1.0 1.0))
  ([w h x y uw vh]
   (let [u  (* 0.5 (/ uw w))
         v  (* 0.5 (/ vh h))
         iu (- uw u)
         iv (- vh v)]
     (mapv #(m/+ % x y) [(vec2 u v) (vec2 iu v) (vec2 iu iv) (vec2 u iv)]))))

(defn uv-cube-map-h
  ([h] (uv-cube-map-h h false))
  ([h pow2?]
   (let [w  (* h 6)
         tw (if pow2? (m/ceil-pow2 w) w)
         fw (/ (/ w tw) 6.0)]
     (mapv #(uv-rect-for-size h h (* % fw) 0.0 fw 1.0) (range 6)))))

(defn uv-cube-map-v
  ([h] (uv-cube-map-v h false))
  ([w pow2?]
   (let [h  (* w 6)
         th (if pow2? (m/ceil-pow2 h) h)
         fh (/ (/ h th) 6.0)]
     (mapv #(uv-rect-for-size w w 0.0 (* % fh) 1.0 fh) (range 6)))))

(def uv-default-rect [(vec2) (vec2 1.0 0.0) (vec2 1.0) (vec2 0.0 1.0)])

(def uv-faces (face-attribs (repeat uv-default-rect)))

;; **** Tube
;;
;; Tube UV mapping is currently only supported by the following implementations:
;;
;; - PTF `sweep-mesh`

(defn uv-tube
  "Generates tubular UV coordinates, using data provided in
  options map (:u :v :du :dv keys)"
  [_ vid _ {:keys [u v du dv]}]
  (case (int vid)
    0 (v/vec2 u v)
    1 (v/vec2 (+ u du) v)
    2 (v/vec2 (+ u du) (+ v dv))
    (v/vec2 u (+ v dv))))

;; **** Disc

(defn uv-flat-disc
  "Generates UV coordinates for a circle/trianglefan, using data
  provided in options map (:theta & :r keys). The first vertex of each
  triangle is assumed to be in the center of the circle, the other 2
  vertices are located at the circles perimeter."
  [_ vid _ {:keys [theta r] :as opts}]
  (case (int vid)
    0 (vec2 0.5)
    1 (vec2 (mm/madd (Math/cos theta) r 0.5)
            (mm/madd (Math/sin theta) r 0.5))
    (let [theta (+ theta (get opts :delta))]
      (vec2 (mm/madd (Math/cos theta) r 0.5)
            (mm/madd (Math/sin theta) r 0.5)))))

(defn uv-polygon-disc
  "HOF UV generator for polygons/polyhedras. Takes polygon resolution
  and computes N UV coords, returns generator fn"
  [res]
  (->> res
       m/norm-range
       (mapv #(m/+ (g/as-cartesian (vec2 0.5 (* % m/TWO_PI))) 0.5))
       repeat
       face-attribs))
