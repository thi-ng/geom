(ns thi.ng.geom.viz.core
  (:require
   [thi.ng.geom.core :as g]
   [thi.ng.geom.vector :as v :refer [vec2 vec3]]
   [thi.ng.geom.utils :as gu]
   [thi.ng.geom.svg.core :as svg]
   [thi.ng.ndarray.core :as nd]
   [thi.ng.ndarray.contours :as contours]
   [thi.ng.math.core :as m]
   [thi.ng.strf.core :as f]))

;; This module currently consists of this single namespace dedicated
;; to creating 2D (and soon 3D too) data visualizations in an output
;; format agnostic way. To achieve that, an overall declarative and
;; pipelined approach has been taken to create these visualizations:
;;
;; 1. We first define a configuration map, supplying all data points,
;;    axis definitions, layout arguments/handlers, styling options etc.
;; 2. This map is then transformed into a tree/scenegraph of geometric
;;    primitives (shapes & groups) representing the visualization.
;; 3. This tree of still pure Clojure data is then further converted into
;;    the final output format, e.g. for SVG first into
;;    https://github.com/weavejester/hiccup and then actual SVG/XML...
;;
;; The declarative nature has several benefits:
;;
;; - place multiple data series w/ potentially different layout methods
;;   into same visualization
;; - create template/preset specs (e.g. pre-styled, only inject new data
;;   points)
;; - easy to integrate in central state atom pattern, compatible w/
;;   Om/Reagent based setups
;; - support multiple output targets from the same visualization spec
;;
;; Apart from SVG (the only target supported at the moment), this
;; module also aims to support OpenGL / WebGL scenegraph generation
;; and http://thi.ng/luxor scene exports for rendering visualizations
;; with http://luxrender.net.
;;
;; Visualization spec format
;;
;; The main configuration map should have at least the following keys,
;; common to all supported visualization methods. Visualizations are
;; created by taking a series of data points and mapping them from
;; their source `:domain` into new coordinate system (`:range`).
;; Furthermore, this target coordinate system itself can be
;; transformed via projections, e.g. to translate from cartesian into
;; polar coordinates (e.g. see Radar plot and other polar examples
;; above).
;;
;; | *Key*     | *Value*                  | *Required* | *Description*                             |
;; |-----------+--------------------------+------------+-------------------------------------------|
;; | `:x-axis` | horizontal axis spec map | Y          | X-axis behavior & representation details  |
;; | `:y-axis` | vertical axis spec map   | Y          | Y-axis behavior & representation details  |
;; | `:grid`   | grid spec map            | N          | Optional background axis grid             |
;; | `:data`   | vector of dataset specs  | Y          | Allows multiple datasets in visualization |
;;
;; The following options are only used for visualizations using
;; `svg-plot2d-polar`:
;;
;; | *Key*     | *Value*                 | *Required* | *Description*                                                                        |
;; |-----------+-------------------------+------------+--------------------------------------------------------------------------------------|
;; | `:origin` | 2D vector, e.g. =[x y]= | Y          | Center pos of radial layout, only required for polar projection                      |
;; | `:circle` | boolean                 | N          | true if axis & grid should be using full circles, only required for polar projection |
;;
;; Axis definitions (:x-axis / :y-axis)
;;
;; Axis specs are usually created via one of the available axis
;; generator functions (`linear-axis`, `log-axis`, `lens-axis`). These
;; functions too take a map w/ some of the same keys, but replace some
;; vals with transformed data and autofill default values for others.
;;
;; | *Key*          | *Value*              | *Required* | *Default*                              | *Description*                                                            |
;; |----------------+----------------------+------------+----------------------------------------+--------------------------------------------------------------------------|
;; | `:scale`       | scale function       | Y          | nil                                    | Scale function to translate domain values into visualization coordinates |
;; | `:domain`      | vec of domain bounds | Y          | nil                                    | Lower & upper bound of data source interval                              |
;; | `:range`       | vec of range bounds  | Y          | nil                                    | Lower & upper bound of projected coordinates                             |
;; | `:pos`         | number               | Y          | nil                                    | Draw position of the axis (ypos for X-axis, xpos for Y-axis)             |
;; | `:major`       | seq of domain values | N          | nil                                    | Seq of domain positions at which to draw labeled tick marks              |
;; | `:minor`       | seq of domain values | N          | nil                                    | Seq of domain positions at which                                         |
;; | `:major-size`  | number               | N          | 10                                     | Length of major tick marks                                               |
;; | `:minor-size`  | number               | N          | 5                                      | Length of minor tick marks                                               |
;; | `:label`       | function             | N          | =(default-svg-label (value-format 2))= | Function to format & emit tick labels                                    |
;; | `:label-dist`  | number               | N          | 10 + major-size                        | Distance of value labels from axis                                       |
;; | `:label-style` | map                  | N          | see next section                       | Style attribute map for value labels                                     |
;; | `:label-y`     | number               | N          | 0                                      | Vertical offset for Y-axis labels                                        |
;; | `:attribs`     | map                  | N          | ={:stroke "black"}=                    | Axis line attribs attributes                                             |
;; | `:visible`     | boolean              | N          | true                                   | Flag if axis will be visible in visualization                            |
;;
;; About tick marks
;;
;; The `linear-axis` & `lens-axis` interpret the given `:major` and
;; `:minor` values as the intended step distance between ticks and
;; generate ticks at multiples of the given value.
;;
;; The `log-axis` generator auto-creates ticks based on the `:base` of
;; the logarithm.
;;
;; Notes for polar projection
;;
;; - the `:range` interval of the x-axis must be an angle interval in radians (see above example)
;; - the `:range` interval of the y-axis must be a radius interval
;;
;; Same goes for `:pos` values: The `:pos` for x-axis is a radius, the
;; `:pos` for y-axis is an angle in radians
;;
;; Default axis label styling
;;
;; For SVG export, each axis is exported as its own group (incl. tick
;; marks & labels). By default the following label style is applied to
;; each group, however this can be overridden for individual labels by
;; specifying a custom `:label` function.
;;
;; ```
;; {:fill        "black"
;;  :stroke      "none"
;;  :font-family "Arial, sans-serif"
;;  :font-size   10
;;  :text-anchor "middle"}
;; ```
;;
;; *** Axis grid definition (:grid)
;;
;; *Note:* If no `:grid` spec is given in the main spec, no background
;; *grid will be displayed...
;;
;; | *Key*      | *Value* | *Required* | *Default*       | *Description*                                            |
;; |------------+---------+------------+-----------------+----------------------------------------------------------|
;; | `:attribs` | hashmap | N          | default attribs | allows extra attributes to be injected (e.g. for SVG)    |
;; | `:minor-x` | boolean | N          | false           | if `false` only uses major tick mark positions on X axis |
;; | `:minor-y` | boolean | N          | false           | if `false` only uses major tick mark positions on Y axis |
;;
;; Dataset specs (:data)
;;
;; The format of these maps is largely dependent on the concrete
;; visualization methods used, but most have the following keys in
;; common:
;;
;; | *Key*       | *Required* | *Default*  | *Description*                                                  |
;; |-------------+------------+------------+----------------------------------------------------------------|
;; | `:layout`   | Y          | nil        | Layout function to map data points                             |
;; | `:values`   | Y          | nil        | Data points to be mapped                                       |
;; | `:attribs`  | N          | nil        | Styling & other attributes to attach to surrounding group node |
;; | `:item-pos` | N          | `identity` | Function returning domain position of single data point        |
;;
;; The `:item-pos` key is only needed if the data items are in a
;; non-standard format. By the default most layout functions expect
;; the data points to be a 2-element vector `[domain-pos value]`.
;; Using an `:item-pos` lookup fn however, data items can be supplied
;; in other form (eg. as maps). Also see examples above...
;;
;; Scales
;;
;; Scaling functions merely provide a means to map values from a source
;; interval (domain) to a target interval (range). The latter usually
;; represents values in the visualization space (e.g. partial screen
;; coordinates).
;;
;; These functions can be useful also outside a visualization
;; context, but here are used in conjunction with their related axis
;; definition functions described below. When creating visualizations,
;; we would not usually use these scaling functions directly, but use
;; them implicitly via our defined axis specs.

;; Value transformations

(defn value-mapper
  [scale-x scale-y] (fn [[x y]] [(scale-x x) (scale-y y)]))

(defn value-transducer
  [{:keys [cull-domain cull-range scale-x scale-y project shape item-pos]}]
  (let [mapper   (value-mapper scale-x scale-y)
        item-pos (or item-pos identity)]
    (cond->       (map (juxt item-pos identity))
      cull-domain (comp (filter #(m/in-range? cull-domain (ffirst %))))
      :always     (comp (map (fn [[p i]] [(mapper p) i])))
      cull-range  (comp (filter #(m/in-range? cull-range (peek (first %)))))
      project     (comp (map (fn [[p i]] [(project p) i])))
      shape       (comp (map shape)))))

(defn process-points
  [{:keys [x-axis y-axis project]} {:keys [values item-pos shape]}]
  (let [[ry1 ry2] (:range y-axis)]
    (->> (if item-pos
           (sort-by (comp first item-pos) values)
           (sort-by first values))
         (sequence
          (value-transducer
           {:cull-domain (:domain x-axis)
            :cull-range  (if (< ry1 ry2) [ry1 ry2] [ry2 ry1])
            :item-pos    item-pos
            :scale-x     (:scale x-axis)
            :scale-y     (:scale y-axis)
            :project     project
            :shape       shape})))))

(defn points->path-segments
  [[p & more]]
  (->> more
       (reduce #(conj! % [:L %2]) (transient [[:M p]]))
       persistent!))

;; Projections

(defn polar-projection
  [origin]
  (let [o (vec2 origin)]
    (fn [[x y]] (m/+ o (g/as-cartesian (vec2 y x))))))

;; Value formatting

(defn value-formatter
  [prec]
  (let [fmt [(f/float prec)]]
    (fn [x] (f/format fmt x))))

(defn format-percent
  [x] (str (int (* x 100)) "%"))

(defn default-svg-label
  [f] (fn [p x] (svg/text p (f x))))

;; Domain analysis ops
;;
;; Raw values to domain point conversion
;;
;; Most of the visualization methods in this module expect a seq of
;; data points in the format: `[domain-position value]`. The function
;; `uniform-domain-points` is useful to convert a sequence of pure
;; values (without position) into a seq of uniformly spaced data
;; points along the full breadth of the given domain:

(defn uniform-domain-points
  "Given a vector of domain bounds and a collection of data values
  (without domain position), produces a lazy-seq of 2-element vectors
  representing the values of the original coll uniformly spread over
  the full domain range, with each of the form: [domain-pos value]."
  [[d1 d2] values]
  (map
   (fn [t v] [(m/mix* d1 d2 t) v])
   (m/norm-range (dec (count values)))
   values))

;; Domain bounds

(def domain-bounds-x #(gu/axis-bounds 0 %))

(def domain-bounds-y #(gu/axis-bounds 1 %))

(def domain-bounds-z #(gu/axis-bounds 2 %))

(defn total-domain-bounds
  [f & colls]
  (transduce
   (map f)
   (completing (fn [[aa ab] [xa xb]] [(min aa xa) (max ab xb)]))
   [m/INF+ m/INF-] colls))

;; Matrix value domain

(defn value-domain-bounds
  [mat]
  (let [vals (seq mat)]
    [(reduce min vals) (reduce max vals)]))

;; Linear scaling

(defn linear-scale
  [domain range]
  (fn [x] (m/map-interval x domain range)))

;; Logorithmic scaling

(defn log
  [base]
  (let [lb (Math/log base)]
    #(/ (cond
          (pos? %) (Math/log %)
          (neg? %) (- (Math/log (- %)))
          :else 0)
        lb)))

(defn log-scale
  [base [d1 d2 :as domain] [r1 r2 :as range]]
  (let [log* (log base)
        d1l  (log* d1)
        dr   (- (log* d2) d1l)]
    (fn [x] (m/mix* r1 r2 (/ (- (log* x) d1l) dr)))))

;; Lens scale (dilating / bundling)
;;
;; The =lens-scale= defines a non-linear mapping by specifying a focal
;; position in the domain interval, as well as a lens strength which
;; controls the compression or expansion of the domain space around
;; this focal point. If strength is positive, the lens is dilating. If
;; negative, it is bundling (compressing). A strength of zero causes a
;; normal/linear scaling behavior.
;;
;; The two animations below show the effect of individually adjusting
;; the focus and lens strength:
;;
;; http://media.thi.ng/geom/viz/lens-focus-2.gif
;; Focus shift, constant strength = 0.5
;;
;; http://media.thi.ng/geom/viz/lens-strength-4.gif
;; Lens strength adjustment, constant focus = 0.0

(defn lens-scale
  [focus strength [d1 d2] [r1 r2]]
  (let [dr (- d2 d1)
        f  (/ (- focus d1) dr)]
    (fn [x] (m/mix-lens r1 r2 (/ (- x d1) dr) f strength))))

;; Axis & tick generators

;; Common axis factory

(defn axis-common*
  [{:keys [visible major-size minor-size label attribs label-style label-dist]
    :or {visible true major-size 10, minor-size 5}
    :as spec}]
  (assoc spec
         :visible     visible
         :major-size  major-size
         :minor-size  minor-size
         :label       (or label (default-svg-label (value-formatter 2)))
         :attribs     (merge
                       {:stroke "black"}
                       attribs)
         :label-style (merge
                       {:fill        "black"
                        :stroke      "none"
                        :font-family "Arial, sans-serif"
                        :font-size   10
                        :text-anchor "middle"}
                       label-style)
         :label-dist  (or label-dist (+ 10 major-size))))

;; Linear

(defn lin-tick-marks
  [[d1 d2] delta]
  (if (m/delta= delta 0.0 m/*eps*)
    '()
    (let [dr (- d2 d1)
          d1' (m/roundto d1 delta)]
      (filter #(m/in-range? d1 d2 %) (range d1' (+ d2 delta) delta)))))

(defn linear-axis
  [{:keys [domain range major minor] :as spec}]
  (let [major' (if major (lin-tick-marks domain major))
        minor' (if minor (lin-tick-marks domain minor))
        minor' (if (and major' minor')
                 (filter (complement (set major')) minor')
                 minor')]
    (-> spec
        (assoc
         :scale (linear-scale domain range)
         :major major'
         :minor minor')
        (axis-common*))))

;; Logarithmic

(defn log-ticks-domain
  [base d1 d2]
  (let [log* (log base)] [(m/floor (log* d1)) (m/ceil (log* d2))]))

(defn log-tick-marks-major
  [base [d1 d2]]
  (let [[d1l d2l] (log-ticks-domain base d1 d2)]
    (->> (for [i (range d1l (inc d2l))]
           (if (>= i 0)
             (* (/ 1 base) (Math/pow base i))
             (* (/ 1 base) (- (Math/pow base (- i))))))
         (filter #(m/in-range? d1 d2 %)))))

(defn log-tick-marks-minor
  [base [d1 d2]]
  (let [[d1l d2l] (log-ticks-domain base d1 d2)
        ticks (if (== 2 base) [0.75] (range 2 base))]
    (->> (for [i (range d1l (inc d2l)) j ticks]
           (if (>= i 0)
             (* (/ j base) (Math/pow base i))
             (* (/ j base) (- (Math/pow base (- i))))))
         (filter #(m/in-range? d1 d2 %)))))

(defn log-axis
  [{:keys [base domain range] :or {base 10} :as spec}]
  (-> spec
      (assoc
       :scale (log-scale base domain range)
       :major (log-tick-marks-major base domain)
       :minor (log-tick-marks-minor base domain))
      (axis-common*)))

;; Lens axis
;;
;; The lens axis is a modified `linear-axis` with two additional
;; required attributes to control the domain space deformation in
;; order to compress or expand the space around a given focal point
;; and therefore introduce a non-linear arrangement. See `lens-scale`
;; above for further details.
;;
;; - `:focus` - the domain value acting as lens focus
;;   (by default the center of the domain is used)
;; - `:strength` - the lens strength & direction
;;   (normalized values -1.0 ... + 1.0, default = 0.5)

(defn lens-axis
  [{:keys [domain range focus strength major minor]
    :or {strength 0.5} :as spec}]
  (let [major' (if major (lin-tick-marks domain major))
        minor' (if minor (lin-tick-marks domain minor))
        minor' (if (and major' minor')
                 (filter (complement (set major')) minor')
                 minor')
        focus  (or focus (/ (apply + domain) 2.0))]
    (-> spec
        (assoc
         :scale    (lens-scale focus strength domain range)
         :major    major'
         :minor    minor'
         :focus    focus
         :strength strength)
        (axis-common*))))

;; Custom shapes

;; Some preset shape functions for use with scatter plots

(defn svg-triangle-up
  [w]
  (let [h (* w (Math/sin m/THIRD_PI))
        w (* 0.5 w)]
    (fn [[[x y]]] (svg/polygon [[(- x w) (+ y h)] [(+ x w) (+ y h)] [x y]]))))

(defn svg-triangle-down
  [w]
  (let [h (* w (Math/sin m/THIRD_PI))
        w (* 0.5 w)]
    (fn [[[x y]]] (svg/polygon [[(- x w) (- y h)] [(+ x w) (- y h)] [x y]]))))

(defn svg-square
  [r] (let [d (* r 2.0)] (fn [[[x y]]] (svg/rect (vec2 (- x r) (- y r)) d d))))

(defn labeled-rect-horizontal
  [{:keys [h r label fill min-width base-line]}]
  (let [r2 (* -2 r)
        h2 (* 0.5 h)]
    (fn [[[ax ay :as a] [bx :as b] item]]
      (svg/group
       {}
       (svg/rect
        (vec2 (- ax r) (- ay h2)) (- bx ax r2) h
        {:fill (fill item) :rx r :ry r})
       (if (< min-width (- bx ax))
         (svg/text (vec2 ax (+ base-line ay)) (label item)))))))

(defn circle-cell
  [a b c d col]
  (svg/circle (gu/centroid [a b c d]) (* 0.5 (g/dist a b)) {:fill col}))

;; Axial visualization methods
;;
;; This section defines the various layout/plotting methods, each with
;; a brief description and lists of custom `:data` spec options. See
;; the "Dataset specs" section and examples above for details about
;; other (required) keys...

;; Line plot
;;
;; This method simply represents the mapped values as a single
;; line-strip (polyline). Values are automatically sorted by domain
;; position, so can be initially unordered.

(defn svg-line-plot
  [v-spec d-spec]
  (svg/line-strip (map first (process-points v-spec d-spec)) (:attribs d-spec)))

;; Area plot
;;
;; Similar to the line plot method above, however resulting points are
;; represented as closed polygon (and hence an `:attribs` key should
;; be supplied w/ a `:fill` color).
;;
;; *Note:* When using polar coordinate mapping (via
;; `svg-plot2d-polar`), a `:res` option should be given too in order
;; to create an arc approximation closing the polygon along the X-axis
;; (e.g. `:res 20`).
;;
;; | *Key*  | *Required* | *Default* | *Description*                                       |
;; |--------+------------+-----------+-----------------------------------------------------|
;; | `:res` | N          |         1 | Number of points used to close polygon along X-axis |

(defn svg-area-plot
  [{:keys [y-axis project] :as v-spec} {:keys [res] :as d-spec}]
  (let [ry1     (first (:range y-axis))
        points  (mapv first (process-points (assoc v-spec :project vec2) d-spec))
        p       (vec2 (first (peek points)) ry1)
        q       (vec2 (ffirst points) ry1)
        points  (concat points (mapv (partial m/mix p q) (m/norm-range (or res 1))))]
    (svg/polygon (mapv project points) (:attribs d-spec))))

;; Radar plot
;;
;; This plot method is intended to be only used with `svg-plot2d-polar`.
;;
;; | *Key*    | *Required* | *Default*   | *Description*                                        |
;; |----------+------------+-------------+------------------------------------------------------|
;; | `:shape` | N          | svg/polygon | Shape function receiving seq of all points & attribs |

(defn svg-radar-plot
  [v-spec {:keys [shape] :or {shape svg/polygon} :as d-spec}]
  (shape (mapv first (process-points v-spec d-spec)) (:attribs d-spec)))

;; Min-Max radar plot
;;
;; This version of the radar plot expects a min/max interval for each
;; data item. For example a single data point of `[2 0.25 0.75]` would
;; define a domain position at x=2 and an interval of 0.25-0.75. If no
;; `:item-pos-*` options are supplied this 3-element vector format is
;; assumed for each data point.
;;
;; | *Key*           | *Required* | *Default* | *Description*                                                    |
;; |-----------------+------------+-----------+------------------------------------------------------------------|
;; | `:item-pos-min` | N          | `[x min]` | Function to provide min. data point                              |
;; | `:item-pos-max` | N          | `[x max]` | Function to provide max. data point                              |
;; | `:shape`        | N          | svg/path  | Shape function receiving seq of outer & inner points and attribs |

(defn svg-radar-plot-minmax
  [v-spec
   {:keys [item-pos-min item-pos-max shape]
    :or   {shape #(svg/path (concat % [[:Z]] %2 [[:Z]]) %3)}
    :as   d-spec}]
  (let [min-points (->> (assoc d-spec :item-pos (or item-pos-min (fn [i] (take 2 i))))
                        (process-points v-spec)
                        (mapv first)
                        (points->path-segments))
        max-points (->> (assoc d-spec :item-pos (or item-pos-max (fn [i] [(first i) (nth i 2)])))
                        (process-points v-spec)
                        (mapv first)
                        (points->path-segments))]
    (shape max-points min-points (assoc (:attribs d-spec) :fill-rule "evenodd"))))

;; Scatter plot
;;
;; | *Key*    | *Required* | *Default* | *Description*                                          |
;; |----------+------------+-----------+--------------------------------------------------------|
;; | `:shape` | N          | `circle`  | Function returning shape primitive for each data point |

(defn svg-scatter-plot
  [v-spec {:keys [attribs shape] :as d-spec}]
  (->> (assoc d-spec :shape (or shape (fn [[p]] (svg/circle p 3))))
       (process-points v-spec)
       (apply svg/group attribs)))

;; Bar plot
;;
;; | *Key*         | *Required* | *Default*  | *Description*                                           |
;; |---------------+------------+------------+---------------------------------------------------------|
;; | `:item-pos`   | N          | `identity` | Function returning domain position of single data point |
;; | `:shape`      | N          | `line`     | Function returning shape primitive for each data point  |
;; | `:interleave` | N          | 1          | Number of bars per domain position                      |
;; | `:offset`     | N          | 0          | Only used for interleaved bars, index position          |
;; | `:bar-width`  | N          | 0          | Only used for interleaved bars, width of single bar     |
;; | `:shape`      | Y          | svg/line   | Function returning shape primitive for each data point  |

(defn svg-bar-plot
  [{:keys [x-axis y-axis project] :or {project vec2}}
   {:keys [values attribs shape item-pos interleave offset bar-width]
    :or   {shape      (fn [a b _] (svg/line a b))
           item-pos   identity
           interleave 1
           bar-width  0
           offset     0}}]
  (let [domain  (:domain x-axis)
        base-y  ((:scale y-axis) (first (:domain y-axis)))
        mapper  (value-mapper (:scale x-axis) (:scale y-axis))
        offset  (+ (* -0.5 (* interleave bar-width)) (* (+ offset 0.5) bar-width))]
    (->> values
         (sequence
          (comp
           (map (juxt item-pos identity))
           (filter #(m/in-range? domain (ffirst %)))
           (map
            (fn [[p i]]
              (let [[ax ay] (mapper p)
                    ax (+ ax offset)]
                (shape (project [ax ay]) (project [ax base-y]) i))))))
         (apply svg/group attribs))))

;; Heatmap
;;
;; | *Key*            | *Required* | *Default*    | *Description*                                      |
;; |------------------+------------+--------------+----------------------------------------------------|
;; | `:matrix`        | Y          | nil          | NDArray instance of data grid                      |
;; | `:palette`       | Y          | nil          | Color list                                         |
;; | `:palette-scale` | N          | linear-scale | Mapping function of matrix values to palette index |
;; | `:value-domain`  | N          | [0 1]        | Domain interval of matrix values                   |
;; | `:clamp`         | N          | false        | If true, matrix values are clamped to value domain |
;;
;; *Note:* If `:clamp` is not enabled, the `:value-domain` acts as
;; filter and will not include cells with values outside the domain,
;; resulting in holes in the visualization. On the other hand, if
;; `:clamp` is enabled, the `:value-domain` acts as a kind of
;; amplification or compression function.

(defn svg-heatmap
  [{:keys [x-axis y-axis project]}
   {:keys [matrix value-domain clamp palette palette-scale attribs shape]
    :or {value-domain  [0.0 1.0]
         palette-scale linear-scale
         shape         #(svg/polygon [%1 %2 %3 %4] {:fill %5})}
    :as d-spec}]
  (let [scale-x (:scale x-axis)
        scale-y (:scale y-axis)
        pmax    (dec (count palette))
        scale-v (palette-scale value-domain [0 pmax])]
    (apply svg/group
           attribs
           (for [p     (nd/position-seq matrix)
                 :let  [[y x] p
                        v     (nd/get-at matrix y x)]
                 :when (or clamp (m/in-range? value-domain v))]
             (shape
              (project [(scale-x x) (scale-y y)])
              (project [(scale-x (inc x)) (scale-y y)])
              (project [(scale-x (inc x)) (scale-y (inc y))])
              (project [(scale-x x) (scale-y (inc y))])
              (palette (m/clamp (int (scale-v v)) 0 pmax)))))))

;; Contour lines
;;
;; Given a 2D matrix (a `thi.ng.ndarray.core.NDArray` instance) of data
;; values and a seq of thresholds, this function computes number of
;; polygons for each threshold level.
;;
;; *Note:* Since the data format for this method is different to the
;; other layouts, we're using the `:matrix` key instead of `:values` to
;; emphasize this difference...
;;
;; | *Key*              | *Required* | *Default*    | *Description*                                                  |
;; |--------------------+------------+--------------+----------------------------------------------------------------|
;; | `:matrix`          | Y          | nil          | NDArray instance of data grid                                  |
;; | `:levels`          | Y          | nil          | Seq of threshold values to find contours for                   |
;; | `:palette`         | Y          | nil          | Color list                                                     |
;; | `:palette-scale`   | N          | linear-scale | Mapping function of matrix values to palette index             |
;; | `:value-domain`    | N          | [0 1]        | Domain interval of matrix values                               |
;; | `:contour-attribs` | N          | nil          | Function to produce shape attribs map for each threshold level |

(defn matrix-2d
  [w h values] (nd/ndarray :float32 values [h w]))

(defn contour-matrix
  [w h values]
  (contours/set-border-2d (matrix-2d w h values) -1e9))

(defn contour->svg
  [scale-x scale-y project]
  (fn [attribs contour]
    (let [contour (map (fn [[y x]] [(scale-x x) (scale-y y)]) contour)]
      (svg/polygon (map project contour) attribs))))

(defn svg-contour-plot
  [{:keys [x-axis y-axis project]}
   {:keys [matrix attribs levels palette palette-scale value-domain contour-attribs]
    :or   {value-domain    [0.0 1.0]
           palette         [[1 1 1]]
           palette-scale   linear-scale
           contour-attribs (constantly nil)}}]
  (let [pmax       (dec (count palette))
        scale-v    (palette-scale value-domain [0 pmax])
        contour-fn (contour->svg (:scale x-axis) (:scale y-axis) project)]
    (->> levels
         (sort)
         (mapv
          (fn [iso]
            (let [c-attribs (contour-attribs (palette (m/clamp (int (scale-v iso)) 0 pmax)))]
              (apply svg/group
                     {} (mapv
                         (partial contour-fn c-attribs)
                         (contours/find-contours-2d matrix iso))))))
         (apply svg/group attribs))))

;; Stacked intervals
;;
;; | *Key*         | *Required* | *Default*  | *Description*                                         |
;; |---------------+------------+------------+-------------------------------------------------------|
;; | `:item-range` | N          | `identity` | Function returning domain interval for each data item |
;; | `:offset`     | N          | 0          | Y-axis offset for this data series                    |
;; | `:shape`      | N          | `svg/line` | Function returning shape primitive for each data item |

(defn overlap? [[a b] [c d]] (and (<= a d) (>= b c)))

(defn compute-row-stacking
  [item-range coll]
  (reduce
   (fn [grid x]
     (let [r (item-range x)]
       (loop [[row & more] grid idx 0]
         (if (or (nil? row) (not (some #(overlap? r (item-range %)) row)))
           (update-in grid [idx] #(conj (or % []) x))
           (recur more (inc idx))))))
   [] coll))

(defn process-interval-row
  [item-range mapper [d1 d2]]
  (fn [i row]
    (map
     (fn [item]
       (let [[a b] (item-range item)]
         [(mapper [(max d1 a) i]) (mapper [(min d2 b) i]) item]))
     row)))

(defn svg-stacked-interval-plot
  [{:keys [x-axis y-axis]}
   {:keys [values attribs shape item-range offset]
    :or   {shape (fn [[a b]] (svg/line (vec2 a) (vec2 b)))
           item-range identity
           offset 0}}]
  (let [scale-x (:scale x-axis)
        scale-y (:scale y-axis)
        domain  (:domain x-axis)
        mapper  (value-mapper scale-x scale-y)]
    (->> values
         (filter #(overlap? domain (item-range %)))
         (sort-by (comp first item-range))
         (compute-row-stacking item-range)
         (mapcat (process-interval-row item-range mapper domain) (range offset 1e6))
         (mapv shape)
         (apply svg/group attribs))))

;; 2D Cartesian Plotting (SVG)

;; SVG axis generators

(defn svg-axis*
  [{:keys [major minor attribs label-style]} axis tick1-fn tick2-fn label-fn]
  (svg/group
   (merge {:stroke "#000"} attribs)
   (seq (map tick1-fn major))
   (seq (map tick2-fn minor))
   (apply svg/group (merge {:stroke "none"} label-style) (mapv label-fn major))
   axis))

(defn svg-x-axis-cartesian
  [{:keys [scale major-size minor-size label-dist pos label] [r1 r2] :range
    :as spec}]
  (let [y-major (+ pos major-size)
        y-minor (+ pos minor-size)
        y-label (+ pos label-dist)]
    (svg-axis*
     spec (svg/line (vec2 r1 pos) (vec2 r2 pos))
     #(let [x (scale %)] (svg/line (vec2 x pos) (vec2 x y-major)))
     #(let [x (scale %)] (svg/line (vec2 x pos) (vec2 x y-minor)))
     #(label (vec2 (scale %) y-label) %))))

(defn svg-y-axis-cartesian
  [{:keys [scale major-size minor-size label-dist label-y pos label] [r1 r2] :range
    :or {label-y 0}
    :as spec}]
  (let [x-major (- pos major-size)
        x-minor (- pos minor-size)
        x-label (- pos label-dist)]
    (svg-axis*
     spec (svg/line (vec2 pos r1) (vec2 pos r2))
     #(let [y (scale %)] (svg/line (vec2 pos y) (vec2 x-major y)))
     #(let [y (scale %)] (svg/line (vec2 pos y) (vec2 x-minor y)))
     #(label (vec2 x-label (+ (scale %) label-y)) %))))

;; Generic plotting helpers

(defn select-ticks
  [axis minor?] (if minor? (concat (:minor axis) (:major axis)) (:major axis)))

(defn svg-axis-grid2d-cartesian
  [x-axis y-axis {:keys [attribs minor-x minor-y]}]
  (let [[x1 x2] (:range x-axis)
        [y1 y2] (:range y-axis)
        scale-x (:scale x-axis)
        scale-y (:scale y-axis)]
    (svg/group
     (merge {:stroke "#ccc" :stroke-dasharray "1 1"} attribs)
     (if (:visible x-axis)
       (map #(let [x (scale-x %)] (svg/line (vec2 x y1) (vec2 x y2))) (select-ticks x-axis minor-x)))
     (if (:visible y-axis)
       (map #(let [y (scale-y %)] (svg/line (vec2 x1 y) (vec2 x2 y))) (select-ticks y-axis minor-y))))))

(defn svg-plot2d-cartesian
  [{:keys [x-axis y-axis grid data] :as opts}]
  (let [opts (assoc opts :project vec2)]
    (svg/group
     {}
     (if grid (svg-axis-grid2d-cartesian x-axis y-axis grid))
     (map (fn [spec] ((:layout spec) opts spec)) data)
     (if (:visible x-axis) (svg-x-axis-cartesian x-axis))
     (if (:visible y-axis) (svg-y-axis-cartesian y-axis)))))

;; 2D Polar Plotting (SVG)

;; SVG axis generators

(defn svg-x-axis-polar
  [{:keys [x-axis project circle origin]}]
  (let [{:keys [scale major-size minor-size label-dist pos]} x-axis
        label   (or (:label x-axis) (default-svg-label (value-formatter 2)))
        [r1 r2] (:range x-axis)
        o       origin]
    (svg-axis*
     x-axis
     (if circle
       (svg/circle o pos {:fill "none"})
       (svg/arc o pos r1 r2 (> (m/abs-diff r1 r2) m/PI) true {:fill "none"}))
     #(let [x (scale %)]
        (svg/line (project [x pos]) (project [x (+ pos major-size)])))
     #(let [x (scale %)]
        (svg/line (project [x pos]) (project [x (+ pos minor-size)])))
     #(let [x (scale %)]
        (label (project [x (+ pos label-dist)]) %)))))

(defn svg-y-axis-polar
  [{:keys [y-axis project]}]
  (let [{:keys [scale label-y pos] :or {label-y 0}} y-axis
        label   (or (:label y-axis) (default-svg-label (value-formatter 2)))
        [r1 r2] (:range y-axis)
        a       (project [pos r1])
        b       (project [pos r2])
        nl      (m/normalize (g/normal (m/- a b)) (:label-dist y-axis))
        n1      (m/normalize nl (:major-size y-axis))
        n2      (m/normalize nl (:minor-size y-axis))]
    (svg-axis*
     y-axis
     (svg/line a b)
     #(let [p (project [pos (scale %)])]
        (svg/line p (m/+ p n1)))
     #(let [p (project [pos (scale %)])]
        (svg/line p (m/+ p n2)))
     #(let [p (project [pos (+ (scale %) label-y)])]
        (label (m/+ p nl) %)))))

(defn svg-axis-grid2d-polar
  [{:keys [x-axis y-axis origin circle project] {:keys [attribs minor-x minor-y]} :grid}]
  (let [[x1 x2] (:range x-axis)
        [y1 y2] (:range y-axis)
        scale-x (:scale x-axis)
        scale-y (:scale y-axis)
        great?  (> (m/abs-diff x1 x2) m/PI)]
    (svg/group
     (merge {:stroke "#ccc" :stroke-dasharray "1 1"} attribs)
     (if (:visible x-axis)
       (map
        #(let [x (scale-x %)] (svg/line (project [x y1]) (project [x y2])))
        (select-ticks x-axis minor-x)))
     (if (:visible y-axis)
       (map
        #(let [y (scale-y %)]
           (if circle
             (svg/circle origin y {:fill "none"})
             (svg/arc origin y x1 x2 great? true {:fill "none"})))
        (select-ticks y-axis minor-y))))))

(defn svg-plot2d-polar
  [{:keys [x-axis y-axis grid data origin] :as spec}]
  (let [spec (assoc spec :project (polar-projection origin))]
    (svg/group
     {}
     (if grid (svg-axis-grid2d-polar spec))
     (map (fn [spec'] ((:layout spec') spec spec')) data)
     (if (:visible x-axis) (svg-x-axis-polar spec))
     (if (:visible y-axis) (svg-y-axis-polar spec)))))

;; (currently unused) date & time helpers

(comment

  (defn clear-day-of-week [^GregorianCalendar cal] (.set cal Calendar/DAY_OF_WEEK 1) (clear-hour cal))
  (defn clear-month [^GregorianCalendar cal] (.set cal Calendar/MONTH 0) (clear-day-of-month cal))

  (defn year [^GregorianCalendar cal] (.get cal Calendar/YEAR))
  (defn month [^GregorianCalendar cal] (.get cal Calendar/MONTH))
  (defn day-of-month [^GregorianCalendar cal] (.get cal Calendar/DAY_OF_MONTH))
  (defn day-of-week [^GregorianCalendar cal] (.get cal Calendar/DAY_OF_WEEK))
  (defn hour [^GregorianCalendar cal] (.get cal Calendar/HOUR))
  (defn minute [^GregorianCalendar cal] (.get cal Calendar/MINUTE))
  (defn second [^GregorianCalendar cal] (.get cal Calendar/SECOND))

  (defn round-to-year
    [epoch]
    (let [cal (epoch->cal epoch)]
      (doto cal
        (.add Calendar/MONTH 6)
        (clear-month))))

  (defn round-to-month
    [epoch]
    (doto (epoch->cal epoch)
      (.setTimeInMillis (long epoch))
      (.add Calendar/DAY_OF_MONTH 16)
      (clear-day-of-month)))

  (defn round-to-week
    [epoch]
    (doto (epoch->cal epoch)
      (.setTimeInMillis (long epoch))
      (.add Calendar/DAY_OF_WEEK 4)
      (clear-day-of-week)))

  (defn round-to-day-of-month
    [epoch]
    (doto (epoch->cal epoch)
      (.add Calendar/HOUR 12)
      (clear-hour)))

  (defn round-to-day-of-week
    [epoch]
    (doto (epoch->cal epoch)
      (.add Calendar/HOUR 12)
      (clear-hour)))

  (defn round-to-hour
    [epoch]
    (doto (epoch->cal epoch)
      (.add Calendar/MINUTE 30)
      (clear-minute)))

  )
