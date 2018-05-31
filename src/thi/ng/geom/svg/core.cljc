(ns thi.ng.geom.svg.core
  (:require
   [clojure.string :refer [starts-with?]]
   [thi.ng.math.core :as m]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.utils :as gu]
   [thi.ng.geom.vector :as v :refer [vec2]]
   [thi.ng.geom.matrix :as mat :refer [M32]]
   [thi.ng.math.core :as m]
   [thi.ng.strf.core :as f]
   [thi.ng.color.core :as col]
   #?(:clj [hiccup.core :refer [html]])))

;; This namespace provides the core functionality to efficiently create
;; SVG representations of the types defined in the geom library. Please
;; consult the various examples for concrete usage information &
;; potential:
;;
;; - `examples/svg/*.clj` - SVG demos
;; - `examples/viz/*.clj` - SVG visualization/charting examples

;; Constants & presets

(def stroke-round {:stroke-linecap "round" :stroke-linejoin "round"})
(def xml-preamble "<?xml version=\"1.0\"?>\n")

;; Formatters
;;
;; SVG expresses coordinates and various other numeric attributes in text
;; form. The SVG element functions defined in this namespace all make use
;; of the formatters below. By dynamically re-binding the following vars,
;; SVG formatting can be customized as needed.

(def ^:dynamic *ff* (f/float 2))
(def ^:dynamic *fmt-vec* (fn [p] (str (*ff* (first p)) "," (*ff* (nth p 1)))))
(def ^:dynamic *fmt-percent* (fn [x] (str (int (* x 100)) "%")))

(def ^:dynamic *fmt-matrix* ["matrix(" *ff* "," *ff* "," *ff* "," *ff* "," *ff* "," *ff* ")"])

(def point-seq-format2 [*fmt-vec* " " *fmt-vec*])
(def point-seq-format3 [*fmt-vec* " " *fmt-vec* " " *fmt-vec*])
(def point-seq-format4 [*fmt-vec* " " *fmt-vec* " " *fmt-vec* " " *fmt-vec*])

(defn point-seq-format
  [n]
  (case (int n)
    1 [*fmt-vec*]
    2 point-seq-format2
    3 point-seq-format3
    4 point-seq-format4
    (->> *fmt-vec*
         (repeat n)
         (interpose " "))))

;; TODO add missing path segment types (Q, T)

(def path-segment-formats
  {:M ["M" *fmt-vec* " "]
   :m ["m" *fmt-vec* " "]
   :L ["L" *fmt-vec* " "]
   :l ["l" *fmt-vec* " "]
   :C ["C" *fmt-vec* " " *fmt-vec* " " *fmt-vec* " "]
   :c ["c" *fmt-vec* " " *fmt-vec* " " *fmt-vec* " "]
   :A ["A" *fmt-vec* " " *ff* " " str " " str " " *fmt-vec* " "]
   :a ["a" *fmt-vec* " " *ff* " " str " " str " " *fmt-vec* " "]
   :Z ["Z"]
   :z ["z"]})

(defprotocol ISVGConvert
  "This protocol is implemented by types which can directly convert
  themselves into an SVG representation. Note, that currently these
  implementations are only made available when the
  `thi.ng.svg.adapter` namespace is loaded."
  (as-svg [_ opts]))

;; Actual conversion to SVG XML strings is only directly supported for
;; the Clojure version using James Reeve's
;; https://github.com/weavejester/hiccup library. For ClojureScript it
;; is more likely (and efficient) to directly translate a SVG data
;; structure into a DOM tree for which there're many different
;; libraries available and we don't want to be prescriptive here and
;; introduce unnecessary dependencies.

#?(:clj
   (defn serialize
     ^String [svg] (str xml-preamble (html {:mode :xml} svg))))

#?(:clj
   (defn serialize-as-byte-array
     ^bytes [svg] (.getBytes (serialize svg) "UTF-8")))

;; Attribute conversion

;; Colors in SVG need to be defined as CSS color strings and hence are
;; completely opaque to other parts of the code base and which might
;; deal with the dynamic generation of color values. Therefore we
;; allow the `:stroke` and `:fill` attributes to be defined as any
;; color type defined by the http://thi.ng/color library (RGB, HSV,
;; HSL, CMYK, YUV etc.). If these attributes are present and *not*
;; already a string value, they will be converted into a CSS color
;; automatically (using the polymorphic `as-css` protocol method of
;; thi.ng/color).
;;
;; *Note:* The same applies to specifying colors in gradients (see
;; `linear-gradient` and `radial-gradient` functions below).

(defn color-attrib
  [attribs id]
  (if-let [att (get attribs id)]
    (if (string? att)
      attribs
      (assoc attribs id @(col/as-css att)))
    attribs))

;; Transforms
;;
;; Many SVG elements support the attachment of local coordinate
;; transformations. If a `:transform` attribute has a non-string
;; value, it will be interpreted as a `Matrix32` or equivalent
;; 6-element vector and will be automatically converted.

(defn matrix-attrib
  [attribs id]
  (if-let [mat (get attribs id)]
    (if (string? mat)
      attribs
      (let [[a c e b d f] mat]
        (assoc attribs id (apply f/format *fmt-matrix* [a b c d e f]))))
    attribs))

;; Attribute processing

(defn filter-attribs
  [attribs]
  (loop [acc (transient attribs), ks (keys attribs)]
    (if ks
      (recur
       (if (starts-with? (name (first ks)) "__")
         (dissoc! acc (first ks))
         acc)
       (next ks))
      (persistent! acc))))

(defn svg-attribs
  [attribs base]
  (if (seq attribs)
    (-> (filter-attribs attribs)
        (color-attrib :stroke)
        (color-attrib :fill)
        (matrix-attrib :transform)
        (into base))
    base))

;; SVG primitives

(defn svg
  [attribs & body]
  [:svg
   (svg-attribs
    attribs
    {"xmlns" "http://www.w3.org/2000/svg"
     "xmlns:xlink" "http://www.w3.org/1999/xlink"
     "version" "1.1"})
   body])

(defn defs
  [& defs]
  [:defs defs])

(defn gradient-stop
  [[pos col]]
  (let [col (if (string? col) col @(col/as-css col))]
    [:stop {:offset (*fmt-percent* pos) :stop-color col}]))

(defn linear-gradient
  [id attribs & stops]
  [:linearGradient
   (assoc attribs :id id)
   (map gradient-stop stops)])

(defn radial-gradient
  [id attribs & stops]
  [:radialGradient
   (assoc attribs :id id)
   (map gradient-stop stops)])

(defn group
  [attribs & body]
  (into [:g (svg-attribs attribs nil)] body))

(defn path
  ([segments]
   (path segments nil))
  ([segments attribs]
   [:path
    (svg-attribs
     attribs
     {:d (apply f/format
                (mapcat (comp path-segment-formats first) segments)
                (mapcat rest segments))})]))

(defn text
  ([p txt]
   (text p txt nil))
  ([p txt attribs]
   [:text
    (svg-attribs attribs {:x (*ff* (first p)) :y (*ff* (nth p 1))})
    txt]))

(defn circle
  ([p radius]
   (circle p radius nil))
  ([p radius attribs]
   [:circle
    (svg-attribs
     attribs
     {:cx (*ff* (first p)) :cy (*ff* (nth p 1)) :r radius})]))

(defn ellipse
  ([p rx ry]
   (ellipse p rx ry nil))
  ([p rx ry attribs]
   [:ellipse
    (svg-attribs
     attribs
     {:cx (*ff* (first p)) :cy (*ff* (nth p 1)) :rx rx :ry ry})]))

(defn arc-segment
  [center radius theta1 theta2 great? ccw?]
  (let [radius (vec2 radius)
        p (m/+ (vec2 center) (g/as-cartesian (vec2 (first radius) theta1)))
        q (m/+ (vec2 center) (g/as-cartesian (vec2 (nth radius 1) theta2)))]
    [[:M p] [:A radius 0 (if great? 1 0) (if ccw? 1 0) q]]))

(defn arc
  ([center radius theta1 theta2 great? ccw?]
   (arc center radius theta1 theta2 great? ccw? nil))
  ([center radius theta1 theta2 great? ccw? attribs]
   (path (arc-segment center radius theta1 theta2 great? ccw?) attribs)))

(defn rect
  ([p w h]
   (rect p w h nil))
  ([p w h attribs]
   [:rect
    (svg-attribs
     attribs
     {:x (*ff* (first p)) :y (*ff* (nth p 1)) :width (*ff* w) :height (*ff* h)})]))

(defn line
  ([a b]
   (line a b nil))
  ([a b attribs]
   [:line
    (svg-attribs
     attribs
     {:x1 (*ff* (first a)) :y1 (*ff* (nth a 1)) :x2 (*ff* (first b)) :y2 (*ff* (nth b 1))})]))

(defn line-decorated
  ([p q start end]
   (line-decorated p q start end nil))
  ([p q start end attribs]
   (list
    (line p q attribs)
    (if start (start q p 0 attribs))
    (if end   (end p q 0 attribs)))))

(defn line-strip
  ([points]
   (line-strip points nil))
  ([points attribs]
   [:polyline
    (svg-attribs
     attribs
     {:fill "none"
      :points (apply f/format (point-seq-format (count points)) points)})]))

(defn line-strip-decorated
  ([points start seg end]
   (line-strip-decorated points start seg end nil))
  ([points start seg end attribs]
   (let [n (dec (count points))]
     (list
      (line-strip points attribs)
      (if start (start (nth points 1) (first points) 0 attribs))
      (if seg   (map-indexed (fn [i [p q]] (seg p q i attribs)) (partition 2 1 points)))
      (if end   (end (points (dec n)) (peek points) n attribs))))))

;; provide aliases

(def polyline line-strip)
(def polyline-decorated line-strip-decorated)

(defn polygon
  ([points]
   (polygon points nil))
  ([points attribs]
   [:polygon
    (svg-attribs
     attribs
     {:points (apply f/format (point-seq-format (count points)) points)})]))

(defn instance
  ([id]
   (instance id nil))
  ([id attribs]
   [:use (svg-attribs attribs {"xlink:href" (str "#" id)})]))

;; Decorators

(defn arrow-head
  ([len theta solid?]
   (arrow-head len theta solid? nil))
  ([len theta solid? opts]
   (fn [p q idx & [attribs]]
     (let [q (vec2 q)
           d (m/normalize (m/- q p) len)]
       (list
        ((if solid? polygon line-strip)
         [(m/- q (g/rotate d (- theta))) q (m/- q (g/rotate d theta))]
         (merge attribs opts)))))))

(defn line-label
  ([]
   (line-label nil))
  ([{:keys [__rotate? __offset] :as opts}]
   (let [opts (-> opts
                  (dissoc :__rotate? :__offset)
                  (update :text-anchor #(or % "middle")))]
     (fn [p q idx & [attribs]]
       (if-let [label (get-in attribs [:__label idx])]
         (let [p (vec2 p)
               m (m/+ (m/mix p q) __offset)
               opts (if __rotate?
                      (assoc opts
                             :transform (str "rotate("
                                             (m/degrees (g/heading (g/normal (m/- p q))))
                                             " " (first m) " " (nth m 1) ")"))
                      opts)]
           (list (text m label (merge (dissoc attribs :__label) opts)))))))))

(defn comp-decorators
  [& fns]
  (fn [p q idx & [attribs]]
    (reduce
     (fn [acc f] (concat acc (f p q idx attribs))) () fns)))
