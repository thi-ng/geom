(ns thi.ng.geom.svg.adapter
  (:require
   [thi.ng.math.core :as m]
   [thi.ng.geom.svg.core :as svg]
   [thi.ng.geom.core :as g]
   #?(:clj [thi.ng.geom.types]
      :cljs [thi.ng.geom.types :refer [Circle2 Line2 LineStrip2 Polygon2 Rect2 Triangle2]])
   [thi.ng.dstruct.core :as d])
  #?(:clj
     (:import
      [thi.ng.geom.types Circle2 Line2 LineStrip2 Polygon2 Rect2 Triangle2])))

;; SVG conversions for geom.types
;;
;; This namespace provides some simple wrappers to allow direct use of
;; the shape entities defined in `src/types.cljc`, without having to
;; manually convert them into their SVG representations.
;;
;; The adapters work by providing implementations of the `ISVGConvert`
;; protocol for all built-in 2D types and a simple helper function to
;; recursively transform any such types used within an SVG scene.
;;
;; Any 3D entities (e.g. meshes) need to be processed via the
;; `thi.ng.geom.svg.renderer` namespace.


;; Adapter implementations

(extend-protocol svg/ISVGConvert

  Line2
  (as-svg
    [{p :points} {:keys [__start __end] :as opts}]
    (if (or __start __end)
      (svg/line-decorated (p 0) (p 1) __start __end opts)
      (svg/line (p 0) (p 1) opts)))

  Circle2
  (as-svg
    [_ opts] (svg/circle (get _ :p) (get _ :r) opts))

  LineStrip2
  (as-svg
    [{:keys [points]} {:keys [__start __segment __end] :as opts}]
    (if (or __start __segment __end)
      (svg/line-strip-decorated points __start __segment __end opts)
      (svg/line-strip points opts)))

  Polygon2
  (as-svg
    [_ opts] (svg/polygon (get _ :points) opts))

  Rect2
  (as-svg
    [{:keys [p size]} opts] (svg/rect p (size 0) (size 1) opts))

  Triangle2
  (as-svg
    [_ opts] (svg/polygon (get _ :points) opts)))

(defn all-as-svg
  [form]
  (d/postwalk
   (fn [x] (if (satisfies? svg/ISVGConvert x) (svg/as-svg x (meta x)) x))
   form))

;; React.js :key prop injection

(defn key-attrib-injector
  "To be used with inject-element-attribs, generates an unique :key
  attrib for each SVG element w/o :key attrib. Returns updated attribs."
  [el attribs] (if (get attribs :key)
                 attribs
                 (assoc attribs :key (str (gensym) (hash el)))))

(defn inject-element-attribs
  "Walks SVG DOM tree with thi.ng.dstruct.core/postwalk and applies
  given function to each element node. The fn takes 2 args: the
  element itself and its attribute map. The fn's return value will be
  used as the new attribute map."
  ([root]
   (inject-element-attribs key-attrib-injector root))
  ([f root]
   (d/postwalk
    (fn [x]
      (if (vector? x)
        (let [y (nth x 1)]
          (if (or (nil? y) (map? y))
            (assoc x 1 (f x y))
            x))
        x))
    root)))
