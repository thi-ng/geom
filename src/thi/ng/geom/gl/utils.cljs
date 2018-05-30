(ns thi.ng.geom.gl.utils
  (:require
   [thi.ng.typedarrays.core :as arrays]
   [thi.ng.xerror.core :as err])
  (:require-macros
   [thi.ng.math.macros :as mm]))

(defn get-script-text
  [id]
  (if-let [e (.getElementById js/document id)]
    (.-text e)
    (err/illegal-arg! (str "Unknown DOM element: " id))))

(defn loop-kv
  "A combination of map & doseq specialized for maps. Takes a function `f` and
    a map, calls `f` with each key & value, discards results."
  [f xs]
  (loop [xs xs]
    (if xs
      (let [x (first xs)]
        (f (first x) (nth x 1))
        (recur (next xs))))))
