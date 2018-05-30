(ns thi.ng.geom.gl.webgl.animator)

(def animframe-provider
  (or
   (.-requestAnimationFrame js/self)
   (.-webkitRequestAnimationFrame js/self)
   (.-mozRequestAnimationFrame js/self)
   (.-msRequestAnimationFrame js/self)
   (.-oRequestAnimationFrame js/self)))

(defn now
  []
  (or
   (.now js/performance)
   (.webkitNow js/performance)
   (.mozNow js/performance)
   (.msNow js/performance)
   (.oNow js/performance)))

(defn animate
  ([f]
   (animate f nil))
  ([f element]
   (let [t0  (.getTime (js/Date.))
         fid (volatile! 0)
         f'  (fn animate* []
               (if (f (* (- (.getTime (js/Date.)) t0) 1e-3) (vswap! fid inc))
                 (if element
                   (animframe-provider animate* element)
                   (animframe-provider animate*))))]
     (f'))))
