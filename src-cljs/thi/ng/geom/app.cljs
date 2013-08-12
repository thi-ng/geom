(ns thi.ng.geom.app
  (:require
   [thi.ng.geom.core :as g]))

(defn ^:export main
  []
  (let [points [[0 0] [100 0] [100 100] [200 100]]
        bnd (g/bounding-rect* points)]
    (.log js/console (clj->js (g/centroid bnd)))))
