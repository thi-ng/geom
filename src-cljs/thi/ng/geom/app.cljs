(ns thi.ng.geom.app
  (:require
   [thi.ng.geom.core :as g]
   [thi.ng.physics.core :as phys]
   [thi.ng.viz.core :as viz]))

(defn ^:export main
  [num-frames]
  (let [points [[0 0] [100 0] [100 100] [200 100]]
        particles (vec (map #(phys/map->VerletParticle
                              {:pos % :prev %
                               :weight 1 :inv-weight 1
                               :force [0 0]
                               :behaviors []
                               :constraints []})
                            points))
        particles (update-in particles [0] phys/lock)
        particles (update-in particles [(dec (count points))] phys/lock)
        springs (vec (map #(phys/map->VerletSpring
                            {:aid % :bid (inc %) :rlen 150 :strength 0.1})
                          (range (dec (count points)))))
        p (phys/map->VerletPhysics
           {:particles particles
            :springs springs
            :behaviors []
            :constraints []
            :timestep 1
            :iter 50
            :drag 0.95})
        canv (viz/make-canvas :width 640 :height 480 :parent "main")
        ctx (viz/canvas-context-2d canv :strokeStyle "#f00")]
    (loop [i num-frames p p]
      (if (pos? i)
        (let [p (phys/update p)]
          ;;(.log js/console "-----")
          ;;(viz/linestrip ctx (map :pos (:particles p)))
          (recur (dec i) p))
        (do
          (viz/linestrip ctx (map :pos (:particles p)))
          (clj->js p))))))
