(ns thi.ng.geom.app
  (:require
   [thi.ng.geom.core :as g]
   [thi.ng.geom.circle :as c]
   [thi.ng.physics.core :as phys]
   [thi.ng.viz.core :as viz]))

(defn draw-springs
  [ctx p]
  (doseq [{:keys [aid bid]} (:springs p)]
    (viz/line
     ctx
     (get-in p [:particles aid :pos])
     (get-in p [:particles bid :pos]))))

(defn ^:export main
  [num-frames]
  (let [o [320 240]
        points (cons o (:points (g/as-polygon (c/circle o 200) 20)))
        particles (vec (map #(phys/map->VerletParticle
                              {:pos % :prev %
                               :weight 1 :inv-weight 1
                               :force [0 0]
                               :behaviors []
                               :constraints []})
                            points))
        particles (update-in particles [0] phys/lock)
        springs (vec (concat
                      (map #(phys/map->VerletSpring
                             {:aid 0 :bid % :rlen 200 :strength 0.01})
                           (range 1 (count points)))
                      (map #(let [bid (inc %)
                                  bid (if (= (count points) bid) 1 bid)
                                  a (particles %)
                                  b (particles bid)
                                  l (g/dist2 (:pos a) (:pos b))]
                              (phys/map->VerletSpring
                               {:aid % :bid bid :rlen l :strength 0.01}))
                           (range 1 (count points)))))
        particles (update-in particles [1 :pos] g/add2 [100 0])
        _ (.log js/console (clj->js springs))
        p (phys/map->VerletPhysics
           {:particles particles
            :springs springs
            :behaviors [(phys/gravity [0 1.5] 1)]
            :constraints []
            :timestep 1
            :iter 50
            :drag 0.95})
        canv (viz/make-canvas :width 640 :height 480 :parent "main")
        ctx (viz/canvas-context-2d canv :strokeStyle "#f00")
        t0 (.getTime (js/Date.))]
    (loop [i num-frames p p]
      (if (pos? i)
        (let [p (phys/update p)]
          (viz/context! ctx :strokeStyle (str "rgba(" (* 255 (- 1.0 (/ i num-frames))) ",0,0,0.25)"))
          (draw-springs ctx p)
          (recur (dec i) p))
        (do
          (draw-springs ctx p)
          (.log js/console (- (.getTime (js/Date.)) t0))
          (clj->js p))
        ))))
