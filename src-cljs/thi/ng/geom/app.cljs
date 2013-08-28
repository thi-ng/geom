(ns thi.ng.geom.app
  (:require
   [thi.ng.geom.core :as g]
   [thi.ng.geom.circle :as c]
   [thi.ng.physics.core :as phys]
   [thi.ng.viz.core :as viz]))

(defn draw-springs
  [ctx p]
  (doseq [{:keys [a b]} (:springs p)]
    (viz/line ctx (.-pos a) (.-pos b))))

(defn ^:export main
  [num-frames]
  (let [o [320 240]
        points (cons o (:points (g/as-polygon (c/circle o 200) 200)))
        particles (vec (map #(phys/VerletParticle. % % false 1 1 [0 0] [] [])
                            points))
        particles (update-in particles [0] phys/lock)
        ;; _ (.log js/console (clj->js particles))
        springs (vec (concat
                      (map #(phys/map->VerletSpring
                             {:a (particles 0) :b (particles %) :rlen 200 :strength 0.01})
                           (range 1 (count points)))
                      (map #(let [bid (inc %)
                                  bid (if (= (count points) bid) 1 bid)
                                  a (particles %)
                                  b (particles bid)
                                  l (g/dist2 (.-pos a) (.-pos b))]
                              (phys/map->VerletSpring
                               {:a a :b b :rlen l :strength 0.01}))
                           (range 1 (count points)))))
        _ (set! (.-pos (particles 1)) (g/add2 (.-pos (particles 1)) [100 0]))
        ;; _ (.log js/console (clj->js springs))
        p (phys/map->VerletPhysics
           {:particles particles
            :springs springs
            :behaviors [(phys/gravity [0 1.5] 1)]
            :constraints []
            :timestep 1
            :iter 30
            :drag 0.99})
        canv (viz/make-canvas :width 640 :height 480 :parent "main")
        ctx (viz/canvas-context-2d canv :strokeStyle "#f00")
        t0 (.getTime (js/Date.))]
    (doseq [i (range num-frames)]
      (phys/update p)
      ;;(viz/context! ctx :strokeStyle (str "rgba(" (* 255 (- 1.0 (/ i num-frames))) ",0,0,0.1)"))
      ;;(draw-springs ctx p)
      )
    (viz/context! ctx :strokeStyle "#00f")
    (draw-springs ctx p)
    (.log js/console (- (.getTime (js/Date.)) t0))
    (clj->js p)
    ))
