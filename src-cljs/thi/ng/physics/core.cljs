(ns thi.ng.physics.core
  (:require
   [thi.ng.data.core :as d]
   [thi.ng.geom.core :as g]
   ))

(defprotocol IUpdate
  (update [this]))

(defprotocol IBehavioral
  (add-behavior [this b])
  (remove-behavior [this b])
  (clear-behaviors [this])
  (apply-behaviors [this]))

(defprotocol IConstrained
  (add-constraint [this c])
  (remove-constraint [this c])
  (clear-constraints [this])
  (apply-constraints [this]))

(defprotocol IParticle
  (add-force [this f])
  (add-velocity [this v])
  (apply-force [this])
  (clear-force [this])
  (clear-velocity [this])
  (scale-velocity [this v])
  (set-weight [this w])
  (lock [this])
  (unlock [this])
  (locked? [this]))

(defprotocol IPhysics
  (update-particles [this])
  (update-springs [this]))

(defprotocol ISpringUpdate
  (update-spring [this particles]))

(defrecord VerletParticle [pos prev locked? weight inv-weight force behaviors constraints]
  IBehavioral
  (add-behavior [this b] (assoc this :behaviors (conj behaviors b)))
  (apply-behaviors [this] (reduce #(%2 %) this behaviors))
  IConstrained
  (add-constraint [this c] (assoc this :constraints (conj constraints c)))
  (apply-constraints [this] (reduce #(%2 %) this constraints))
  IParticle
  (lock [this] (assoc this :locked? true))
  (unlock [this] (assoc this :locked? false))
  (locked? [this] locked?)
  (set-weight [this w] (assoc this :weight w :inv-weight (/ 1.0 w)))
  (add-force [this f] (assoc this :force (g/add2 force f)))
  (apply-force [this]
    (assoc this
      :pos (-> force
               (g/scale2 weight)
               (g/add2 (g/sub2 pos prev))
               (g/add2 pos))
      :prev pos
      :force [0.0 0.0]))
  (scale-velocity [this s]
    (assoc this :prev (g/mix2 pos prev s)))
  IUpdate
  (update [this]
    (if locked?
      this
      (-> this
          apply-behaviors
          apply-force
          apply-constraints))))

(defrecord VerletPhysics [particles springs behaviors constraints timestep iter drag]
  IBehavioral
  (add-behavior [this b] (assoc this :behaviors (conj behaviors b)))
  (apply-behaviors [this]
    (assoc this :particles (vec (d/apply-fns behaviors particles))))
  IConstrained
  (add-constraint [this c] (assoc this :constraints (conj constraints c)))
  (apply-constraints [this]
    (assoc this :particles (vec (d/apply-fns constraints particles))))
  IPhysics
  (update-particles [this]
    (assoc this
      :particles (vec (map #(update (scale-velocity % drag)) particles))))
  (update-springs [this]
    (assoc this
      :particles
      (loop [i iter particles particles]
        (if (pos? i)
          (recur (dec i) (reduce (fn [particles s] (update-spring s particles)) particles springs))
          particles))))
  IUpdate
  (update [this]
    (-> this
        apply-behaviors
        update-particles
        update-springs
        apply-constraints)))

(defrecord VerletSpring [aid bid rlen strength a-locked? b-locked?]
  ISpringUpdate
  (update-spring [this particles]
    ;; (.log js/console "sup" (clj->js (map :pos particles)))
    (let [a (particles aid)
          b (particles bid)
          aw (:inv-weight a)
          bw (:inv-weight b)
          delta (g/sub2 (:pos b) (:pos a))
          dist (+ (g/mag2 delta) 1e-6)
          nd (* (/ (- dist rlen) (* dist (+ aw bw))) strength)
          ;;_ (.log js/console (clj->js [a b delta dist nd]))
          particles (if (or (:locked? a) a-locked?)
                      particles
                      (update-in particles [aid :pos] #(g/fma2 delta (* nd aw) %)))
          particles (if (or (:locked? b) b-locked?)
                      particles
                      (update-in particles [bid :pos] #(g/fma2 delta (* (- nd) bw) %)))]
      particles)))

(defn gravity
  [force timestep]
  (let [f-scaled (g/scale2 force (* timestep timestep))]
    (fn [p]
      (add-force p f-scaled))))
