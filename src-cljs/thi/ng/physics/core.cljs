(ns thi.ng.physics.core
  (:require
   [thi.ng.geom.core :as g]))

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
  (set-weight [this w])
  (lock [this])
  (unlock [this])
  (locked? [this]))

(defprotocol IPhysics)

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
  (apply-force [this]
    (assoc this
      :pos (-> force
               (g/scale2 weight)
               (g/add2 (g/sub2 pos prev))
               (g/add2 pos))
      :prev pos
      :force [0.0 0.0]))
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
      :springs (vec (reduce (fn [springs _] (map update springs)) (range iter)))))
  IUpdate
  (update [this]
    (-> this
        apply-behaviors
        update-particles
        update-springs
        apply-constraints))
  )
