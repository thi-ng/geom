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
  (update-spring [this]))

(deftype VerletParticle [pos prev locked? weight inv-weight force behaviors constraints]
  IBehavioral
  (add-behavior [this b] (set! (.-behaviors this) (conj behaviors b)) this)
  (apply-behaviors [this] (reduce #(%2 %) this behaviors))
  IConstrained
  (add-constraint [this c] (set! (.-constraints this) (conj constraints c)) this)
  (apply-constraints [this] (reduce #(%2 %) this constraints))
  IParticle
  (lock [this]
    (set! (.-locked? this) true)
    this)
  (unlock [this]
    (set! (.-locked? this) false)
    this)
  (locked? [this] locked?)
  (set-weight [this w]
    (set! (.-weight this) w)
    (set! (.-inv-weight this) (/ 1.0 w))
    this)
  (add-force [this f]
    (set! (.-force this) (g/add2 force f))
    this)
  (apply-force [this]
    (let [t pos]
      (set! (.-pos this)
            (-> force
                (g/scale2 weight)
                (g/add2 (g/sub2 pos prev))
                (g/add2 pos)))
      (set! (.-prev this) t)
      (set! (.-force this) [0.0 0.0]))
    this)
  (scale-velocity [this s]
    (set! (.-prev this) (g/mix2 pos prev s))
    this)
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
    (doseq [b behaviors p particles] (b p))
    this)
  IConstrained
  (add-constraint [this c] (assoc this :constraints (conj constraints c)))
  (apply-constraints [this]
    (doseq [c constraints p particles] (c p))
    this)
  IPhysics
  (update-particles [this]
    (doseq [p particles] (update (scale-velocity p drag)))
    this)
  (update-springs [this]
    (doseq [i (range iter) s springs] (update-spring s))
    this)
  IUpdate
  (update [this]
    (-> this
        apply-behaviors
        update-particles
        update-springs
        apply-constraints)))

(defrecord VerletSpring [a b rlen strength a-locked? b-locked?]
  ISpringUpdate
  (update-spring [this]
    (let [aw (.-inv-weight a)
          bw (.-inv-weight b)
          delta (g/sub2 (.-pos b) (.-pos a))
          dist (+ (g/mag2 delta) 1e-6)
          nd (* (/ (- dist rlen) (* dist (+ aw bw))) strength)]
      (when-not (or (.-locked? a) a-locked?)
        (set! (.-pos a) (g/fma2 delta (* nd aw) (.-pos a))))
      (when-not (or (.-locked? b) b-locked?)
        (set! (.-pos b) (g/fma2 delta (* (- nd) bw) (.-pos b)))))))

(defn gravity
  [force timestep]
  (let [f-scaled (g/scale2 force (* timestep timestep))]
    (fn [p]
      (add-force p f-scaled))))
