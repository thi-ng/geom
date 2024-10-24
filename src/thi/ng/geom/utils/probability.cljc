(ns thi.ng.geom.utils.probability
  (:require [thi.ng.math.core :as m]))

;; TODO: move namespace to thi.ng.math.core?

(defn- mapping
  "Create a mapping of every value in collection to f(value)."
  [f coll]
  (reduce (fn [m x] (assoc m x (f x))) {} coll))

(defn rand-weighted
  "Given a mapping of values to weights, randomly choose a value biased by weight"
  [weights]
  (let [sample (m/random (apply + (vals weights)))]
    (loop [cumulative 0.0
           [[choice weight] & remaining] weights]
      (when weight
        (let [sum (+ cumulative weight)]
          (if (< sample sum)
            choice
            (recur sum remaining)))))))

(defn rand-weighted-by
  "Given a sequence of values `xs`, weight each value by a function `f` and return
  a weighted random selection."
  [f xs]
  (rand-weighted (mapping f xs)))

(comment
  (rand-weighted {})
  (frequencies (repeatedly 1000 #(rand-weighted {:a 0.2 :b 0.8})))
  (frequencies (repeatedly 1000 #(rand-weighted {:a 0.1 :b 0.7 :c 0.2})))
  (frequencies (repeatedly 1000 #(rand-weighted {:a 2 :b 8 :c 32})))
  (rand-weighted-by inc [])
  (frequencies (repeatedly 1000 #(rand-weighted-by inc [0 2 5]))))
