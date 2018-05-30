(ns thi.ng.geom.macros.voxel)

(defmacro not-cond->
  "Like clojure.core/cond-> but with inverted test semantics."
  [expr & clauses]
  (assert (even? (count clauses)))
  (let [g (gensym)
        pstep (fn [[test step]] `(if ~test ~g (-> ~g ~step)))]
    `(let [~g ~expr
           ~@(interleave (repeat g) (map pstep (partition 2 clauses)))]
       ~g)))
