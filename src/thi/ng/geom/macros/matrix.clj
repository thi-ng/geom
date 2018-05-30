(ns thi.ng.geom.macros.matrix
  (:require [thi.ng.math.macros :as mm]))

(defmacro det-item
  [a b c d, e f g h, i j k l, m n o p]
  `(+ (mm/sub
       (mm/mul ~a ~b ~c ~d)
       (mm/mul ~e ~f ~g ~h)
       (mm/mul ~i ~j ~k ~l))
      (mm/mul ~m ~n ~o ~p)))

(defmacro inv-item
  [a b c d e f g]
  `(* (mm/msubadd ~a ~b ~c ~d ~e ~f) ~g))
