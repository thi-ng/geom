(ns thi.ng.geom.test.core.protocols
  #?(:cljs
     (:require-macros
      [cemerick.cljs.test :refer (is deftest with-test run-tests testing)]))
  (:require
   [thi.ng.math.core :as m]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.matrix]
   [thi.ng.geom.quaternion]
   [thi.ng.geom.vector]
   #?(:clj
      [clojure.test :refer :all]
      :cljs
      [cemerick.cljs.test]))
  #?(:clj
     (:import
      [thi.ng.geom.matrix Matrix32 Matrix44]
      [thi.ng.geom.quaternion Quat4]
      [thi.ng.geom.vector Vec2 Vec3])))

(def proto-ids
  {:conj      m/IConjugate
   :cross     m/ICrossProduct
   :det       m/IDeterminant
   :dist      g/IDistance
   :dot       m/IDotProduct
   :head      g/IHeading
   :inv       m/IInvert
   :limit     m/ILimit
   :mag       m/IMagnitude
   :mat       g/IMatrixConvert
   :math      m/IMathOps
   :mimax     m/IMinMax
   :mix       m/IInterpolate
   :norm      m/INormalize
   :polar     g/IPolar
   :refl      g/IReflect
   :rotate    g/IRotate
   :rot3d     g/IRotate3D
   :scale     g/IScale
   :shear     g/IShear
   :translate g/ITranslate
   :tx        g/ITransform
   :vtx       g/IVectorTransform})

(def vec-common
  #{:cross :dist :dot :head :inv :limit :mag :math :mix
    :norm :polar :refl :rotate :scale :translate :tx})

(def mat-common
  #{:math :det :inv :rotate :scale :tx :vtx})

#?(:clj
   (defn satisfies-all?
     [type & protos]
     (testing
         (.getName type)
       (doseq [p protos]
         (is (true? (extends? (proto-ids p) type))
             (str (get-in proto-ids [p :on-interface])))))))

#?(:clj
   (deftest proto-implementation-check
     (dorun
      (map
       (fn [[t protos]] (apply satisfies-all? t protos))
       {
        thi.ng.geom.matrix.Matrix32
        (conj mat-common :shear)

        thi.ng.geom.matrix.Matrix44
        mat-common

        thi.ng.geom.quaternion.Quat4
        #{:conj :dot :inv :mag :mat :math :mix :norm :scale :vtx}

        thi.ng.geom.vector.Vec2
        vec-common

        thi.ng.geom.vector.Vec3
        (conj vec-common :rot3d)
        }))))
