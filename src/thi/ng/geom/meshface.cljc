(ns thi.ng.geom.meshface
  (:require
   [thi.ng.geom.core :as g]
   [thi.ng.geom.vector :as v :refer [vec2 vec3]]))

(defn xf-face-verts
  [mesh] (map #(first (g/raw % mesh))))

(deftype MeshFace
    [vertices
     #?@(:clj [^:unsynchronized-mutable _hash]
         :cljs [^:mutable _hash])]

  g/IVertexAccess
  (vertices [_] vertices)
  (vertices [_ _] vertices)

  g/IAttributeAccess
  (attribs [_ _] nil)

  g/IRawAccess
  (raw [_ _] [vertices])

  Object
  (toString [_] (str \[ vertices \]))
  #?@(:clj
      [(hashCode [_] (or _hash (set! _hash (.hashCode vertices))))
       (equals
        [_ o]
        (and (instance? MeshFace o)
             (clojure.lang.Util/equals vertices (.-vertices ^MeshFace o))))

       Comparable
       (compareTo [_ o] (compare vertices (.-vertices ^MeshFace o)))

       clojure.lang.IHashEq
       (hasheq [_] (.hashCode _))]

      :cljs
      [IHash
       (-hash [_] (or _hash (set! _hash (hash vertices))))

       IComparable
       (-compare [_ o] (compare vertices (.-vertices ^MeshFace o)))]))

(deftype IndexedMeshFace
    [vertices attribs
     #?@(:clj [^:unsynchronized-mutable _hash]
         :cljs [^:mutable _hash])]

  g/IVertexAccess
  (vertices [_ mesh]
    (let [idx (-> mesh :vertices :id->v)]
      (mapv #(get idx %) vertices)))

  g/IAttributeAccess
  (attribs [_ mesh]
    (let [mattr (get mesh :attribs)]
      (reduce-kv
       (fn [acc k v]
         (let [idx (-> mattr (get k) (get :id->v))]
           (assoc acc k (if (sequential? v) (mapv #(get idx %) v) (get idx v)))))
       {} attribs)))
  (attribs [_ mesh attr]
    (let [idx  (-> mesh (get :attribs) (get attr) (get :id->v))
          aval (attribs attr)]
      (if (sequential? aval) (mapv idx attribs) (idx aval))))

  g/IRawAccess
  (raw [_ mesh]
    [(g/vertices _ mesh) (g/attribs _ mesh)])

  Object
  (toString [_]
    (str \[ vertices \space attribs \]))
  #?@(:clj
      [(hashCode
        [_]
        (or _hash
            (set! _hash
                  (unchecked-add-int
                   (unchecked-multiply-int (.hashCode vertices) 31)
                   (.hashCode attribs)))))
       (equals
        [_ o]
        (and (instance? IndexedMeshFace o)
             (clojure.lang.Util/equals vertices (.-vertices ^IndexedMeshFace o))
             (clojure.lang.Util/equals attribs (.-attribs ^IndexedMeshFace o))))

       Comparable
       (compareTo
        [_ o]
        (let [c (compare vertices (.-vertices ^IndexedMeshFace o))]
          (if (zero? c)
            (compare attribs (.-attribs ^IndexedMeshFace o))
            c)))

       clojure.lang.IHashEq
       (hasheq [_] (.hashCode _))]

      :cljs
      [IHash
       (-hash
        [_]
        (or _hash
            (set! _hash
                  (-> (hash vertices)
                      (imul 31)
                      (+ (hash attribs))
                      (bit-or 0)))))

       IComparable
       (-compare
        [_ o]
        (let [c (compare vertices (.-vertices ^IndexedMeshFace o))]
          (if (zero? c)
            (compare attribs (.-attribs ^IndexedMeshFace o))
            c)))]))
