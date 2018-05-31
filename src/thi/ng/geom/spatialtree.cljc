(ns thi.ng.geom.spatialtree
  (:require
   [thi.ng.geom.core :as g]
   [thi.ng.geom.utils :as gu]
   [thi.ng.geom.vector :as v :refer [vec2 vec3]]
   [thi.ng.geom.utils.intersect :as isec]
   [thi.ng.geom.aabb :as a]
   [thi.ng.geom.rect :as r]
   #?(:clj [thi.ng.geom.types] :cljs [thi.ng.geom.types :refer [AABB Rect2]])
   [thi.ng.math.core :as m :refer [*eps* delta=]])
  #?(:clj (:import [thi.ng.geom.types AABB Rect2])))

(defprotocol PTreeOps
  (child-index-for-point [_ p])
  (child-for-point [_ p])
  (get-children [_])
  (set-child [_ i c])
  (set-children [_ c])
  (set-point [_ p d])
  (make-child-for-point [_ p d add-point?])
  (split-node [_]))

(defn path-for-point
  "Returns depth-first seq of nodes for given point."
  [root p]
  (loop [node root, path (list root)]
    (let [c (child-for-point node p)]
      (if c (recur c (conj path c)) path))))

(defn add-point*
  "Associates point with data in tree, recursively creates all required intermediate nodes."
  [root p d]
  (loop [node root, p p, d d]
    (if (get-children node)
      (recur (make-child-for-point node p d false) p d)
      (let [point (g/get-point node)]
        (if point
          (if-not (delta= point p *eps*)
            (let [data (g/get-point-data node)]
              (split-node node)
              (make-child-for-point node p d true)
              (recur node point data)))
          (set-point node p d))))))

(defn delete-point*
  "Removes point from tree (if found) and prunes any resulting empty nodes.
  Returns given node (root)."
  [root p]
  (let [[node & path] (path-for-point root p)]
    (when (delta= p (g/get-point node) *eps*)
      (set-point node nil nil)
      (loop [path path]
        (if path
          (let [[node & parents] path]
            (set-child node (child-index-for-point node p) nil)
            (when (every? nil? (get-children node))
              (set-children node nil)
              (recur parents))))))
    root))

(defn check-with
  [q isec? inside?]
  (if (isec? (g/bounds q))
    (let [children (get-children q)]
      (if children
        (loop [ch children]
          (if ch
            (if-let [c (first ch)]
              (if (check-with c isec? inside?)
                true
                (recur (next ch)))
              (recur (next ch)))))
        (let [p (g/get-point q)]
          (and p (inside? p)))))))

(defn select-with
  "Produces a vector of points in the tree within a given region.
  Uses two predicate fns to logically define the query region:

  `isec?`  : single-arg fn to check if the bounds of a tree node
  intersect the query region
  `inside?`: single-arg fn to check if a point lies within the region."
  ([q isec? inside?]
   (select-with q isec? inside? (transient [])))
  ([q isec? inside? acc]
   (if (isec? (g/bounds q))
     (let [children (get-children q)]
       (if children
         (reduce
          (fn [acc c] (if c (select-with c isec? inside? acc) acc))
          acc children)
         (let [p (g/get-point q)]
           (if (and p (inside? p)) (conj! acc (g/get-point-data q)) acc))))
     acc)))

(defn select-with-shape
  [q s] (persistent! (select-with q #(g/intersect-shape s %) #(g/contains-point? s %))))

(defn- do-with-radius
  [f f2 q c r]
  (let [rsq (* r r)]
    (f q #(f2 (get % :p) (m/+ (get % :p) (get % :size)) c r) #(< (g/dist-squared c %) rsq))))

(defn points-in-circle?
  [q c r] (do-with-radius check-with isec/intersect-rect-circle? q c r))

(defn points-in-sphere?
  [q c r] (do-with-radius check-with isec/intersect-aabb-sphere? q c r))

(defn select-with-circle
  [q c r] (persistent! (do-with-radius select-with isec/intersect-rect-circle? q c r)))

(defn select-with-sphere
  [q c r] (persistent! (do-with-radius select-with isec/intersect-aabb-sphere? q c r)))

(defn- lazy-select-with*
  [isec? inside? queue]
  (lazy-seq
   (let [[q & r] queue]
     (if (and q (isec? (g/bounds q)))
       (let [cs (filter identity (get-children q))
             p (g/get-point q)]
         (if (seq cs)
           (lazy-select-with* isec? inside? (concat cs r))
           (if (and p (inside? p))
             (cons (g/get-point-data q) (lazy-select-with* isec? inside? r))
             (when (seq r) (lazy-select-with* isec? inside? r)))))
       (when (seq r) (lazy-select-with* isec? inside? r))))))

(defn lazy-select-with
  "Same as `select-with`, only produces a lazy sequence of points."
  [q isec? inside?]
  (lazy-select-with* isec? inside? [q]))

(defn lazy-select-with-shape
  [q s] (lazy-select-with* #(g/intersect-shape s %) #(g/contains-point? s %) [q]))

(deftype MutableQuadtreeNode
    #?(:clj
       [^double x ^double y ^double w ^double h
        ^:unsynchronized-mutable children
        ^:unsynchronized-mutable point
        ^:unsynchronized-mutable data
        ^:unsynchronized-mutable bounds]
       :cljs
       [x y w h
        ^:mutable children
        ^:mutable point
        ^:mutable data
        ^:mutable bounds])

  g/ISpatialTree
  (add-point [_ p d] (add-point* _ p d) _)
  (delete-point [_ p] (delete-point* _ p))
  (get-point [_] point)
  (get-point-data [_] data)

  g/IClear
  (clear*
    [_] (MutableQuadtreeNode. x y w h nil nil nil bounds))
  (clear!
    [_]
    (set! children nil)
    (set! children point)
    (set! children data)
    _)

  PTreeOps
  (child-index-for-point
    [_ [px py]]
    (if (< px (+ x w))
      (if (< py (+ y h)) 0 2)
      (if (< py (+ y h)) 1 3)))
  (child-for-point
    [_ p] (if children (children (child-index-for-point _ p))))
  (make-child-for-point
    [_ p d add?]
    (let [idx (child-index-for-point _ p)]
      (or (children idx)
          (let [cx (if (> (bit-and idx 1) 0) (+ x w) x)
                cy (if (> (bit-and idx 2) 0) (+ y h) y)
                c  (MutableQuadtreeNode.
                    cx cy (* 0.5 w) (* 0.5 h) nil (if add? p) (if add? d) nil)]
            (set-child _ idx c)
            c))))
  (split-node
    [_]
    (set! children [nil nil nil nil])
    (set! point nil)
    (set! data nil)
    _)
  (get-children [_] children)
  (set-child [_ i c] (set! children (assoc children i c)) _)
  (set-children [_ c] (set! children c) _)
  (set-point [_ p d] (set! point p) (set! data d) _)

  g/IBounds
  (bounds
    [_]
    (if bounds
      bounds
      (set! bounds
            (Rect2.
             (vec2 x y) (vec2 (* w 2.0) (* h 2.0))))))

  Object
  (toString
    [_]
    (str "#thi.ng.geom.spatialtree.MutableQuadtreeNode"
         "{:bounds " (pr-str (g/bounds _))
         " :children " (pr-str children)
         " :p " (pr-str point)
         " :d " (pr-str data)
         "}")))

(deftype MutableOctreeNode
    #?(:clj
       [^double x ^double y ^double z ^double w ^double h ^double d
        ^:unsynchronized-mutable children
        ^:unsynchronized-mutable point
        ^:unsynchronized-mutable data
        ^:unsynchronized-mutable bounds]
       :cljs
       [x y z w h d
        ^:mutable children
        ^:mutable point
        ^:mutable data
        ^:mutable bounds])

  g/ISpatialTree
  (add-point [_ p d] (add-point* _ p d) _)
  (delete-point [_ p] (delete-point* _ p))
  (get-point [_] point)
  (get-point-data [_] data)

  g/IClear
  (clear*
    [_] (MutableOctreeNode. x y z w h d nil nil nil bounds))
  (clear!
    [_]
    (set! children nil)
    (set! children point)
    (set! children data)
    _)

  PTreeOps
  (child-index-for-point
    [_ [px py pz]]
    (+ (if (< pz (+ z d)) 0 4)
       (if (< px (+ x w))
         (if (< py (+ y h)) 0 2)
         (if (< py (+ y h)) 1 3))))
  (child-for-point
    [_ p] (if children (children (child-index-for-point _ p))))
  (make-child-for-point
    [_ p data add?]
    (let [idx (child-index-for-point _ p)]
      (if (children idx)
        (children idx)
        (let [cx (if (> (bit-and idx 1) 0) (+ x w) x)
              cy (if (> (bit-and idx 2) 0) (+ y h) y)
              cz (if (> (bit-and idx 4) 0) (+ z d) z)
              c  (MutableOctreeNode.
                  cx cy cz (* 0.5 w) (* 0.5 h) (* 0.5 d) nil (if add? p) (if add? data) nil)]
          (set-child _ idx c)
          c))))
  (split-node
    [_]
    (set! children [nil nil nil nil nil nil nil nil])
    (set! point nil)
    _)
  (get-children [_] children)
  (set-child [_ i c] (set! children (assoc children i c)) _)
  (set-children [_ c] (set! children c) _)
  (set-point [_ p d] (set! point p) (set! data d) _)

  g/IBounds
  (bounds
    [_]
    (if bounds
      bounds
      (set! bounds
            (AABB.
             (vec3 x y z) (vec3 (* w 2.0) (* h 2.0) (* d 2.0))))))

  Object
  (toString
    [_]
    (str "#thi.ng.geom.spatialtree.MutableOctreeNode"
         "{:bounds " (pr-str (g/bounds _))
         " :children " (pr-str children)
         " :p " (pr-str point)
         " :d " (pr-str data)
         "}")))
#?(:clj
   (do
     (require 'clojure.pprint)
     (defmethod print-method MutableQuadtreeNode
       [^MutableQuadtreeNode o ^java.io.Writer w] (.write w (.toString o)))
     (defmethod print-method MutableOctreeNode
       [^MutableOctreeNode o ^java.io.Writer w] (.write w (.toString o)))))

(defn quadtree
  "Create a new quadtree root node with the given XY position & dimensions."
  ([{[x y] :p [w h] :size}]
   (quadtree x y w h))
  ([[x y] size]
   (let [[w h] (if (number? size) [size size] size)]
     (quadtree x y w h)))
  ([x y size]
   (quadtree x y size size))
  ([x y w h]
   (MutableQuadtreeNode. x y (* 0.5 w) (* 0.5 h) nil nil nil nil)))

(defn octree
  "Create a new octree root node with the given XYZ position & dimensions."
  ([{[x y z] :p [w h d] :size}]
   (octree x y z w h d))
  ([[x y z] size]
   (let [[w h d] (if (number? size) [size size size] size)]
     (octree x y z w h d)))
  ([x y z size]
   (octree x y z size size size))
  ([x y z w h d]
   (MutableOctreeNode. x y z (* 0.5 w) (* 0.5 h) (* 0.5 d) nil nil nil nil)))
