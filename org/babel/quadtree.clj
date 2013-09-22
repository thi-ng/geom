(require '[thi.ng.geom [core :as g] [rect :as r] [aabb :as a]] '[thi.ng.math.core :as m])

(defprotocol TreeOps
  (child-index-for-point [this p])
  (child-for-point [this p])
  (make-child-for-point [this p add-p?])
  (node-bounds [this])
  (split-node [this])
  (add-point [this p])
  (delete-point [this p]))

(defn commit-path
  [node path]
  (if (seq path)
    (let [[parent i] (peek path)]
      (recur
       (->> node (assoc (:children parent) i) (assoc parent :children))
       (pop path)))
    node))

(defn commit-prune
  [node path]
  (let [children (filter identity (:children node))
        node (if (seq children)
               (if (= 1 (count children))
                 (if-let [d (:data (first children))]
                   (assoc node :data d :children nil)
                   node)
                 node))]
    (if (seq path)
      (let [[parent i] (peek path)]
        (recur
         (->> node
              (assoc (:children parent) i)
              (assoc parent :children))
         (pop path)))
      node)))

(defn add-point*
  [this p path]
  (if (:children this)
    (let [[c i] (make-child-for-point this p false)]
      (add-point* c p (conj path [this i])))
    (let [data (:data this)]
      (if data
        (if (m/delta= data p)
          (or (ffirst path) this)
          (let [n (split-node this)
                [c i] (make-child-for-point n p true)]
            (-> n
                (assoc-in [:children i] c)
                (add-point* data [])
                (commit-path path))))
        (commit-path (assoc this :data p) path)))))

(defn delete-point*
  [this p path]
  (if (:children this)
    (let [[c i] (child-for-point this p)]
      (if c
        (delete-point* c p (conj path [this i]))
        (or (ffirst path) this)))
    (if (m/delta= p (:data this))
      (if (seq path)
        (let [[p i] (peek path)
              p (assoc-in p [:children i] nil)]
          (commit-prune p (pop path)))
        (assoc this :data nil))
      (or (ffirst path) this))))

(def ^{:const true :private true} qt-children [nil nil nil nil])
(def ^{:const true :private true} ot-children [nil nil nil nil nil nil nil nil])

(defrecord QuadtreeNode [x y w h children data]
  TreeOps
  (child-index-for-point [this [px py]]
    (if (< px (+ x w))
      (if (< py (+ y h)) 0 2)
      (if (< py (+ y h)) 1 3)))
  (child-for-point [this p]
    (let [idx (child-index-for-point this p)]
      [(children idx) idx]))
  (make-child-for-point [this p add?]
    (let [idx (child-index-for-point this p)]
      (if (children idx)
        [(children idx) idx]
        (let [cx (if (pos? (bit-and idx 1)) (+ x w) x)
              cy (if (pos? (bit-and idx 2)) (+ y h) y)]
          [(QuadtreeNode. cx cy (* 0.5 w) (* 0.5 h) nil (if add? p)) idx]))))
  (node-bounds [this]
    (thi.ng.geom.types.Rect. (g/vec2 x y) (* w 2) (* h 2)))
  (split-node [this]
    (assoc this :children qt-children :data nil))
  (add-point
    [this p]
    (if (g/contains-point? (node-bounds this) p)
      (add-point* this p [])
      this))
  (delete-point
    [this p]
    (if (g/contains-point? (node-bounds this) p)
      (delete-point* this p [])
      this)))

(defrecord OctreeNode [x y z w h d children data]
  TreeOps
  (child-index-for-point [this [px py pz]]
    (+ (if (< pz (+ z d)) 0 4)
       (if (< px (+ x w))
         (if (< py (+ y h)) 0 2)
         (if (< py (+ y h)) 1 3))))
  (child-for-point [this p]
    (let [idx (child-index-for-point this p)]
      [(children idx) idx]))
  (make-child-for-point [this p add?]
    (let [idx (child-index-for-point this p)]
      (if (children idx)
        [(children idx) idx]
        (let [cx (if (pos? (bit-and idx 1)) (+ x w) x)
              cy (if (pos? (bit-and idx 2)) (+ y h) y)
              cz (if (pos? (bit-and idx 4)) (+ z d) z)]
          [(OctreeNode. cx cy cz (* 0.5 w) (* 0.5 h) (* 0.5 d) nil (if add? p)) idx]))))
  (node-bounds [this]
    (thi.ng.geom.types.AABB. (g/vec3 x y z) (g/vec3 (* w 2) (* h 2) (* d 2))))
  (split-node [this]
    (assoc this :children ot-children :data nil))
  (add-point
    [this p]
    (if (g/contains-point? (node-bounds this) p)
      (add-point* this p [])
      this))
  (delete-point
    [this p]
    (if (g/contains-point? (node-bounds this) p)
      (delete-point* this p [])
      this)))

(defn quadtree
  "Create a new quadtree root node with the given XY position & dimensions."
  [x y w h]
  (QuadtreeNode. x y (* 0.5 w) (* 0.5 h) nil nil))

(defn octree
  "Create a new octree root node with the given XYZ position & dimensions."
  [x y z w h d]
  (OctreeNode. x y z (* 0.5 w) (* 0.5 h) (* 0.5 d) nil nil))

(defn select-with
  "Produces a seq of points in the tree within a given region.
    Uses two predicate fns to logically define the query region:

    `isec?`: single-arg fn to check if the bounds of a tree node intersect the region
    `filter?`: single-arg fn to check if a point lies within the region."
  ([isec? inside? q]
     (select-with isec? inside? q nil))
  ([isec? inside? q acc]
     (if (isec? (node-bounds q))
       (if (:children q)
         (reduce
          (fn [acc c] (if c (select-with isec? inside? c acc) acc))
          acc (:children q))
         (let [p (:data q)]
           (if (and p (inside? p)) (conj acc p) acc)))
       acc)))

(defn select-with-shape
  [s q] (select-with #(g/intersect-shape s %) #(g/contains-point? s %) q))

(def q (time (reduce add-point (quadtree 0 0 100 100) [[55 10] [25 10] [55 11] [52 11]])))
(def o (time (reduce add-point (octree 0 0 0 100 100 100) [[55 10 25] [25 10 55] [55 11 25]])))


(let [points [[55 10] [25 10] [55 11] [52 11]]
      q (reduce add-point (quadtree 0 0 100 100) points)]
  (pprint q)
  (reduce
   (fn [q p]
     (prn "-----" p)
     (let [q (delete-point* q p [])]
       (pprint q)
       q))
   q points))
