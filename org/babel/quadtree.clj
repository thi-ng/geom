(require '[thi.ng.geom [core :as g] [rect :as r]] '[thi.ng.math.core :as m])

(defprotocol TreeOps
  (child-index-for-point [this p])
  (child-for-point [this p])
  (make-child-for-point [this p add-p?])
  (node-bounds [this])
  (split-node [this])
  (add-point [this p]))

(defn root-node*
  [this path]
  (loop [p (:parent this) n this path path]
    (if (seq path)
      (recur
       (:parent p)
       (->> n (assoc (:children p) (peek path)) (assoc p :children))
       (pop path))
      n)))

(defn add-point*
  [this p path]
  (if (:children this)
    (let [[c i] (make-child-for-point this p false)]
      (add-point* c p (conj path i)))
    (let [data (:data this)]
      (if data
        (if (m/delta= data p)
          (root-node* this path)
          (let [n (split-node this)
                [c i] (make-child-for-point n p true)]
            (-> n
                (assoc-in [:children i] c)
                (add-point* data [])
                (root-node* path))))
        (root-node* (assoc this :data p) path)))))

(def ^:const qtchildren [nil nil nil nil])

(defrecord QuadtreeNode [x y w h parent children data]
  TreeOps
  (child-index-for-point [this [px py]]
    (if (< px (+ x w))
      (if (< py (+ y h)) 0 2)
      (if (< py (+ y h)) 1 3)))
  (child-for-point [this p]
    (let [idx (child-index-for-point this p)] [(children idx) idx]))
  (make-child-for-point [this p add?]
    (let [idx (child-index-for-point this p)]
      (if (children idx)
        [(children idx) idx]
        (let [cx (if (pos? (bit-and idx 1)) (+ x w) x)
              cy (if (pos? (bit-and idx 2)) (+ y h) y)]
          [(QuadtreeNode. cx cy (* 0.5 w) (* 0.5 h) this nil (if add? p)) idx]))))
  (node-bounds [this]
    (thi.ng.geom.types.Rect. (g/vec2 x y) (* w 2) (* h 2)))
  (split-node [this]
    (assoc this :children qtchildren :data nil))
  (add-point
    [this p]
    (if (g/contains-point? (node-bounds this) p)
      (add-point* this p [])
      this)))

(defn quadtree
  "Create a new quadtree root node with the given XY position & dimensions."
  [x y w h]
  (QuadtreeNode. x y (* 0.5 w) (* 0.5 h) nil nil nil))

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

(def q (time (reduce add-point (quadtree 0 0 100 100) [[55 10] [25 10] [55 11]])))
