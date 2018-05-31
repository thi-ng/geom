(ns thi.ng.geom.utils.intersect
  #?(:cljs
     (:require-macros
      [thi.ng.math.macros :as mm]))
  (:require
   [thi.ng.geom.core :as g]
   [thi.ng.geom.utils :as gu]
   [thi.ng.geom.vector :as v :refer [vec2 vec3 V2 V3]]
   [thi.ng.math.core :as m :refer [*eps*]]
   #?(:clj [thi.ng.math.macros :as mm])))

(declare intersect-plane-aabb?)

(defn- sq [x] (* x x))

(defn intersect-circle-circle?
  ([{p :p r1 :r} {q :p r2 :r}]
   (intersect-circle-circle? p r1 q r2))
  ([p r1 q r2]
   (let [delta (m/- q p)
         d (m/mag delta)]
     (when (and (<= d (+ r1 r2)) (>= d (m/abs* (- r1 r2))))
       (let [a (/ (+ (- (* r1 r1) (* r2 r2)) (* d d)) (* 2.0 d))
             invd (/ 1.0 d)
             p (m/madd delta (* a invd) p)
             h (Math/sqrt (- (* r1 r1) (* a a)))
             perp (m/* (g/normal delta) (* h invd))]
         [(m/+ p perp) (m/- p perp)])))))

(defn intersect-rect-rect?
  ([{[px py] :p [w h] :size} {[qx qy] :p [qw qh] :size}]
   (not (or (> px (+ qx qw)) (> qx (+ px w)) (> py (+ qy qh)) (> qy (+ py h)))))
  ([[px1 py1] [qx1 qy1] [px2 py2] [qx2 qy2]]
   (not (or (> px1 qx2) (> px2 qx1) (> py1 qy2) (> py2 qy1)))))

(defn intersect-rect-circle?
  ([{p :p sz :size} {s :p r :r}]
   (intersect-rect-circle? p (m/+ p sz) s r))
  ([[px py] [qx qy] [cx cy] r]
   (let [ds (if (< cx px) (sq (- cx px)) (if (> cx qx) (sq (- cx qx)) 0.0))
         ds (+ ds (if (< cy py) (sq (- cy py)) (if (> cy qy) (sq (- cy qy)) 0.0)))]
     (<= ds (* r r)))))

(defn intersect-aabb-aabb?
  ([{pa :p sa :size} {pb :p sb :size}]
   (intersect-aabb-aabb? pa pb (m/+ pa sa) (m/+ pb sb)))
  ([pa pb qa qb]
   (if (and (<= (pa 0) (qb 0)) (<= (pb 0) (qa 0)))
     (if (and (<= (pa 1) (qb 1)) (<= (pb 1) (qa 1)))
       (and (<= (pa 2) (qb 2)) (<= (pb 2) (qa 2)))))))

;; Source: Graphics Gems 2 / SO:
;; http://stackoverflow.com/questions/4578967

(defn intersect-aabb-sphere?
  ([{p :p sz :size} {s :p r :r}]
   (intersect-aabb-sphere? p (m/+ p sz) s r))
  ([[px py pz] [qx qy qz] [cx cy cz] r]
   (let [ds (if (< cx px) (sq (- cx px)) (if (> cx qx) (sq (- cx qx)) 0.0))
         ds (+ ds (if (< cy py) (sq (- cy py)) (if (> cy qy) (sq (- cy qy)) 0.0)))
         ds (+ ds (if (< cz pz) (sq (- cz pz)) (if (> cz qz) (sq (- cz qz)) 0.0)))]
     (<= ds (* r r)))))

(comment
  ;; example usage

  (isec/intersect-aabb-frustum?
   box-p box-size
   (mat/frustum-planes view-mat proj-mat)))

;; https://groups.google.com/forum/#!topic/comp.graphics.algorithms/M6lvyWC6PqU

(defn intersect-aabb-frustum?
  "Takes 2 vectors defining an AABB (min-p & size) and a seq of plane
  parameters (each element [normal w]). The plane normals must be
  pointing *inwards*. Returns :inside, :intersect or :outside"
  [[px py pz :as p] size planes]
  (let [[qx qy qz] (m/+ p size)]
    (reduce
     (fn [res [[nx ny nz] w]]
       (let [vx (if (pos? nx) px qx)
             vy (if (pos? ny) py qy)
             vz (if (pos? nz) pz qz)]
         (if (pos? (mm/madd nx vx ny vy nz vz w))
           (reduced :outside)
           (let [vx (if (pos? nx) qx px)
                 vy (if (pos? ny) qy py)
                 vz (if (pos? nz) qz pz)]
             (if (pos? (mm/madd nx vx ny vy nz vz w))
               :intersect
               res)))))
     :inside planes)))

(defn intersect-sphere-sphere?
  ([{p1 :p r1 :r} {p2 :p r2 :r}]
   (intersect-sphere-sphere? p1 r1 p2 r2))
  ([p1 r1 p2 r2]
   (<= (g/dist-squared p1 p2) (mm/addm r1 r2 r1 r2))))

(defn intersect-ray-sphere?
  ([{rp :p dir :dir} {p :p r :r}]
   (intersect-ray-sphere? rp dir p r))
  ([rp dir p r]
   (let [q (m/- p rp)
         ds (m/mag-squared q)
         v (- (m/dot q dir))
         d (mm/msub r r (- ds (* v v)))]
     (if (>= d 0.0)
       (let [d (Math/sqrt d)
             a (+ v d)
             b (- v d)]
         (if-not (and (< a 0) (< b 0))
           (if (and (> a 0) (> b 0))
             (if (> a b) [b a] [a b])
             (if (> b 0) [b a] [a b]))
           [a b]))))))

;; Source: http://geomalgorithms.com/a06-_intersect-2.html

(defn intersect-ray-triangle3?
  [p d a b c]
  (let [u (m/- b a)
        v (m/- c a)
        n (m/cross u v)]
    (if (m/delta= V3 n)
      {:type :degenerate}
      (let [w0 (m/- p a)
            a' (- (m/dot n w0))
            b' (m/dot n d)]
        (if (m/delta= 0.0 b')
          (if (m/delta= 0.0 a')
            {:type :same-plane}
            {:type :no-intersect})
          (let [r (/ a' b')]
            (if (< r 0.0)
              {:type :no-intersect}
              (let [i (m/madd d r p)
                    [u v w] (gu/triangle-barycentric-coords a b c i u v)]
                (if (and (>= u 0.0) (>= w 0.0) (m/in-range? 0.0 1.0 v))
                  {:type :intersect :p i}
                  {:type :no-intersect :p i})))))))))

;; Source:
;; http://fileadmin.cs.lth.se/cs/Personal/Tomas_Akenine-Moller/code/tribox3.txt

(defn- triaabb-axis-test
  [pa1 pb1 pa2 pb2 a b fa fb sa sb]
  (let [q (mm/madd a pa1 b pb1)
        r (mm/madd a pa2 b pb2)
        [min max] (if (< q r) [q r] [r q])
        rad (mm/madd fa sa fb sb)]
    (if (<= min rad) (>= max (- rad)))))

(defn- triaabb-edge-test-e0
  [[ax ay az :as a] [bx by bz :as b] [cx cy cz] [ex ey ez :as e] [sx sy sz]]
  (let [[fx fy fz] (m/abs e)]
    (if (triaabb-axis-test ay az cy cz ez (- ey) fz fy sy sz)
      (if (triaabb-axis-test ax az cx cz (- ez) ex fz fx sx sz)
        (triaabb-axis-test bx by cx cy ey (- ex) fy fx sx sy)))))

(defn- triaabb-edge-test-e1
  [[ax ay az] [bx by bz :as b] [cx cy cz :as c] [ex ey ez :as e] [sx sy sz]]
  (let [[fx fy fz] (m/abs e)]
    (if (triaabb-axis-test ay az cy cz ez ey fz fy sy sz)
      (if (triaabb-axis-test ax az cx cz (- ez) ex fz fx sx sz)
        (triaabb-axis-test ax ay bx by ey (- ex) fy fx sx sy)))))

(defn- triaabb-edge-test-e2
  [[ax ay az :as a] [bx by bz] [cx cy cz :as c] [ex ey ez :as e] [sx sy sz]]
  (let [[fx fy fz] (m/abs e)]
    (if (triaabb-axis-test ay az by bz ez (- ey) fz fy sy sz)
      (if (triaabb-axis-test ax az bx bz (- ez) ex fz fx sx sz)
        (triaabb-axis-test bx by cx cy ey (- ex) fy fx sx sy)))))

(defn- triaabb-edge-minmax
  [a b c s]
  (if (< (min (min a b) c) s)
    (>= (max (max a b) c) (- s))))

(defn intersect-triangle3-aabb?
  [a b c p s]
  (let [[sx sy sz :as s] (m/* s 0.5)
        p (m/+ p s)
        [ax ay az :as a] (m/- a p)
        [bx by bz :as b] (m/- b p)
        [cx cy cz :as c] (m/- c p)
        e0 (m/- b a)
        e1 (m/- c b)]
    (if (triaabb-edge-test-e0 a b c e0 s)
      (if (triaabb-edge-test-e1 a b c e1 s)
        (if (triaabb-edge-test-e2 a b c (m/- a c) s)
          (if (triaabb-edge-minmax ax bx cx sx)
            (if (triaabb-edge-minmax ay by cy sy)
              (if (triaabb-edge-minmax az bz cz sz)
                (intersect-plane-aabb? a (m/cross e0 e1) s)))))))))

(defn intersect-line2-line2?
  [[px1 py1 :as p] [qx1 qy1 :as q]
   [px2 py2 :as lp] [qx2 qy2 :as lq]]
  (let [dx1 (- qx1 px1), dy1 (- qy1 py1)
        dx2 (- qx2 px2), dy2 (- qy2 py2)
        dx12 (- px1 px2), dy12 (- py1 py2)
        denom (mm/msub dy2 dx1 dx2 dy1)
        na (mm/msub dx2 dy12 dy2 dx12)
        nb (mm/msub dx1 dy12 dy1 dx12)]
    (if (zero? denom)
      (if (and (zero? na) (zero? nb))
        (let [ip (gu/closest-point-on-segment lp p q)
              iq (gu/closest-point-on-segment lq p q)]
          (if (or (m/delta= ip lp) (m/delta= iq lq))
            {:type :coincident :p ip :q iq}
            {:type :coincident-no-intersect :p ip :q iq}))
        {:type :parallel})
      (let [ua (/ na denom)
            ub (/ nb denom)
            i (m/mix p q ua)]
        (if (and (>= ua 0.0) (<= ua 1.0) (>= ub 0.0) (<= ub 1.0))
          {:type :intersect :p i :ua ua :ub ub}
          {:type :intersect-outside :p i :ua ua :ub ub})))))

(defn intersect-line2-edges?
  [rp rq edges]
  (->> edges
       (transduce
        (comp
         (map (fn [[p q]] (intersect-line2-line2? rp rq p q)))
         (filter #(if (get % :p) (get #{:intersect :coincident} (get % :type)))))
        (completing
         (fn [closest isec]
           (let [p (get isec :p)
                 d (g/dist-squared rp p)]
             (if (< d (closest 1)) [p d] closest))))
        [nil m/INF+])
       (first)))

(defn intersect-ray2-edges?
  [rp rd edges]
  (intersect-line2-edges? rp (m/madd rd 1e29 rp) edges))

(defn intersect-plane-plane?
  [an aw bn bw]
  (when-not (or (m/delta= aw bw *eps*) (m/delta= an bn *eps*))
    (let [od (m/dot an bn)
          det (/ (mm/madd od od -1))
          u (* (mm/madd bw od aw) det)
          v (* (mm/madd aw od bw) det)]
      {:p (m/madd an u (m/* bn v)) :dir (gu/ortho-normal an bn)})))

(defn intersect-ray-plane?
  [rp dir n w]
  (let [dp (m/dot n dir)
        dn (+ (m/dot n rp) w)]
    (if (m/delta= 0.0 dp *eps*)
      {:type (if (zero? dn) :on-plane :parallel)}
      (let [du (- (/ dn dp))
            i (m/madd dir du rp)]
        (if (m/in-range? 0.0 1.0 du)
          {:type :intersect :p i}
          {:type :intersect-outside :p i})))))

;; Source:
;; http://fileadmin.cs.lth.se/cs/Personal/Tomas_Akenine-Moller/code/tribox3.txt

(defn intersect-plane-aabb?
  "Takes a point & normal defining a plane and 2 vectors defining an
  AABB (min-p & size). Returns true if plane intersects box."
  ([p n q s]
   (let [s2 (m/* s 0.5)]
     (intersect-plane-aabb? (m/- p (m/+ q s2)) n s2)))
  ([[px py pz] [nx ny nz :as n] [sx sy sz]]
   (let [[vx1 vx2] (if (pos? nx) [(- (- sx) px) (- sx px)] [(- sx px) (- (- sx) px)])
         [vy1 vy2] (if (pos? ny) [(- (- sy) py) (- sy py)] [(- sy py) (- (- sy) py)])
         [vz1 vz2] (if (pos? nz) [(- (- sz) pz) (- sz pz)] [(- sz pz) (- (- sz) pz)])]
     (if (<= (mm/madd nx vx1 ny vy1 nz vz1) 0.0)
       (>= (mm/madd nx vx2 ny vy2 nz vz2) 0.0)))))

(defn intersect-plane-sphere?
  [n w p r] (<= (m/abs* (+ (m/dot n p) w)) r))

;; Tetrahedron intersection
;; https://gist.github.com/postspectacular/9021724
;; http://vcg.isti.cnr.it/Publications/2003/GPR03/fast_tetrahedron_tetrahedron_overlap_algorithm.pdf

(defn- subdot
  "Computes sum((a-b)*c), where a, b, c are 3D vectors."
  [a b c] (let [d (m/subm a b c)] (mm/add (d 0) (d 1) (d 2))))

(defn- face-a
  "Takes a transformation fn and the 4 delta vectors between tetra1/tetra2.
    Returns 2-elem vec of [bitmask affine-coords]."
  [f deltas]
  (let [aff (mapv f deltas)]
    [(mm/bitmask pos? (aff 0) (aff 1) (aff 2) (aff 3)) aff]))

(defn- face-b1?
  "Takes the 4 delta vectors between tetra2/tetra1 and a normal.
    Returns true if all dot products are positive."
  [deltas n] (every? #(pos? (m/dot % n)) deltas))

(defn- face-b2?
  "Like face-b1?, but optimized for last face of tetrahedron."
  [verts refv n] (every? #(pos? (subdot % refv n)) verts))

(defn- edge-a
  "Takes 2 bitmasks and edge flags, returns true if there's a
    separating plane between the faces shared by that edge."
  [ma mb ea eb]
  (let [xa (bit-and ma (bit-xor ma mb))
        xb (bit-and mb (bit-xor xa mb))
        edge (fn [a b i j]
               (let [cp (mm/msub (ea i) (eb j) (ea j) (eb i))]
                 (or (and (pos? cp) (pos? (bit-or xa a)) (pos? (bit-or xb b)))
                     (and (neg? cp) (pos? (bit-or xa b)) (pos? (bit-or xb a))))))]
    (not
     (or
      (not= 15 (bit-or ma mb))
      (edge 1 2 1 0)
      (edge 1 4 2 0)
      (edge 1 8 3 0)
      (edge 2 4 2 1)
      (edge 2 8 3 1)
      (edge 4 8 3 2)))))

(defn- get-edge
  "Lazy edge evaluation. Takes a vector of edges, vector of edge
    points and an edge id. Looks up edge for given id and if not yet
    present constructs it. Returns 2-elem vector of [edges edge]."
  [edges epoints id]
  (let [e (edges id)]
    (if e
      [edges e]
      (let [ep (epoints id), e (m/- (ep 0) (ep 1))]
        [(assoc edges id e) e]))))

(defn- check-faces-a
  "Takes the 4 delta vectors between the two tetras, edge definitions
    of the 1st tetra, vertices of the 2nd, a reference point of the 1st
    and a seq of specs, each encoding a specific check (either calls to
    face-a* or edge-a). Returns vector of bitmasks or nil if fail early."
  [deltas epoints verts p specs]
  (loop [masks [], affine [], edges [nil nil nil nil nil], s specs]
    (if s
      (let [[f a b] (first s)]
        (if (or (= :f f) (= :f* f))
          (let [[edges ea] (get-edge edges epoints a)
                [edges eb] (get-edge edges epoints b)
                n (m/cross ea eb)
                [m a] (if (= :f f)
                        (face-a #(m/dot % n) deltas)
                        (face-a #(subdot % p n) verts))]
            (if (< m 15)
              (recur (conj masks m) (conj affine a) edges (next s))))
          (if-not (edge-a (masks a) (masks b) (affine a) (affine b))
            (recur masks affine edges (next s)))))
      masks)))

(defn- check-faces-b
  "Much like check-faces-a, but for 2nd tetra and specs encoding calls to face-b1/2?.
    Returns true if tetras do intersect."
  [deltas epoints verts p specs]
  (loop [edges [nil nil nil nil nil], s specs]
    (if s
      (let [[f a b] (first s)
            [edges ea] (get-edge edges epoints a)
            [edges eb] (get-edge edges epoints b)]
        (if-not (if (= :f f)
                  (face-b1? deltas (m/cross ea eb))
                  (face-b2? verts p (m/cross ea eb)))
          (recur edges (next s))))
      true)))

(defn intersect-tetrahedra?
  "Takes 2 seqs of 4 3D points, each defining a tetrahedron. Returns
    true if they intersect. Orientation of points is irrelevant (unlike
    in the original algorithm this implementation is based on)."
  [[pa pb pc pd :as p] [qa qb qc qd :as q]]
  (let [masks (check-faces-a
               (map #(m/- % pa) q)
               [[pb pa] [pc pa] [pd pa] [pc pb] [pd pb]]
               q pb [[:f 0 1] [:f 2 0] [:e 0 1] [:f 1 2]
                     [:e 0 2] [:e 1 2] [:f* 4 3] [:e 0 3]
                     [:e 1 3] [:e 2 3]])]
    (if masks
      (or (not= 15 (reduce bit-or masks))
          (check-faces-b
           (map #(m/- % qa) p)
           [[qb qa] [qc qa] [qd qa] [qc qb] [qd qb]]
           p qb [[:f 0 1] [:f 2 0] [:f 1 2] [:f* 4 3]])))))
