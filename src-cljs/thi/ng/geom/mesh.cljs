(ns thi.ng.geom.mesh
  (:require
   [thi.ng.geom.triangle :as tri]
   [thi.ng.math.core :as m]
   [thi.ng.data.core :as d]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.types :as t]
   [clojure.set :as set]))

(defn add-face
  [{:keys [vertices edges faces] :as m} [a b c] & attribs]
  (let [a (get (find vertices a) 0 a)
        b (get (find vertices b) 0 b)
        c (get (find vertices c) 0 c)
        f [a b c]]
    (if (and (nil? (get faces f))
             (not (or (= a b) (= a c) (= b c))))
      (let [vertices (-> vertices
                       (d/index-kv a {:next b :prev c :f f})
                       (d/index-kv b {:next c :prev a :f f})
                       (d/index-kv c {:next a :prev b :f f}))
            edges (-> edges (d/index-kv #{a b} f) (d/index-kv #{b c} f) (d/index-kv #{c a} f))]
        (assoc m
          :vertices vertices
          :edges edges
          :faces (conj faces f)))
      m)))

(defn add-face!
  "Internal helper fn to add a face using transients of a mesh structure.
  Returns vector of new transients `[vertices edges faces]`."
  [[vertices edges faces] [a b c :as f]]
  (if (and (nil? (get faces f))
           (not (or (= a b) (= a c) (= b c))))
    (let [vertices (-> vertices
                     (d/index-kv! a {:next b :prev c :f f})
                     (d/index-kv! b {:next c :prev a :f f})
                     (d/index-kv! c {:next a :prev b :f f}))
          edges (-> edges (d/index-kv! #{a b} f) (d/index-kv! #{b c} f) (d/index-kv! #{c a} f))]
      [vertices edges (conj! faces f)])
    [vertices edges faces]))

(defn begin-edit
  [{:keys [vertices edges faces]}]
  [(transient vertices) (transient edges) (transient faces)])

(defn commit-edit
  [m [vertices edges faces]]
  (assoc m
    :vertices (persistent! vertices)
    :edges (persistent! edges)
    :faces (persistent! faces)))

(defn into-mesh
  "Adds all faces of the elements given in `more` to the mesh `m`.
  Each element of `more` can be either an existing mesh or a face list."
  [m & more]
  (let [{:keys [vertices edges faces]} m
        [v e f] (reduce
                  (fn[state m] (reduce add-face! state (if (map? m) (:faces m) m)))
                  [(transient vertices) (transient edges) (transient faces)]
                  more)]
    (assoc m
      :vertices (persistent! v)
      :edges (persistent! e)
      :faces (persistent! f))))

(defn- make-mesh
  [fctor fmix fmid]
  (t/map->Mesh
    {:vertices {} :edges {} :fnormals {} :vnormals {}
     :normals #{} :faces #{}
     :fns {:ctor fctor :mix fmix :mid fmid}}))

(defn mesh2
  "Builds a new 2d mesh data structure and (optionally) populates it with
  the given items (a seq of existing meshes and/or faces). Faces are defined
  as 3-element vectors of their vertices."
  ([] (make-mesh mesh2 g/mix2 g/mid2))
  ([& more]
    (if (map? (first more))
      (into-mesh (first more) (rest more))
      (into-mesh (mesh2) more))))

(defn mesh3
  ([] (make-mesh mesh3 g/mix3 g/mid3))
  ([& more]
    (if (map? (first more))
      (into-mesh (first more) (rest more))
      (into-mesh (mesh3) more))))

(defn valence
  [m v] (inc (count (get (:vertices m) v))))

(defn vertex-faces
  [m v] (d/value-set :f (:vertices m) v))

(defn vertex-neighbors
  [m v]
  (set/union
    (d/value-set :next (:vertices m) v)
    (d/value-set :prev (:vertices m) v)))

(defn compute-face-normals
  [{:keys [faces] :as m}]
  (let [[normals fnormals]
        (reduce
          (fn [[norms fnorms] [a b c :as f]]
            (let [[norms n] (d/index! norms
                    (-> (g/sub3 a b) (g/cross3 (g/sub3 a c)) (g/normalize3)))]
              [norms (assoc! fnorms f n)]))
          [(transient #{}) (transient {})] faces)]
    (assoc m
      :normals (persistent! normals)
      :fnormals (persistent! fnormals))))

(defn compute-vertex-normals
  [{:keys [vertices normals fnormals] :as m}]
  (let [[normals vnormals]
        (reduce
          (fn [[norms vnorms] v]
            (let [faces (vertex-faces m v)
                  n (->> faces
                      (map #(get fnormals %))
                      (reduce g/add3)
                      (g/normalize3))
                  [norms n] (d/index! norms n)]
              [norms (assoc! vnorms v n)]))
          [(transient normals) (transient {})] (keys vertices))]
    (assoc m
      :normals (persistent! normals)
      :vnormals (persistent! vnormals))))

(defn remove-face
  [{:keys [vertices edges faces fnormals vnormals] :as m} f]
  {:pre [(get faces f)]}
  (let [[vertices vnormals edges]
        (reduce
          (fn[[vertices vnormals edges] [a b]]
            (let [e #{a b} efaces (disj (get edges e) f)
                  edges (if (seq efaces)
                          (assoc edges e efaces)
                          (dissoc edges e))
                  ve (filter #(not= (:f %) f) (get vertices a))]
              (if (seq ve)
                [(assoc vertices a (into #{} ve)) vnormals edges]
                [(dissoc vertices a) (dissoc vnormals a) edges])))
          [vertices vnormals edges]
          (d/successive-nth 2 (conj f (first f))))]
    (assoc m
      :vertices vertices
      :vnormals vnormals
      :edges edges
      :faces (disj faces f)
      :fnormals (dissoc fnormals f))))

(defn remove-vertex
  [{:keys [vertices vedges faces] :as m} v]
  {:pre [(get vertices v)]}
  (reduce remove-face m (vertex-faces m v)))

(defn replace-vertex
  ([m v v2]
    (let [vfaces (vertex-faces m v)]
      (-> (reduce remove-face m vfaces)
        (replace-vertex v v2 vfaces))))
  ([m v v2 faces]
    (let [subst {v v2}]
      (reduce #(add-face % (replace subst %2)) m faces))))

(defn merge-vertices
  [{{fmid :mid} :fns :as m} a b]
  {:pre [((vertex-neighbors m a) b)]}
  (let [fa (vertex-faces m a) fb (vertex-faces m b)
        ab-isec (set/intersection fa fb)
        a-xor (set/difference fa ab-isec)
        b-xor (set/difference fb ab-isec)
        mp (fmid a b)]
    (-> (reduce remove-face m (set/union ab-isec a-xor b-xor))
      (replace-vertex a mp a-xor)
      (replace-vertex b mp b-xor))))

(defn subdivide-edge
  [{{fmix :mix} :fns :as m} a b & splits]
  {:pre [((vertex-neighbors m a) b)]}
  (let [fa (vertex-faces m a) fb (vertex-faces m b)
        faces (set/intersection fa fb)
        s-points (map #(fmix a b %) (d/wrap-seq splits 0.0 1.0))
        s-points (d/successive-nth 2 s-points)]
    (reduce
      (fn [[m newfaces] f]
        (reduce
          (fn [[m newfaces] [sa sb]]
            (let [nf (replace {a sa b sb} f)]
              [(add-face m nf) (conj newfaces nf)]))
          [m newfaces] s-points))
      [(reduce remove-face m faces) #{}]
      faces)))

(defn subdivide-face
  [{{fmix :mix} :fns :as m} [a b c :as f] p displace splits]
  (if (seq splits)
    (let [splits (d/wrap-seq splits 0.0 1.0)
          {:keys[vertices edges faces] :as m} (remove-face m f)
          edge-vertex (if displace
                        (fn[[ea eb :as e] t] (displace e (fmix ea eb t) t))
                        (fn[[ea eb] t] (fmix ea eb t)))
          [vertices edges faces]
          (reduce
            (fn [m e]
              (reduce
                (fn [m [sa sb]] (add-face! m [sa sb p]))
                m (d/successive-nth 2
                    (map (partial edge-vertex e) splits))))
            [(transient vertices) (transient edges) (transient faces)]
            [[a b] [b c] [c a]])]
      (assoc m
        :vertices (persistent! vertices)
        :edges (persistent! edges)
        :faces (persistent! faces)))
    (-> (remove-face m f)
        (add-face [a b p])
        (add-face [b c p])
        (add-face [c a p]))))

(defn subdivide-mesh
  [m & {f-vertex :vertex f-displace :displace f-filter :filter splits :splits}]
  (let [f-vertex (or f-vertex (fn[[a b c]] (g/scale3 (g/add3 a b c) m/THIRD)))]
    (reduce
      (fn[m f] (subdivide-face m f (f-vertex f) f-displace splits))
      m (if f-filter (filter f-filter (:faces m)) (:faces m)))))

(defn loop-subdivide-face
  [{{fmid :mid} :fns :as m} [a b c :as f]]
  (let [{:keys[vertices edges faces] :as m} (remove-face m f)
        [mab mbc mca] (map (fn [[p q]] (fmid p q)) [[a b] [b c] [c a]])]
    (reduce add-face m [[a mab mca] [mab b mbc] [mbc c mca] [mab mbc mca]])))

(defn loop-subdivide-mesh
  ([m] (loop-subdivide-mesh 1 m))
  ([n m] (reduce (fn [m _] (reduce loop-subdivide-face m (:faces m))) m (range n)))
  ([n f m] (reduce (fn [m _] (reduce loop-subdivide-face m (filter f (:faces m)))) m (range n))))

(defn laplacian2
  [{:keys[vnormals] :as m} amp]
  (fn [p]
    (let [neighbors (vertex-neighbors m p)]
      (g/add3 p
        (g/normalize3
          (reduce g/add3 (vnormals p)
            (map (comp #(g/scale3 % 0.5) vnormals) neighbors))
          amp)))))

(defn laplacian
  [m]
  (fn [p]
    (let [neighbors (vertex-neighbors m p)
          nc (count neighbors)]
      (if (pos? nc)
        (g/scale3 (reduce g/add3 neighbors) (/ 1.0 nc))
        p))))

(defn spherify
  [r] #(g/normalize3 % r))

(defn transform
  [{:keys [faces vertices fns]} f]
  (let [subst (into (hash-map) (map (fn[v] [v (f v)]) (keys vertices)))]
    (apply (:ctor fns)
      (map (fn[[a b c]] [(get subst a) (get subst b) (get subst c)])
        faces))))

(defn slice-with
  "Returns a 2-element vector of [mesh newfaces]."
  ([m e] (slice-with m e (:faces m)))
  ([m e faces]
    (let [verts (persistent!
                  (reduce #(-> (conj! % (%2 0)) (conj! (%2 1)) (conj! (%2 2)))
                    (transient #{}) faces))
          cverts (zipmap verts (map #(g/classify-point e %) verts))
          classifier #(get cverts %2)]
      (reduce
        (fn [[m nf :as state] f]
          (if-let [tfaces (tri/slice-with f e classifier)]
            [(reduce add-face
              (reduce add-face (remove-face m f) (get tfaces -1))
              (get tfaces 1))
             (into (into nf (get tfaces -1)) (get tfaces 1))]
            state))
        [m []] faces))))

(defn keep-faces
  ([m f] (keep-faces m f (:faces m)))
  ([m f faces]
    (reduce
      (fn [m face] (if (f face) m (remove-face m face)))
      m faces)))

; (def m
;   (mesh3
;     [[0 0 0] [100 0 0] [100 100 0]]
;     [[0 0 0] [100 100 0] [0 100 0]]
;     [[100 0 0] [200 0 0] [100 100 0]]
;     [[200 0 0] [200 100 0] [100 100 0]]
;    ))

; (def m (-> m compute-face-normals compute-vertex-normals))

;(pprint m)
