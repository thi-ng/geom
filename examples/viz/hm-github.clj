;;(require '[tentacles.repos :as repos])
(require '[thi.ng.ndarray.core :as nd])
(require '[clojure.string :as str])
(require '[clojure.java.shell :refer [sh]])

(def day         (* 24 60 60 1000))
(def week        (* 7 day))
(def fmt-iso8601 (java.text.SimpleDateFormat. "yyyy-MM-dd'T'HH:mm:ssX"))
(def fmt-month   (java.text.SimpleDateFormat. "MMM"))
(def fmt-year    (java.text.SimpleDateFormat. "yyyy"))
(def ->epoch #(try (.getTime (.parse fmt-iso8601 %)) (catch Exception e)))

(defn month-or-year
  [from]
  #(let [d (java.util.Date. (long (+ from (* % week))))]
     (.format (if (zero? (.getMonth d)) fmt-year fmt-month) d)))

;; currently disabled to due CLJ 1.8 incompatibility
#_(defn load-commits-gh
    [user repo]
    (prn (str "loading GH commit history: " user "/" repo))
    (->> (repos/commits user repo {:all-pages true})
         (map #(->epoch (get-in % [:commit :author :date])))
         (filter identity)))

(defn load-commits-fs
  [repo-path]
  (->> (sh "git" "log" "--pretty=format:%aI" :dir repo-path)
       :out
       str/split-lines
       (map ->epoch)
       (filter identity)))

(defn commits-per-week-day
  [t0 commits]
  (->> (for [c commits
             :let [t (- c t0)
                   w (int (/ t week))
                   d (int (/ (rem t week) day))]]
         [w d])
       (frequencies)
       (sort-by first)))

(defn commits->matrix
  [commits]
  (let [weeks (inc (- (ffirst (last commits)) (first (ffirst commits))))
        mat (nd/ndarray :int8 (byte-array (* 7 weeks)) [7 weeks])]
    (doseq [[[w d] n] commits] (nd/set-at mat d w n))
    mat))

(let [commits   (load-commits-fs ".")
      ;;commits   (load-commits-gh "thi-ng" "geom")
      [from to] (viz/value-domain-bounds commits)
      from      (* (long (/ from week)) week)
      to        (* (inc (long (/ to week))) week)
      mat       (commits->matrix (commits-per-week-day from commits))
      weeks     (last (nd/shape mat))
      max-x     (+ 50 (* weeks 10))]
  (->> {:x-axis (viz/linear-axis
                 {:domain [0 weeks]
                  :range  [50 max-x]
                  :major  4
                  :minor  1
                  :pos    85
                  :label  (viz/default-svg-label (month-or-year from))})
        :y-axis (viz/linear-axis
                 {:domain  [0 7]
                  :range   [10 80]
                  :visible false})
        :data   [{:matrix        mat
                  :value-domain  [1 (reduce max mat)]
                  :palette       (->> :yellow-red grad/cosine-schemes (grad/cosine-gradient 100))
                  :palette-scale viz/linear-scale
                  :layout        viz/svg-heatmap
                  :shape         viz/circle-cell}]}
       (viz/svg-plot2d-cartesian)
       (svg/svg {:width (+ 70 max-x) :height 120})
       (svg/serialize)
       (spit "out/commit-history.svg")))
