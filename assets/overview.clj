(require '[clojure.java.io :as io])
(require '[clojure.string :as str])

(def root "/Users/toxi/work/clj/thing/geom/babel/src/thi/ng/geom")

(defn dir-contents
  [pred tx dir]
  (->> (file-seq (java.io.File. dir))
       rest
       (filter pred)
       (map tx)))

(defn parse-ns
  [^java.io.File f]
  (let [src (first (line-seq (io/reader f)))]
    (re-find #"thi\.ng\.geom\.[a-z0-9\.]+" src)))

(->> root
     (dir-contents #(.isDirectory %) identity)
     (into {}
       (map
        (fn [m]
          [(.getName m)
           (dir-contents #(re-find #"\.clj*" (.getName %)) parse-ns (.getAbsolutePath m))])))
     (pprint))
