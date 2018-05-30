(defn run-all-in-dir
  "Loads (and executes) all .clj files in given directory."
  [dir]
  (doseq [f (filter #(.endsWith (.getName %) ".clj") (file-seq (java.io.File. dir)))]
    (load-file (.getAbsolutePath f))))
