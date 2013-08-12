(defproject thi.ng/geom "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]]
  :plugins [[lein-cljsbuild "0.3.2"]]
  :cljsbuild
  {:builds
   [{:source-paths ["src-cljs"]
     :id "dev"
     :compiler
     {:pretty-print true
      :output-to "resources/public/geom.js"
      :externs ["inc/angular.js"]
      :optimizations :simple}
     :jar false}
    {:source-paths ["src-cljs"]
     :id "prod"
     :compiler
     {:pretty-print false
      :output-to "resources/public/geom.min.js"
      :externs ["inc/angular.js"]
      :optimizations :advanced}
     :jar false}]})
