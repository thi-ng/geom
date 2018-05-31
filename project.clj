(defproject thi.ng/geom "1.0.0-RC2"
  :description  "thi.ng geometry kit - meta project spec including all modules"
  :url          "https://github.com/thi-ng/geom"
  :license      {:name "Apache Software License"
                 :url  "http://www.apache.org/licenses/LICENSE-2.0"
                 :distribution :repo}
  :scm          {:name "git"
                 :url  "https://github.com/thi-ng/geom"}

  :min-lein-version "2.4.0"

  :dependencies  [[org.clojure/clojure "1.9.0"]
                  [org.clojure/clojurescript "1.10.238"]
                  [thi.ng/color "1.2.0"]
                  [thi.ng/dstruct "0.2.1"]
                  [thi.ng/math "0.2.1"]
                  [thi.ng/ndarray "0.3.2"]
                  [thi.ng/shadergraph "0.3.0"]
                  [thi.ng/strf "0.2.2"]
                  [thi.ng/typedarrays "0.1.6"]
                  [thi.ng/xerror "0.1.0"]
                  [org.jogamp.gluegen/gluegen-rt "2.3.2"]
                  [org.jogamp.jogl/jogl-all "2.3.2"]
                  [cljs-log "0.2.2"]
                  [hiccup "1.0.5"]]

  :perforate    {:environments [{:namespaces [thi.ng.geom.bench.core.vector]}]}

  :profiles     {:dev   {:dependencies      [[criterium "0.4.4"]]
                         :plugins           [[lein-cljsbuild "1.1.7"]
                                             [com.cemerick/clojurescript.test "0.3.3"]]
                         :node-dependencies [[benchmark "1.0.0"]]
                         :global-vars       {*warn-on-reflection* true}
                         :jvm-opts          ^:replace ["-Dclojure.compiler.direct-linking=false"]
                         :aliases           {"cleantest" ["do" "clean," "test," "cljsbuild" "test"]
                                             "bench" ["with-profile" "bench" "do" "clean," "perforate," "cljsbuild" "test"]}}
                 :bench {:dependencies [[perforate-x "0.1.0"]]
                         :plugins      [[perforate "0.3.4"]
                                        [lein-npm "0.6.2"]]
                         :cljsbuild
                         {:builds
                          [{:id             "bench"
                            :source-paths   ["src" "test" "benchmarks"]
                            :notify-command ["node" "target/cljs/benchmark.js"]
                            :compiler       {:target       :nodejs
                                             :output-to     "target/cljs/benchmark.js"
                                             :optimizations :simple
                                             :pretty-print  true}}]
                          :test-commands {"unit-tests" ["phantomjs" :runner "target/geom.js"]}}}}

  :cljsbuild    {:builds [{:id           "simple"
                           :source-paths ["src" "test" "examples/gl"]
                           :compiler     {:output-to     "target/geom.js"
                                          :optimizations :whitespace
                                          :pretty-print  true}}
                          {:source-paths ["src" "examples/gl"]
                           :id           "prod"
                           :compiler     {:output-to      "target/geom.js"
                                          :optimizations  :advanced
                                          :pretty-print   false}}]
                 :test-commands {"unit-tests" ["phantomjs" :runner "target/geom.js"]}}

  :pom-addition [:developers
                 [:developer
                  [:name "Karsten Schmidt"]
                  [:url "http://thi.ng"]
                  [:timezone "0"]]])
