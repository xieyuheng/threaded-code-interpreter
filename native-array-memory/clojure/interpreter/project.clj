(defproject interpreter "0.1.0-SNAPSHOT"
  :description "threaded code interpreter in clojure"
  :url "http://github.com/cicada-language/threaded-code-interpreter"
  :license {:name "MIT License"}
  :dependencies [[org.clojure/clojure "1.8.0"]]
  :main ^:skip-aot interpreter.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
