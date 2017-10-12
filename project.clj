(defproject propel "0.1.0"
  :description "Tom Helmuth's small Push implementation in Clojure."
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]]
  :main ^:skip-aot propel.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
