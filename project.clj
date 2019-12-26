(defproject aoc "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.0"] [com.clojure-goes-fast/clj-async-profiler "0.1.0"]]
  :main ^:skip-aot aoc.core
  :target-path "target/%s"
  :jvm-opts ["-Djdk.attach.allowAttachSelf"]
  :profiles {:uberjar {:aot :all}})
