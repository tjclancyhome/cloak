(defproject cloak "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [com.taoensso/timbre "3.3.1"]
                 [clojure-lanterna "0.9.4"]]
  :main ^:skip-aot cloak.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
