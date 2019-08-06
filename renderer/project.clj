(defproject render "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [uncomplicate/fluokitten "0.9.1"]]
  :java-source-paths ["src/render/comp/tree"]
  :main ^:skip-aot render.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
