(defproject taapost "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.12.0"]
                 [scicloj/tablecloth "7.029.2" :exclusions [org.clojure/tools.reader
                                                            com.taoensso/timbre
                                                            com.taoensso/encore
                                                            com.taoensso/truss]]
                 [org.dhatim/fastexcel-reader "0.12.8" :exclusions [org.apache.poi/poi-ooxml]]
                 [roz/roz  "b88bc3617d467254f37c4914c91b41aab734f0cb"
                  :exclusions [com.taoennso/timbre com.taoennso/encore]]
                 [org.scicloj/tableplot "1-beta9.1"]
                 [org.clojure/tools.reader "1.3.6"]
                 [com.taoensso/timbre "6.6.1"]
                 [com.taoensso/encore  "3.128.0"]
                 [com.taoensso/truss "1.12.0"]
                 ;;we should be picking this up but aren't....
                 [batik-rasterize "0.1.2"]
                 [spork "0.2.1.8-SNAPSHOT"
                  :exclusions [org.clojure/tools.reader]]]
  :jvm-opts ^:replace ["-Xmx4g" "-XX:NewSize=200m"]
  :plugins [[reifyhealth/lein-git-down "0.4.1"]]
  :middleware [lein-git-down.plugin/inject-properties]
  :repositories [["public-github" {:url "git://github.com"}]]
  :git-down {roz  {:coordinates  joinr/roz}})


