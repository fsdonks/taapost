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
                 [org.dhatim/fastexcel-reader "0.12.8" :exclusions [org.apache.poi/poi-ooxml org.apache.commons/commons-compress]]
                 ;;needed for docjure....
                 [commons-io/commons-io "2.11.0"]
                 [roz/roz  "b88bc3617d467254f37c4914c91b41aab734f0cb"
                  :exclusions [com.taoennso/timbre com.taoennso/encore]]
                 [org.scicloj/tableplot "1-beta9.1"]
                 [org.clojure/tools.reader "1.3.6"]
                 [com.taoensso/timbre "6.6.1"]
                 [com.taoensso/encore  "3.128.0"]
                 [com.taoensso/truss "1.12.0"]
                 ;;we should be picking this up but aren't....
                 [xerces/xercesImpl "2.12.2"]
                 [batik-rasterize "0.1.2" :exclusions [xml-apis org.clojure/clojure xerces/xerces org.apache.xmlgraphics/batik-transcoder
                                                       org.apache.xmlgraphics/batik-codec org.apache.xmlgraphics/batik-anim
                                                       org.apache.xmlgraphics/xmlgraphics-commons]]
                 [org.apache.xmlgraphics/batik-all "1.18"]
                 [spork "0.2.1.8-SNAPSHOT"
                  :exclusions [org.clojure/tools.reader]]
                 [proc  "0.3.5-SNAPSHOT"
                  :exclusions [spork]]]
  :jvm-opts ^:replace ["-Xmx4g" "-XX:NewSize=200m"]
  :plugins [[reifyhealth/lein-git-down "0.4.1"]]
  :middleware [lein-git-down.plugin/inject-properties]
  :repositories [["public-github" {:url "git://github.com"}]]
  :git-down {roz  {:coordinates  joinr/roz}})


