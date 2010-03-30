(defproject com.evocomputing/rincanter
  "1.0.0-SNAPSHOT"
  :description "Clojure/R integration using rosuda JRIEngine"
  :repositories {"incanter" "http://repo.incanter.org"}
  :dependencies [[org.clojure/clojure "1.1.0"]
                 [org.clojure/clojure-contrib "1.0-SNAPSHOT"]
                 [org.incanter/incanter-full "1.2.0-SNAPSHOT"]]
  :native-dependencies [[jriengine "0.8.4"]]
  :dev-dependencies [[swank-clojure "1.1.0"]
                     [autodoc "0.7.0"]
                     [native-deps "1.0.0"]]
  :autodoc {:name "rincanter"
            :description "Clojure/R integration using rosuda JRIEngine"
            :page-title "Rincanter API documentation"
            :copyright "Eclipse Public License (EPL)"
            :web-src-dir "http://github.com/jolby/rincanter/blob/"
            :web-home "http://jolby.github.com/rincanter/"})