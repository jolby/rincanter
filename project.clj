(defproject com.evocomputing/rincanter
  "1.0.0-SNAPSHOT"
  :description "Clojure/R integration using rosuda JRIEngine"
  :dependencies [[org.clojure/clojure "1.2.0"]
                 [org.clojure/clojure-contrib "1.2.0"]
                 [incanter "1.2.3"]]
  :native-dependencies [[jriengine "0.8.4"]]
  :dev-dependencies [[swank-clojure "1.4.0-SNAPSHOT"]
                     [autodoc "0.7.0"]
                     [native-deps "1.0.5"]]
  :autodoc {:name "rincanter"
            :description "Clojure/R integration using rosuda JRIEngine"
            :page-title "Rincanter API documentation"
            :copyright "Eclipse Public License (EPL)"
            :web-src-dir "http://github.com/jolby/rincanter/blob/"
            :web-home "http://jolby.github.com/rincanter/"})