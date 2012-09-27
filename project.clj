(defproject com.evocomputing/rincanter
  "1.0.0-SNAPSHOT"
  :description "Clojure/R integration using rosuda JRIEngine"
  :dependencies [[org.clojure/clojure "1.4.0"]
                 [org.clojure/tools.logging "0.2.3"]
                 [org.clojure/core.incubator "0.1.2"]
                 [incanter "1.3.0"]
                 [REngine/REngine "0.5-5"]
                 [JRIEngine/JRIEngine "0.5-5"]]
  :repositories {"snapshots" "http://nexus:8081/nexus/content/repositories/snapshots"
                 "releases"  "http://nexus:8081/nexus/content/repositories/releases"}
  :autodoc {:name "rincanter"
            :description "Clojure/R integration using rosuda JRIEngine"
            :page-title "Rincanter API documentation"
            :copyright "Eclipse Public License (EPL)"
            :web-src-dir "http://github.com/jolby/rincanter/blob/"
            :web-home "http://jolby.github.com/rincanter/"})