(defproject com.evocomputing/rincanter
  "1.0.0-SNAPSHOT"
  :description "Clojure/R integration using rosuda JRIEngine"
  :dependencies [[org.clojure/clojure "1.1.0"]
                 [org.clojure/clojure-contrib "1.0-SNAPSHOT"]
                 ;;commenting these out- they were made from my
                 ;;local install
                 ;;[org.rosuda.REngine.JRI/JRIEngine "0.5.1"]
                 ;;[org.rosuda.REngine/REngine "0.5.1"]

                 ;;this is incanter from git installed locally
                 [incanter/incanter-app "1.0-master-SNAPSHOT"]]
  :dev-dependencies [[leiningen/lein-swank "1.0.0-SNAPSHOT"]
                     [autodoc "0.7.0"]]
  :autodoc { :name "rincanter"
            :description "Clojure/R integration using rosuda JRIEngine"
            :page-title "Rincanter API documentation"
            :copyright "Eclipse Public License (EPL)"
            :web-src-dir "http://github.com/jolby/rincanter/blob/"
            :web-home "http://jolby.github.com/rincanter/"}
  ;:native-path "./native"
  )