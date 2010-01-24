(defproject com.evocomputing/cljr
  "1.0.0-SNAPSHOT"
  :description "Clojure/R integration using rjava"
  ;;:repositories [["ubic"  "http://www.chibi.ubc.ca/maven2/"]]
  :dependencies [[org.clojure/clojure "1.1.0"]
                 [org.clojure/clojure-contrib "1.0-SNAPSHOT"]
                 [org.rosuda.REngine.JRI/JRIEngine "0.5.1"]
                 [org.rosuda.REngine/REngine "0.5.1"]
                 [incanter/incanter-app "1.0-master-SNAPSHOT"]]
  :dev-dependencies [[leiningen/lein-swank "1.0.0-SNAPSHOT"]]
  ;:native-path "./native"
  )