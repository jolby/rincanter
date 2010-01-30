;; by Joel Boehland http://github.com/jolby/rincanter
;; January 24, 2010

;; Copyright (c) Joel Boehland, 2010. All rights reserved.  The use
;; and distribution terms for this software are covered by the Eclipse
;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this
;; distribution.  By using this software in any fashion, you are
;; agreeing to be bound by the terms of this license.  You must not
;; remove this notice, or any other, from this software.


(ns #^{:doc
       "Rincanter is a Clojure/incanter wrapper around the rosuda
JRIEngine Java/R bridge. The hope is to allow easy access to an
embedded R engine from incanter. It also offers conversion between
Clojure/incanter datatypes and R datatypes such as R dataframe to
incanter dataset."
       :author "Joel Boehland"}

  com.evocomputing.rincanter
  (:import (org.rosuda.REngine REXP RList REXPGenericVector
                               REXPInteger REXPDouble REXPString REXPLogical
                               RFactor REXPFactor
                               REngineException REXPMismatchException)
           (org.rosuda.REngine.JRI JRIEngine)
           (org.rosuda.JRI RMainLoopCallbacks))
  (:use (clojure.contrib core def))
  (:use (incanter core))
  (:use (com.evocomputing.rincanter convert)))

(defonce *jri-engine* (ref nil))

(defn init-jri-engine
  "Initialize the jri engine. This can only be done once in the
lifetime of the jvm-- not our rules- that's how the embedded R engine
seems to want things."
  ([] (init-jri-engine nil))
  ([handler]
     ;;check for pre-existing jri-engine
     (when @*jri-engine*
       (throw (IllegalStateException.
               "JRIEngine can only be initialized once per lifetime of the VM.")))
     ;;Tell Rengine to not die if it can't load the JRI native lib
     ;;This allows us to catch and deal with this situation ourselves
     (System/setProperty "jri.ignore.ule" "yes")
     (dosync
      (ref-set *jri-engine*
               {:jri (JRIEngine. (into-array ["--vanilla" "--quiet"]) handler)}))))

(defn get-jri-engine
  "Get the jri-engine. If it doesn't exist, create it using the
  default init-jri-engine function and return it."
  []
  (if @*jri-engine*
    (@*jri-engine* :jri)
    (do (init-jri-engine)
        (if @*jri-engine*
          (@*jri-engine* :jri)
          (throw (IllegalStateException.
                  "Unable to initialize JRIEngine"))))))

(defn r-eval-no-catch
  "Eval expression in the R engine. Will not catch any exceptions that
  happen during evaluation"
  [expression]
  (let [r (get-jri-engine)]
    (.parseAndEval r expression)))

(defn r-eval-raw
  "Eval expression in the R engine. Just return the raw JRI/R wrapper,
  don't convert to Clojure object"
  [expression]
  (let [r (get-jri-engine)]
    (try
     (.parseAndEval r expression)
     (catch REngineException ex
       (println (format "Caught exception evaluating expression: %s\n: %s" expression ex)))
     (catch REXPMismatchException ex
       (println (format "Caught exception evaluating expression: %s\n: %s" expression ex))))))

(defn r-eval
  "Eval expression in the R engine. Convert the return value from
  JRI/R to Clojure"
  [expression]
  (from-r (r-eval-raw expression)))

(defn r-try-parse-eval
  "Eval expression in the R engine, wrapped (on the R side) in
try/catch. Will catch errors on the R side and convert to Exception
and throw"
  [expression]
  (let [r (get-jri-engine)]
    (try
     (.assign r ".tmp." expression)
     (let [rexp (.parseAndEval r "try(eval(parse(text=.tmp.)),silent=TRUE)")]
       (if (.inherits rexp "try-error")
         (throw (Exception.
                 (format "Error in R evaluating expression:\n %s.\nR exception: %s"
                         expression (.asString rexp))))
         rexp))
     (catch REngineException ex
       (println (format "Caught exception evaluating expression: %s\n: %s" expression ex)))
     (catch REXPMismatchException ex
       (println (format "Caught exception evaluating expression: %s\n: %s" expression ex))))))

(defmacro with-r-eval-no-catch
  "Evaluate forms that are string using r-eval-no-catch, otherwise, just eval
clojure code normally"
  [& forms]
  `(do ~@(map #(if (string? %) (list 'r-eval-no-catch %) %) forms)))

(defmacro with-r-eval-raw
  "Evaluate forms that are string using r-eval-raw, otherwise, just eval
  Clojure code normally"
  [& forms]
  `(do ~@(map #(if (string? %) (list 'r-eval %) %) forms)))

(defmacro with-r-eval
  "Evaluate forms that are string using r-eval, otherwise, just eval
  Clojure code normally"
  [& forms]
  `(do ~@(map #(if (string? %) (list 'r-eval %) %) forms)))

(defmacro with-r-try-parse-eval
  "Evaluate forms that are string using r-try-parse-eval, otherwise
  just eval Clojure code normally"
  [& forms]
  `(do ~@(map #(if (string? %) (list 'r-try-parse-eval %) %) forms)))

(defn r-set!
 "Assign r-name to val within the R engine"
 [r-name val]
  (let [r (get-jri-engine)]
    (try
     (.assign r r-name val)
     (catch REngineException ex
       (println (format "Caught exception assigning R val: %s\n: %s" r-name ex))))))

(defn r-get-raw
  "Retrieve the value with this name in the R engine. Do not convert
  from JRI to Clojure type."
  [r-name]
  (r-eval-raw r-name))

(defn r-get
  "Retrieve the value with this name in the R engine"
  [r-name]
  (r-eval r-name))

(defn r-inspect
  "Runs str(object) on the R side, capturing console output. Runs
  println on returned Strings"
  [obj]
  (if (string? obj)
    (dorun (map #'println  (r-eval (format "capture.output(str(%s))" obj))))
    (do
      (r-set! ".printtmp." (if (instance? REXP obj) obj (to-r obj)))
      (dorun (map #'println (r-eval "capture.output(str(.printtmp.))"))))))

(defn r-install-CRAN
  "Tries to install the provided package using the optionally provided
repository or the master CRAN repository"
  ([package]
     (dorun (map #'println
                 (r-eval (format "capture.output(install.packages(\"%s\", repos=\"http://cran.r-project.org\"))" package)))))
  ([package repo]
     (dorun (map #'println
                 (r-eval (format "capture.output(install.packages(\"%s\", repos=\"%s\"))" package repo))))))

;;
;;Inspection, typechecking and print methods
;;===========================================================================
;;
(defmethod print-method org.rosuda.REngine.REXP [o w]
  (.write w (str "#=(org.rosuda.REngine.REXP. " (.toString o) ")")))
