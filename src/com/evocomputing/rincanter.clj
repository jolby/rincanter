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
       "Rincanter is a clojure/incanter wrapper around the rosuda
JRIEngine Java/R bridge. The hope is to allow easy access to an
embedded R engine from incanter. It also offers translation between
clojure/incanter datatypes and R datatypes such as R dataframe to
incanter dataset."
       :author "Joel Boehland"}

  com.evocomputing.rincanter
  (:import (org.rosuda.REngine REXP RList REXPGenericVector
                               REXPInteger REXPDouble REXPString REXPLogical
                               RFactor REXPFactor
                               REngineException REXPMismatchException)
           (org.rosuda.REngine.JRI JRIEngine)
           (org.rosuda.JRI RMainLoopCallbacks))
  (:use (clojure.contrib core def
                         [seq-utils :only [flatten]]))
  (:use (incanter core)))

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

(defn r-eval-raw
  "Eval expression in the R engine. Will not catch any exceptions that
  happen during evaluation"
  [expression]
  (let [r (get-jri-engine)]
    (.parseAndEval r expression)))

(defn r-eval
  "Eval expression in the R engine"
  [expression]
  (let [r (get-jri-engine)]
    (try
     (.parseAndEval r expression)
     (catch REngineException ex
       (println (format "Caught exception evaluating expression: %s\n: %s" expression ex)))
     (catch REXPMismatchException ex
       (println (format "Caught exception evaluating expression: %s\n: %s" expression ex))))))

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

(defmacro with-r-eval-raw
  "Evaluate forms that are string using r-eval, otherwise, just eval
clojure code normally"
  [& forms]
  `(do ~@(map #(if (string? %) (list 'r-eval-raw %) %) forms)))

(defmacro with-r-eval
  "Evaluate forms that are string using r-eval, otherwise, just eval
  clojure code normally"
  [& forms]
  `(do ~@(map #(if (string? %) (list 'r-eval %) %) forms)))

(defmacro with-r-try-parse-eval
  "Evaluate forms that are string using r-try-parse-eval, otherwise
  just eval clojure code normally"
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

(defn r-get
  "Retrieve the value with this name in the R engine"
  [r-name]
  (r-eval r-name))

;;
;;Inspection, typechecking and print methods
;;===========================================================================
;;
(defmethod print-method org.rosuda.REngine.REXP [o w]
  (.write w (str "#=(org.rosuda.REngine.REXP. " (.toString o) ")")))

(defn r-attr-names
  "Return a seq of attributes, or nil if the REXP has no attributes"
  [rexp]
  (if-let [names (._attr rexp)]
    (vec (.names (.asList names)))))

(defn r-attr
  "Returns the attribute with the passed in name, or nil."
  [rexp attr-name]
  (if (.isInstance org.rosuda.REngine.REXP rexp)
    (.getAttribute rexp attr-name)))

(defn r-true
  "Most R variables are collections. This tests a sequence for
equality with 1. Returns true if all equal 1, false otherwise"
  [seq]
  (every? #(= 1 %) seq))

;;
;; Converting R types <--> Clojure types
;;===========================================================================
;;
(defn dataframe?
  "Returns true if the passed in object is an R dataframe, nil
otherwise"
  [rexp]
  (if (some #{"data.frame"}
            (-?> rexp (r-attr "class") .asStrings))
    true
    nil))

(defn from-r-dispatch
  [rexp]
  (cond
   (nil? rexp) ::nil
   (dataframe? rexp) ::dataframe
   true (class rexp)))

(defmulti from-r from-r-dispatch)

(defmethod from-r REXPLogical
  [rexp]
  (with-meta
    (vec (.isTRUE rexp))
    {:r-type REXPLogical}))

(defmethod from-r REXPInteger
  [rexp]
  (with-meta
    (vec (.asIntegers rexp))
    {:r-type REXPInteger}))

(defmethod from-r REXPDouble
  [rexp]
  (with-meta
    (vec (.asDoubles rexp))
    {:r-type REXPDouble}))

(defmethod from-r REXPString
  [rexp]
  (with-meta
    (vec (.asStrings rexp))
    {:r-type REXPString}))

(defn r-factor-to-categorical-var
  [rfactor]
  (categorical-var
   :data (vec (.asIntegers rfactor))
   :labels (vec (.levels rfactor))
   :levels (vec (map #(.levelIndex rfactor %) (.levels rfactor)))))

(defmethod from-r REXPFactor
  [rexp]
  (from-r (.asFactor rexp)))

(defmethod from-r RFactor
  [rexp]
  (with-meta (vec (.asIntegers rexp))
    {:r-type REXPFactor
     :category-variable (r-factor-to-categorical-var rexp)}))

(defmethod from-r RList
  [rexp]
  (with-meta
    (vec (map #(from-r %) rexp))
    {:r-type RList}))

(defmethod from-r REXPGenericVector
  [rexp]
  (with-meta
    (from-r (.asList rexp))
    {:r-type REXPGenericVector}))

(defmethod from-r ::dataframe
  [dataframe]
  (if-not (dataframe? dataframe)
    (throw IllegalArgumentException
           "Must be R class data.frame"))
  (let [names (from-r (r-attr dataframe "names"))
        cols (from-r (.asList dataframe))
        col-meta (zipmap names (map #(meta %) cols))
        ds (partition (count names) (apply interleave cols))
        dataset (dataset names ds)]
    (with-meta dataset (merge (meta dataset) {:col-meta col-meta :r-type ::dataframe}))))

(defmethod from-r ::nil
  [rexp]
  nil)

(defn to-r-dispatch
  [obj]
  (cond
   (nil? obj) ::nil
   (and (meta obj)
        (:r-type (meta obj))) (:r-type (meta obj))
   true (class obj)))

(defmulti to-r to-r-dispatch)

(defmethod to-r REXPInteger
  [obj]
  (REXPInteger. (int-array obj)))

(defmethod to-r REXPLogical
  [obj]
  (REXPLogical. (byte-array obj)))

(defmethod to-r REXPDouble
  [obj]
  (REXPDouble. (double-array obj)))

(defmethod to-r REXPString
  [obj]
  (REXPString. (into-array obj)))

(defmethod to-r REXPFactor
  [obj]
  (let [ids (int-array obj)
        levels (:labels (:category-variable (meta obj)))]
  (REXPFactor. (int-array obj)
               (into-array (:labels (:category-variable (meta obj)))))))

(defmethod to-r org.rosuda.REngine.RList
  [obj]
  (map #(to-r %) obj))

(defmethod to-r REXPGenericVector
  [obj]
  (map #(to-r %) obj))

(defmethod to-r ::dataframe
  [dataset]
  (with-data dataset
    (let [names (col-names dataset)
          col-meta (:col-meta (meta dataset))
          cols (into [] (map #(let [col ($ %)] (with-meta col (col-meta (names %))))
                             (range (count names))))]
      (REXPGenericVector. (RList. (map #'to-r cols) (into-array names))))))

(defmethod to-r ::nil
  [obj]
  nil)
