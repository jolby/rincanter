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
       "Convert between the rosuda Java/R wrapper types and Clojure types"
       :author "Joel Boehland"}

  com.evocomputing.rincanter.convert
  (:import (org.rosuda.REngine REXP RList REXPGenericVector
                               REXPInteger REXPDouble REXPString REXPLogical
                               RFactor REXPFactor
                               REngineException REXPMismatchException)
           (org.rosuda.REngine.JRI JRIEngine)
           (org.rosuda.JRI RMainLoopCallbacks))
  (:use (clojure.contrib core def
                         [seq-utils :only [flatten]]))
  (:use (incanter core)))

(declare from-r)
(declare to-r)

(defn r-attr-names
  "Return a seq of attributes, or nil if the REXP has no attributes"
  [rexp]
  (when (.isInstance org.rosuda.REngine.REXP rexp)
    (if-let [names (._attr rexp)]
      (vec (.names (.asList names))))))

(defn r-attr
  "Returns the attribute with the passed in name, or nil."
  [rexp attr-name]
  (when (.isInstance org.rosuda.REngine.REXP rexp)
    (.getAttribute rexp attr-name)))

(defn r-atts
  "Returns a map of the R object's attribute-names -> attributes, or nil."
  [rexp]
  (when (.isInstance org.rosuda.REngine.REXP rexp)
    (into {} (map #(vec (list % (from-r (r-attr rexp %)))) (r-attr-names rexp)))))

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

(defmacro def-from-r
  "Define a from-r method using standard boilerplate"
  [rexp type & convert-forms]
    `(defmethod from-r ~type
       [~rexp]
       (with-meta
         (vec ~@convert-forms)
         {:r-type ~type})))

(def-from-r rexp REXPLogical (.isTrue rexp))
(def-from-r rexp REXPInteger (.asIntegers rexp))
(def-from-r rexp REXPDouble (.asDoubles rexp))
(def-from-r rexp REXPString (.asStrings rexp))
(def-from-r rexp RList (map #(from-r %) rexp))

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
