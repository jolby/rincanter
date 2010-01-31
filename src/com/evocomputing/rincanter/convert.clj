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
  (:import (org.rosuda.REngine REXPNull REXP RList REXPList REXPVector REXPGenericVector
                               REXPInteger REXPDouble REXPString REXPLogical
                               RFactor REXPFactor
                               REngineException REXPMismatchException)
           (org.rosuda.REngine.JRI JRIEngine)
           (org.rosuda.JRI RMainLoopCallbacks))
  (:use (clojure.contrib core))
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

(defn r-has-attr?
  "Returns true if the attribute with the passed in name exists, or false."
  [rexp attr-name]
  (if (and (.isInstance org.rosuda.REngine.REXP rexp)
           (.hasAttribute rexp attr-name))
    true
    false))

(defn r-atts
  "Returns a map of the R object's attribute-names -> attributes, or nil."
  [rexp]
  (when (.isInstance org.rosuda.REngine.REXP rexp)
    (into {} (map #(vec (list % (from-r (r-attr rexp %)))) (r-attr-names rexp)))))

(defn r-atts-raw
  "Returns the JRI object representing the R attributes for this object
 or nil if there are no attributes"
  [rexp]
  (when (.isInstance org.rosuda.REngine.REXP rexp)
    (._attr rexp)))

(defn r-map-to-atts
  "Converts a map into the nested list form that rosuda JRI seems to
want in its constructors"
  [attr-map]
  (.REXPList (RList.
              (into-array REXP (map #(to-r %) (vals attr-map))
              (into-array String (keys attr-map))))))

(defn r-true
  "Most R variables are collections. This tests a sequence for
equality with 1 or boolean true. Returns true if all equal 1 or true, false otherwise"
  [seq]
  (every? #(or (= 1 %) (true? %))  seq))

;;
;; Converting R types <--> Clojure types
;;===========================================================================
;;
(defn dataframe?
  "Returns true if the passed in object is an R dataframe, false
otherwise"
  [rexp]
  (if (some #{"data.frame"}
            (-?> rexp (r-attr "class") .asStrings))
    true
    false))

(defn from-r-dispatch
  [rexp]
  (cond
   (nil? rexp) ::nil
   (dataframe? rexp) ::dataframe
   true (class rexp)))

(defmulti from-r
"Convert the rosuda JRI/R type to a matching Clojure type"
  from-r-dispatch)

(defmacro def-from-r
  "Define a from-r method using standard boilerplate"
  [rexp type & convert-forms]
    `(defmethod from-r ~type
       [~rexp]
       (with-meta
         ~@convert-forms
         {:r-type ~type :r-atts (r-atts-raw ~rexp)})))

(def-from-r rexp REXPLogical (vec (.isTrue rexp)))
(def-from-r rexp REXPInteger (vec (.asIntegers rexp)))
(def-from-r rexp REXPDouble (vec (.asDoubles rexp)))
(def-from-r rexp REXPString (vec (.asStrings rexp)))
(def-from-r rexp RList (vec (map #(from-r %) rexp)))
(def-from-r rexp REXPGenericVector (from-r (.asList rexp)))

(defn r-factor-to-categorical-var
  [rfactor]
  (categorical-var
   :data (vec (.asIntegers rfactor))
   :labels (vec (.levels rfactor))
   :levels (vec (map #(.levelIndex rfactor %) (.levels rfactor)))))

(defmethod from-r REXPFactor
  [rexp]
  (with-meta (vec (.asIntegers (.asFactor rexp)))
    {:r-type REXPFactor
     :r-atts (r-atts-raw rexp)
     :category-variable (r-factor-to-categorical-var (.asFactor rexp))}))

(defmethod from-r RFactor
  [rexp]
  (with-meta (vec (.asIntegers rexp))
    {:r-type REXPFactor ;;Not a mistake- use the higher level
     ;;REXPFactor interface on the return trip
     :r-atts (r-atts-raw rexp)
     :category-variable (r-factor-to-categorical-var rexp)}))

;;catch-all
(defmethod from-r REXP [rexp] rexp)

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
    (with-meta dataset (merge (meta dataset)
                              {:col-meta col-meta
                               :r-type ::dataframe
                               :r-atts (r-atts-raw dataframe)}))))
(defmethod from-r REXPNull
  [rexp]
  nil)

(defmethod from-r ::nil
  [rexp]
  nil)

(def *object-type-to-primitive-type-map*
     {java.lang.Byte Byte/TYPE
      java.lang.Integer Integer/TYPE
      java.lang.Float Float/TYPE
      java.lang.Double Double/TYPE})

(defn prefer-primitive-type
  "If possible, ensure array is one of the primitive types. Convert if neccessary"
  [class]
  (or (*object-type-to-primitive-type-map* class) class))

(defn to-r-dispatch
  [obj]
  (cond
   (nil? obj) ::nil
   (.isInstance org.rosuda.REngine.REXP obj) ::rexp
   (and (meta obj)
        (:r-type (meta obj))) (:r-type (meta obj))
   (#{"[B" "[Ljava.lang.Byte;"} (.getName (class obj))) ::byte-array
   (#{"[I" "[Ljava.lang.Integer;"} (.getName (class obj))) ::int-array
   (#{"[D" "[Ljava.lang.Double;"} (.getName (class obj))) ::double-array
   (= "[Ljava.lang.String;" (.getName (class obj))) ::string-array
   ;;These must go after the array tests or infinite recursion will
   ;;take place
   (seq? obj) ::seq
   (seqable? obj) ::seqable
   true (class obj)))

(defmulti to-r
"Convert the Clojure type to the proper rosuda JRI/R type"
  to-r-dispatch)

(defmethod to-r ::rexp
  [obj]
  ;;This obj is already converted to one of the JRI types
  ;;just return it.
  obj)

(defmethod to-r ::seq
  [obj]
  (let [type (prefer-primitive-type (.getClass (first obj)))]
  (to-r (into-array type obj))))

(defmethod to-r ::seqable
  [obj]
  (to-r (seq obj)))

(defmethod to-r REXPInteger
  [obj]
  (REXPInteger. (int-array obj) (r-atts-raw obj)))

(defmethod to-r ::int-array
  [obj]
  (REXPInteger. obj (r-atts-raw obj)))

(defmethod to-r REXPLogical
  [obj]
  (REXPLogical. (byte-array (map #'byte obj)) (r-atts-raw obj)))

(defmethod to-r ::byte-array
  [obj]
  (REXPLogical. obj (r-atts-raw obj)))

(defmethod to-r REXPDouble
  [obj]
  (REXPDouble. (double-array obj) (r-atts-raw obj)))

(defmethod to-r ::double-array
  [obj]
  (REXPDouble. obj (r-atts-raw obj)))

(defmethod to-r REXPString
  [obj]
  (REXPString. (into-array String obj) (r-atts-raw obj)))

(defmethod to-r ::string-array
  [obj]
  (REXPString. (into-array String obj) (r-atts-raw obj)))

(defmethod to-r REXPFactor
  [obj]
  (let [ids (int-array obj)
        levels (:labels (:category-variable (meta obj)))]
    (REXPFactor. (int-array obj)
               (into-array (:labels (:category-variable (meta obj))))
               (:r-atts (meta obj)))))

(defmethod to-r org.rosuda.REngine.RList
  [obj]
  (map #(to-r %) obj))

(defmethod to-r REXPGenericVector
  [obj]
  (map #(to-r %) obj))

(defmethod to-r ::dataframe
  [dataset]
  (with-data dataset
    (let [names (into-array String (col-names dataset))
          col-meta (:col-meta (meta dataset))
          cols (into [] (map #(let [col ($ %)] (with-meta col (col-meta (aget names %))))
                             (range (alength names))))
          colarr (into-array REXP (map #(to-r %) cols))]
      (REXPGenericVector.
       (RList. colarr names)
       (:r-atts (meta dataset))))))

(defmethod to-r ::nil
  [obj]
  nil)
