(ns com.evocomputing.test.rincanter
  (:import (org.rosuda.REngine REXP RList REXPGenericVector
                               REXPInteger REXPDouble REXPString REXPLogical
                               RFactor REXPFactor
                               REngineException REXPMismatchException)
           (org.rosuda.REngine.JRI JRIEngine)
           (org.rosuda.JRI RMainLoopCallbacks))
  (:use (clojure test))
  (:use (incanter core))
  (:use (com.evocomputing.rincanter convert))
  (:use (com.evocomputing rincanter)))

(deftest can-connect-to-R
  (is (not (= nil get-jri-engine))))

(deftest to-r-conversions
  (is (= REXPLogical (class (to-r (into-array Byte/TYPE (map #'byte [1 2 3]))))))
  (is (= REXPInteger (class (to-r (into-array Integer/TYPE [1 2 3])))))
  (is (= REXPDouble (class (to-r (into-array Double/TYPE [1.0 2.0 3.0]))))))

(deftest convert-dataframe-to-dataset
  (with-r-eval
    "data(iris)"
    ;;starts off an R dataframe, turns into an incanter dataset
    (is (= (type (from-r (r-get "iris"))) :incanter.core/dataset))))

(deftest pass-dataframe-through-equivalence
  (with-r-eval
    "data(iris)"
    ;;convert iris dataframe to an incanter dataset, then convert back
    ;;to an R dataframe and set it in the R environment
    (r-set! "irisds" (to-r (from-r (r-get "iris"))))
    ;;irisds is now an R dataframe it should be identical to iris dataframe
    (is (r-true (from-r (r-eval "identical(irisds, iris)"))))))
