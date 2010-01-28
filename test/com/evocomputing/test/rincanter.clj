(ns com.evocomputing.test.rincanter
  (:import (org.rosuda.REngine REXP RList REXPGenericVector
                               REXPInteger REXPDouble REXPString
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
