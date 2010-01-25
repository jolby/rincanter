(ns com.evocomputing.test.rincanter
  (:import (org.rosuda.REngine REXP RList REXPGenericVector
                               REXPInteger REXPDouble REXPString
                               RFactor REXPFactor
                               REngineException REXPMismatchException)
           (org.rosuda.REngine.JRI JRIEngine)
           (org.rosuda.JRI RMainLoopCallbacks))
  (:use (clojure test))
  (:use (incanter core))
  (:use (com.evocomputing rincanter)))

(deftest can-connect-to-R
  (is (not (= nil get-jri-engine))))

(deftest pass-through-data
  (with-r-eval
    "data(iris)"
    ;;starts off an R dataframe, turns into an incanter dataset
    (is (= (type (from-r (r-get "iris"))) :incanter.core/dataset))))

(deftest pass-through-equivalence
  (with-r-eval
    "data(iris)"
    ;;ds is now an incanter dataset, let's convert back
    ;;and ensure we didn't lose information
    (r-set! "irisds" (to-r (from-r (r-get "iris"))))
    (is (r-true (from-r (r-eval "identical(irisds, iris)"))))))

;; (defn test-with-r-eval []
;;   (with-r-eval
;;     "data(iris)"
;;     (r-get "iris")
;;     "a = 1:10"
;;     (r-get "a")))

;; (defn macexp-with-r-eval []
;;      (macroexpand-1
;;       '(with-r-eval
;;          "data(iris)"
;;          (r-get "iris")
;;          "a = 1:10"
;;          (r-get "a"))))

