(ns com.evocomputing.test.rincanter
  (:import (org.rosuda.REngine REXP RList REXPGenericVector
                               REXPInteger REXPDouble REXPString
                               RFactor REXPFactor
                               REngineException REXPMismatchException)
           (org.rosuda.REngine.JRI JRIEngine)
           (org.rosuda.JRI RMainLoopCallbacks))
  (:use (clojure.contrib core test-is))
  (:use (incanter core))
  (:use (com.evocomputing rincanter)))


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

