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

