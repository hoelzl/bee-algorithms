(ns beeAlgorithms.test.core
  (:use [beeAlgorithms.core])
  (:use [clojure.test]))

(deftest make-example-test 
  (is (= (make-example)
    {}) "Testing make-example."))
