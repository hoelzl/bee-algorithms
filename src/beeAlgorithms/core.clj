(ns beeAlgorithms.core
  (:gen-class)
  (:use (incanter core stats charts)))

(def $pi java.lang.Math/PI)

(defn accelerated-sin-2 [x]
  (let [scaled-x (/ x $pi 10.0)]
    (sin (* $pi scaled-x scaled-x))))

(defn accelerated-sin-3 [x]
  (let [scaled-x (/ x $pi 10.0)]
    (sin (* 0.3 $pi scaled-x scaled-x scaled-x))))

(defn accelerated-sin-exp [x]
  (let [scaled-x (/ x $pi 10)]
    (sin (- (exp scaled-x) 1))))


(defn -main [& args]
  (view (function-plot accelerated-sin-2 0 120.0))
  (view (function-plot accelerated-sin-3 0 120.0))
  (view (function-plot accelerated-sin-exp 0 120.0))
)
