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

(defn external-temperature [t]
  (* 5 (accelerated-sin-exp t)))

(defn with-delay [sequence]
  (cons (first sequence) sequence))

(defn -main [& args]
  (let [time (range 0 120 0.1)
        temperature (map external-temperature time)]
  (view (xy-plot time temperature)))
  ; (view (function-plot accelerated-sin-3 0 120.0))
  ; (view (function-plot accelerated-sin-exp 0 120.0))
)
