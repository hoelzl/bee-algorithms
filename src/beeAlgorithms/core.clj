(ns beeAlgorithms.core
  (:gen-class)
  (:use (incanter core stats charts)))

(def $pi java.lang.Math/PI)

(defn accelerated-sin-2 [x]
  (if (< x 0)
    0.0
    (let [scaled-x (/ x $pi 10.0)]
      (sin (* $pi scaled-x scaled-x)))))

(defn accelerated-sin-3 [x]
  (if (< x 0)
    0.0
    (let [scaled-x (/ x $pi 10.0)]
      (sin (* 0.3 $pi scaled-x scaled-x scaled-x)))))

(defn accelerated-sin-exp [x]
  (if (< x 0)
    0.0
    (let [scaled-x (/ x $pi 10)]
      (sin (- (exp scaled-x) 1)))))

(defn external-temperature [t]
  (* 5 (accelerated-sin-exp t)))

(defn with-delay [sequence]
  (cons (first sequence) sequence))

(defn draw-result [& {time :time, temperature :temperature,
		      :or {time (range 0 120 0.02),
			   temperature (map external-temperature time)}}]
  (view (xy-plot time temperature
		 :x-label "Time (min)" :y-label "Temperature (°C)")))

(defn -main [& args]
  (let [t (range 0 120 0.01)
	temp (map external-temperature t)]
    (draw-result :time t :temperature temp))
  ; (draw-result)
)
