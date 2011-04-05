(ns beeAlgorithms.core
  (:gen-class)
  (:use (incanter core stats charts
		  [distributions :exclude [variance mean]])))

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

(defn make-external-temperature-fun [max driver-fun]
  (fn [t]
    (* max (driver-fun t))))

(def external-temperature
     (make-external-temperature-fun 5.0 accelerated-sin-exp))


(def *number-of-bees* 10)
(def *bees* (atom []))

(defn make-bees [& {:keys [start-cooling-distribution stop-cooling-distribution
			   n change],
		    :or {start-cooling-distribution [1.0],
			 stop-cooling-distribution [0.0],
			 n *number-of-bees*,
			 change (/ 1 *number-of-bees*)}}]
  (let [bees (for [i (range n)]
	       (let [start-cooling (abs (draw start-cooling-distribution))
		     stop-cooling (min (draw stop-cooling-distribution) (- start-cooling 0.5))
		     start-heating (- start-cooling)
		     stop-heating (- stop-cooling)]
		 {:type ::bee,
		  :start-cooling start-cooling
		  :stop-cooling stop-cooling
		  :start-heating start-heating
		  :stop-heating stop-heating
		  :change change
		  :previous-action (atom ::none)}))]
    (swap! *bees* (fn [atom] bees))
    bees))

(defn bee-action [bee temperature]
  (let [previous-action (:previous-action bee)
	previous-action-value @previous-action]
    ;; Using this function bees cannot transition directly from
    ;; heating to cooling and vice versa.  Not sure whether this is correct.
    (cond (= previous-action-value ::cooling)
	  (if (< temperature (:stop-cooling bee))
	    (do
	      (swap! previous-action (fn [action] ::none))
	      0.0)
	    (- (:change bee)))
	  (= previous-action-value ::heating)
	  (if (> temperature (:stop-heating bee))
	    (do
	      (swap! previous-action (fn [action] ::none))
	      0.0)
	    (:change bee))
	  (= previous-action-value ::none)
	  (cond (>= temperature (:start-cooling bee))
		(do
		  (swap! previous-action (fn [action] ::cooling))
		  (- (:change bee)))
		(<= temperature (:start-heating bee))
		(do
		  (swap! previous-action (fn [action] ::heating))
		  (:change bee))
		:else 0.0))))


(defn bee-actions [bees temperature]
  (sum (map #(bee-action % temperature) bees)))

(defn with-delay
  ([sequence]
     (cons (first sequence) sequence))
  ([sequence n]
     (if (zero? n)
       sequence
       (recur (with-delay sequence) (- n 1)))))

(def *time-seq-step-size* 1)

(defn time-seq []
  (range 0 120 *time-seq-step-size*))

(defn delta-t-seq [external-temperature-seq]
  (map - external-temperature-seq (with-delay external-temperature-seq)))

(defn result-temperatures [current-t delta-t-env bees]
  (let [current-delta-t-env (first delta-t-env)
	current-delta-t-bees (bee-actions bees current-t)
	next-t (+ current-t current-delta-t-env current-delta-t-bees)]
    (println [current-t current-delta-t-env current-delta-t-bees])
    (lazy-seq
     (cons current-t
	   (let [new-delta-t-env (rest delta-t-env)]
		 (if (seq new-delta-t-env)
		   (result-temperatures next-t new-delta-t-env bees)
		   []))))))

(defn plot-result [& {time :time, temperature :temperature,
		      :or {time (time-seq),
			   temperature (map external-temperature time)}}]
  (xy-plot time temperature
	   :x-label "Time (min)" :y-label "Temperature (°C)"))

(defn -main [& args]
  (let [time (time-seq)
	temp (map external-temperature time)
	plot (plot-result :time time :temperature temp)
	bees (make-bees)
	delta-t-env  (delta-t-seq temp)
	result-temp (result-temperatures 0.0 delta-t-env bees)]
    ;; (println delta-t-env)
    ;; (println result-temp)
    (add-lines plot time result-temp)
    (view plot)))
