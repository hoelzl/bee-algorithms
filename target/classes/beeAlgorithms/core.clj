(ns beeAlgorithms.core
  (:gen-class)
  (:use (incanter core stats charts
		  [distributions :exclude [variance mean]])))


(def *number-of-bees* 100)
(def *end-time* 120)
(def *time-step* 1)

(defn make-example [& {:keys [start-cooling-distribution
			      stop-cooling-distribution
			      number-of-bees
			      delta-temp
			      end-time
			      time-step
			      title],
		       :or {start-cooling-distribution (normal-distribution 2.0 1.0),
			    stop-cooling-distribution (normal-distribution 0.0 0.1),
			    number-of-bees *number-of-bees*,
			    delta-temp (/ 1 *number-of-bees*)
			    end-time *end-time*
			    time-step *time-step*
			    title "Unnamed Example"}}]
  {:start-cooling-disribution start-cooling-distribution,
   :stop-cooling-distribution stop-cooling-distribution,
   :number-of-bees number-of-bees
   :delta-temp delta-temp
   :end-time end-time
   :time-step time-step
   :title title})

(def $default-example (make-example :start-cooling-disribution (normal-distribution 1.0 0.5)
				    :stop-cooling-distribution (normal-distribution 0.5 0.1)
				    :number-of-bees *number-of-bees*
				    :delta-temp (/ 1 *number-of-bees*)
				    :title "Normally Distributed Default Example"))

(def $fixed-example (make-example :start-cooling-disribution [1.0]
				  :stop-cooling-distribution [0.5]
				  :number-of-bees *number-of-bees*
				  :delta-temp (/ 1 *number-of-bees*)
				  :title "Fixed Distribution"))
(def $pi java.lang.Math/PI)

(defn sin-1 [x]
  (if (< x 0)
    0.0
    (let [scaled-x (/ x 2)]
      (sin scaled-x))))

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

(def *bees* (atom []))

(defn make-bees [example]
  (let [start-cooling-distribution (:start-cooling-disribution example)
	stop-cooling-distribution (:stop-cooling-distribution example)
	number-of-bees (:number-of-bees example)
	delta-temp (:delta-temp example)
	bees (for [i (range number-of-bees)]
	       (let [start-cooling (abs (draw start-cooling-distribution))
		     stop-cooling (min (draw stop-cooling-distribution) (- start-cooling 0.5))
		     start-heating (- start-cooling)
		     stop-heating (- stop-cooling)]
		 {:type ::bee,
		  :start-cooling start-cooling
		  :stop-cooling stop-cooling
		  :start-heating start-heating
		  :stop-heating stop-heating
		  :delta-temp delta-temp
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
	    (- (:delta-temp bee)))
	  (= previous-action-value ::heating)
	  (if (> temperature (:stop-heating bee))
	    (do
	      (swap! previous-action (fn [action] ::none))
	      0.0)
	    (:delta-temp bee))
	  (= previous-action-value ::none)
	  (cond (>= temperature (:start-cooling bee))
		(do
		  (swap! previous-action (fn [action] ::cooling))
		  (- (:delta-temp bee)))
		(<= temperature (:start-heating bee))
		(do
		  (swap! previous-action (fn [action] ::heating))
		  (:delta-temp bee))
		:else 0.0))))


(defn bee-actions [bees temperature]
  (sum (map #(bee-action % temperature) bees)))

(defn with-offset
  ([sequence]
     (cons (first sequence) sequence))
  ([sequence n]
     (if (zero? n)
       sequence
       (recur (with-offset sequence) (- n 1)))))

(defn time-seq [example]
  (range 0 (:end-time example)  (:time-step example)))

(defn delta-t-seq [external-temperature-seq]
  (map - external-temperature-seq (with-offset external-temperature-seq)))

(defn result-temperatures [current-t delta-t-env bees]
  (let [current-delta-t-env (first delta-t-env)
	current-delta-t-bees (bee-actions bees current-t)
	next-t (+ current-t current-delta-t-env current-delta-t-bees)]
    ;; (println [current-t current-delta-t-env current-delta-t-bees])
    (lazy-seq
     (cons current-t
	   (let [new-delta-t-env (rest delta-t-env)]
		 (if (seq new-delta-t-env)
		   (result-temperatures next-t new-delta-t-env bees)
		   []))))))

(defn plot-result [& {:keys {example
                             times
                             temperatures
                             title}}]
  (let [example (or example $default-example)
        times (or times (:times example))
        temperatures (or temperatures (map external-temperature times))
        title (or title (:title example))]
    (xy-plot times temperatures
     	     :title title 
     	     :x-label "Time (min)" :y-label "Temperature (°C)")))

(defn run-example [example]
  (let [plot (plot-result :example example)
        bees (make-bees example)
        times (:times example)
        delta-t-env  (delta-t-seq (map external-temperature example))
        result-temp (result-temperatures 0.0 delta-t-env bees)]
    ;; (println delta-t-env)
    ;; (println result-temp)
    (add-lines plot times result-temp)
    (view plot)))

(defn -main [& args]
  (run-example $default-example)
  (run-example $fixed-example))

(-main)
