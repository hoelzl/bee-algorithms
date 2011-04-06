(ns beeAlgorithms.core
  (:gen-class)
  (:use (incanter core stats charts
		  [distributions :exclude [variance mean]])))


(def ^{:doc "The number of bees that are generated."}

     *number-of-bees* 100)

(def ^{:doc "The end time at which the experiment stops.

  Conceptually this is the time in minutes, although nothing in the
  simulation really depends on the time scale."}

     *end-time* 120)

(def ^{:doc "The size in which we increment the time.

  The total number of steps in an experiment is (* *end-time*
  *number-of-bees*)."}
     
     *time-step* 0.1)

(def ^{:doc "The total temperature all bees can generate in a single
  time step."}

     *total-bee-temp-delta* 5.0)
       

(def ^{:doc "The maximum external temperature.

  More precisely the factor by which we multiply the external driver
  function; since all drivers take values between -1 and 1 this
  corresponds to the absolute value of maximal and minimal
  temperature."}

     *max-external-temperature* 5.0)

(def $pi java.lang.Math/PI)

(defn scaled-sin-2
  "For positive arguments a sinus function whose argument is scaled by
  half, for negative values 0."
  [x]
  (if (< x 0)
    0.0
    (let [scaled-x (/ x 2)]
      (sin scaled-x))))

(defn accelerated-sin-2
  "Constantly 0 for negative values, a sinus function with
  quadratically accelerating period for positive values."
  [x]
  (if (< x 0)
    0.0
    (let [scaled-x (/ x $pi 15.0)]
      (sin (* $pi scaled-x scaled-x)))))

(defn accelerated-sin-3
  "Constantly 0 for negative values, a sinus function whose period
  accelerates in the third power for positive values."
  [x]
  (if (< x 0)
    0.0
    (let [scaled-x (/ x $pi 10.0)]
      (sin (* 0.3 $pi scaled-x scaled-x scaled-x)))))

(defn accelerated-sin-exp
  "Constantly 0 for negative values, a sinus function with
  exponentially accelerating period for positive values."
  [x]
  (if (< x 0)
    0.0
    (let [scaled-x (/ x $pi 10.0)]
      (sin (- (exp scaled-x) 1)))))

(defn make-external-temperature-fun
  "Returns a function that returns the same value as driver-fun
  multiplied by max."
  [max driver-fun]
  (fn [t]
    (* max (driver-fun t))))

(defn single-bee-temp-delta
  "Returns the temperature difference a bee can generate in a single
  simulation step."
  
  []
  (* *total-bee-temp* *time-step*))


(defn make-bees [start-cooling-distribution
		 stop-cooling-distribution
		 number-of-bees
		 delta-temp]
  (for [i (range number-of-bees)]
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
       :previous-action (atom ::none)})))

(defn make-example
  [& {:keys [start-cooling-distribution
	     stop-cooling-distribution
	     external-temperature
	     number-of-bees
	     delta-temp
	     end-time
	     time-step
	     title],
      :or {start-cooling-distribution (normal-distribution 2.0 1.0)
	   stop-cooling-distribution (normal-distribution 0.0 0.1)
	   external-temperature (make-external-temperature-fun
				 *max-external-temperature* accelerated-sin-2)
	   number-of-bees *number-of-bees*
	   delta-temp (/ (single-bee-temp-delta) *number-of-bees*)
	   end-time *end-time*
	   time-step *time-step*
	   title "Unnamed Example"}}]
  {:start-cooling-disribution start-cooling-distribution
   :stop-cooling-distribution stop-cooling-distribution
   :external-temperature external-temperature
   :number-of-bees number-of-bees
   :bees (make-bees start-cooling-distribution
		    stop-cooling-distribution
		    number-of-bees
		    delta-temp)
   :delta-temp delta-temp
   :end-time end-time
   :time-step time-step
   :time-seq (range 0 end-time time-step)
   :title title})

(def $default-example (make-example :start-cooling-distribution (normal-distribution 1.0 0.5)
				    :stop-cooling-distribution (normal-distribution 0.5 0.25)
				    :title "Normal Distribution"))

(def $tight-example (make-example :start-cooling-distribution (normal-distribution 1.0 0.25)
				  :stop-cooling-distribution (normal-distribution 0.5 0.125)
				  :title "Tight Normal Distribution"))

(def $loose-example (make-example :start-cooling-distribution (normal-distribution 1.0 1.0)
				  :stop-cooling-distribution (normal-distribution 0.5 0.5)
				  :title "Loose Normal Distribution"))

(def $fixed-example (make-example :start-cooling-distribution [1.0]
				  :stop-cooling-distribution [0.5]
				  :title "Fixed Distribution"))

(def $tight-symmetric-random-example (make-example :start-cooling-distribution (normal-distribution 1.0 0.1)
						   :stop-cooling-distribution (normal-distribution 0.0 0.1)
						   :title "Tight Symmetric Normal Distribution"))

(def $loose-symmetric-random-example (make-example :start-cooling-distribution (normal-distribution 1.0 0.5)
						   :stop-cooling-distribution (normal-distribution 0.0 0.5)
						   :title "Loose Symmetric Normal Distribution"))

(def $symmetric-fixed-example (make-example :start-cooling-distribution [1.0]
					    :stop-cooling-distribution [0.0]
					    :title "Fixed Distribution"))

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

(defn plot-result [& {:keys [example
                             times
                             temperatures
                             title]}]
  (let [example (or example $default-example)
        times (or times (:time-seq example))
        temperatures (or temperatures (map (:external-temperature example) times))
        title (or title (:title example))]
    (xy-plot times temperatures
     	     :title title 
     	     :x-label "Time (min)" :y-label "Temperature (°C)")))

(defn run-example [example]
  (let [plot (plot-result :example example)
        bees (:bees example)
        times (:time-seq example)
        delta-t-env  (delta-t-seq (map (:external-temperature example) times))
        result-temp (result-temperatures 0.0 delta-t-env bees)]
    ;; (println delta-t-env)
    ;; (println result-temp)
    (add-lines plot times result-temp)
    (view plot)))

(defn -main [& args]
  (run-example $default-example)
  (run-example $tight-example)
  (run-example $loose-example)
  (run-example $fixed-example))

