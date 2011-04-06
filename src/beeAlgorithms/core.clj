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

(def ^{:doc "Well, π, obviously."}
     $pi java.lang.Math/PI)

(def ^{:doc "The empty queue."}
     $empty-queue clojure.lang.PersistentQueue/EMPTY)

(defn make-queue
  "Make a queue of the given size containing initial-element.
  "
  [size initial-element]
  (nth (nth (iterate (fn [[q v]] [(conj q v) v]) [$empty-queue initial-element]) size) 0))

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
    (let [scaled-x (/ x $pi 25.0)]
      (sin (* $pi scaled-x scaled-x)))))

(defn accelerated-sin-15
  "Constantly 0 for negative values, a sinus function with
  quadratically accelerating period for positive values."
  [x]
  (if (< x 0)
    0.0
    (let [scaled-x (/ x $pi 10.0)]
      (sin (* $pi (pow scaled-x 1.5))))))

(defn accelerated-sin-3
  "Constantly 0 for negative values, a sinus function whose period
  accelerates in the third power for positive values."
  [x]
  (if (< x 0)
    0.0
    (let [scaled-x (/ x $pi 15.0)]
      (sin (* 0.3 $pi scaled-x scaled-x scaled-x)))))

(defn accelerated-sin-exp
  "Constantly 0 for negative values, a sinus function with
  exponentially accelerating period for positive values."
  [x]
  (if (< x 0)
    0.0
    (let [scaled-x (/ x $pi 15.0)]
      (sin (- (exp scaled-x) 1)))))

(defn make-external-temperature-fun
  "Returns a function that returns the same value as driver-fun
  multiplied by max."
  [max driver-fun]
  (fn [t]
    (* max (driver-fun t))))

(defn single-bee-temp-delta-per-step
  "Returns the temperature difference a bee can generate in a single
  simulation step."
  []
  (* *total-bee-temp-delta* *time-step*))


(defn make-bees
  "Return a lazy sequence of number-of-bees bees with values for
  start-cooling, stop-cooling and delta-temp taken from the respective
  distributions.

  The value of start-cooling is always the absolute value of the value
  drawn from the distribution, stop-cooling may be negative.  The
  values of start-heating and stop-heating are always the negations of
  start-cooling and stop-cooling.
  "
  [number-of-bees
   start-cooling-distribution
   stop-cooling-distribution
   delta-temp-distribution]
  (for [i (range number-of-bees)]
    (let [start-cooling (abs (draw start-cooling-distribution))
	  stop-cooling (min (draw stop-cooling-distribution) start-cooling)
	  delta-temp (draw delta-temp-distribution)
	  start-heating (- start-cooling)
	  stop-heating (- stop-cooling)]
      {:type ::bee
       :index i
       :start-cooling start-cooling
       :stop-cooling stop-cooling
       :start-heating start-heating
       :stop-heating stop-heating
       :delta-temp delta-temp
       :previous-action (atom ::none)})))

(defn make-experiment
  "Returns a new experiment.
  "
  [& {:keys [number-of-bees
	     start-cooling-distribution
	     stop-cooling-distribution
	     delta-temp-distribution
	     external-temperature
	     delay
	     end-time
	     time-step
	     title],
      :or {number-of-bees *number-of-bees*
	   start-cooling-distribution (normal-distribution 2.0 1.0)
	   stop-cooling-distribution (normal-distribution 0.0 0.1)
	   delta-temp-distribution [(/ (single-bee-temp-delta-per-step) *number-of-bees*)]
	   external-temperature (make-external-temperature-fun
				 *max-external-temperature* accelerated-sin-15)
	   delay 0
	   end-time *end-time*
	   time-step *time-step*
	   title "Unnamed Experiment"}}]
  {:start-cooling-disribution start-cooling-distribution
   :stop-cooling-distribution stop-cooling-distribution
   :external-temperature external-temperature
   :number-of-bees number-of-bees
   :bees (make-bees number-of-bees
		    start-cooling-distribution
		    stop-cooling-distribution
		    delta-temp-distribution)
   :delta-temp-distribution delta-temp-distribution
   :delay delay
   :end-time end-time
   :time-step time-step
   :time-seq (range 0 end-time time-step)
   :title title})

(def $default-experiment
     (make-experiment
      :start-cooling-distribution (normal-distribution 1.0 0.5)
      :stop-cooling-distribution (normal-distribution 0.5 0.25)
      :title "Normal Distribution"))

(def $tight-experiment
     (make-experiment
      :start-cooling-distribution (normal-distribution 1.0 0.25)
      :stop-cooling-distribution (normal-distribution 0.5 0.125)
      :title "Tight Normal Distribution"))

(def $extra-tight-experiment
     (make-experiment
      :start-cooling-distribution (normal-distribution 1.0 0.025)
      :stop-cooling-distribution (normal-distribution 0.5 0.0125)
      :title "Extra-Tight Normal Distribution"))

(def $loose-experiment
     (make-experiment
      :start-cooling-distribution (normal-distribution 1.0 1.0)
      :stop-cooling-distribution (normal-distribution 0.5 0.5)
      :title "Loose Normal Distribution"))

(def $fixed-experiment
     (make-experiment
      :start-cooling-distribution [1.0]
      :stop-cooling-distribution [0.5]
      :title "Fixed Distribution"))

(def $standard-experiments
     [$default-experiment $tight-experiment
      $extra-tight-experiment $loose-experiment
      $fixed-experiment])

(def $tight-symmetric-random-experiment
     (make-experiment
      :start-cooling-distribution (normal-distribution 1.0 0.1)
      :stop-cooling-distribution (normal-distribution -1.0 0.1)
      :title "Tight Symmetric Normal Distribution"))

(def $extra-tight-symmetric-random-experiment
     (make-experiment
      :start-cooling-distribution (normal-distribution 1.0 0.1)
      :stop-cooling-distribution (normal-distribution -1.0 0.1)
      :title "Extra-Tight Symmetric Normal Distribution"))

(def $loose-symmetric-random-experiment
     (make-experiment
      :start-cooling-distribution (normal-distribution 1.0 0.5)
      :stop-cooling-distribution (normal-distribution -1.0 0.5)
      :title "Loose Symmetric Normal Distribution"))

(def $symmetric-fixed-experiment
     (make-experiment
      :start-cooling-distribution [1.0]
      :stop-cooling-distribution [-1.0]
      :title "Symmetric Fixed Distribution"))

(def $symmetric-experiments
     [$tight-symmetric-random-experiment, $extra-tight-symmetric-random-experiment,
      $loose-symmetric-random-experiment, $symmetric-fixed-experiment])
      

(def $tight-zero-random-experiment
     (make-experiment
      :start-cooling-distribution (normal-distribution 0.0 0.1)
      :stop-cooling-distribution (normal-distribution 0.0 0.1)
      :title "Tight Normal Distribution at 0.0"))

(def $extra-tight-zero-random-experiment
     (make-experiment
      :start-cooling-distribution (normal-distribution 0.0 0.01)
      :stop-cooling-distribution (normal-distribution 0.0 0.01)
      :title "Extra-Tight Normal Distribution at 0.0"))

(def $loose-zero-random-experiment
     (make-experiment
      :start-cooling-distribution (normal-distribution 0.0 0.5)
      :stop-cooling-distribution (normal-distribution 0.0 0.5)
      :title "Loose Normal Distribution at 0.0"))

(def $zero-fixed-experiment
     (make-experiment
      :start-cooling-distribution [0.0]
      :stop-cooling-distribution [0.0]
      :title "Fixed Distribution at 0.0"))

(def $zero-experiments
     [$tight-zero-random-experiment, $extra-tight-zero-random-experiment,
      $loose-zero-random-experiment, $zero-fixed-experiment])


(def $default-experiments
     [$default-experiment $loose-experiment
      $fixed-experiment
      $loose-symmetric-random-experiment $symmetric-fixed-experiment
      $loose-zero-random-experiment $zero-fixed-experiment])


(defn bee-action
  "Selects the action performed by a bee at each simulation step.

  The action for each step depends on the previous action: if the bee
  was previously heating or cooling and the current temperature does
  not indicate that this should stop (i.e., if it is below
  stop-cooling or above stop-heating) we continue with the previous
  action.  This implements a simple kind of hysterisis, except when
  the start and stop temperatures are equal.  If the bee was
  previously doing nothing and the temperature is above start-cooling
  the bee starts heating, if the temperature is below start-heating it
  starts heating.  Otherwise it continues to do nothing.

  Note that if (<= start-cooling temperature start-heating) the bee
  prefers cooling over heating; for bees created with make-bees such a
  specification can only happen when
  (= start-cooling temperature end-cooling 0.0).
  "
  [bee temperature]
  (let [previous-action (:previous-action bee)
	previous-action-value @previous-action]
    (cond (= previous-action-value ::cooling)
	  (if (< temperature (:stop-cooling bee))
	    (if (<= temperature (:start-heating bee))
	      (do
		(swap! previous-action (fn [action] ::heating))
		(:delta-temp bee))
	      (do
		(swap! previous-action (fn [action] ::none))
		0.0))
	    (- (:delta-temp bee)))
	  (= previous-action-value ::heating)
	  (if (> temperature (:stop-heating bee))
	    (if (>= temperature (:start-cooling bee))
	      (do
		(swap! previous-action (fn [action] ::cooling))
		(- (:delta-temp bee)))
	      (do
		(swap! previous-action (fn [action] ::none))
		0.0))
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


(defn bee-actions
  "Perform bee-action at temperature for all bees.
  "
  [bees temperature]
  (sum (map #(bee-action % temperature) bees)))

(defn duplicate-initial-element
  "Prepend the first element to sequence once or n times.
  "
  ([sequence]
     (cons (first sequence) sequence))
  ([n sequence]
     (if (zero? n)
       sequence
       (concat (repeat n (first sequence)) sequence))))

(defn delta-seq
  "Generates the sequence consisting of the differences between
  consecutive elements of inputs.
  "
  [inputs]
  (map - inputs (duplicate-initial-element inputs)))

(defn controlled-seq
  "Returns the sequence of temperatures that results when initial-temp
  is influenced by env-deltas and controlled by bees.  The
  transmission of the true environment temperature to the bees can be
  delayed by delay steps.  In other words, if (> delay 0), the bees
  try to control the environment using outdated measurements.
  "
  ([initial-temp env-deltas bees]
     (controlled-seq 0 initial-temp env-deltas bees))
  ([delay initial-temp env-deltas bees]
     (loop [env-temps [initial-temp]
	    env-temp initial-temp
	    delayed-env-temp initial-temp
	    env-deltas env-deltas
	    delayed-env-deltas (concat (repeat delay 0.0) env-deltas)
	    delayed-bee-deltas (make-queue delay 0.0)]
       (if (seq env-deltas)
	 (let [env-delta (first env-deltas)
	       bee-delta (bee-actions bees delayed-env-temp)
	       new-env-temp (+ env-temp env-delta bee-delta)
	       new-delayed-bee-deltas (conj delayed-bee-deltas bee-delta)
	       new-delayed-env-temp (+ delayed-env-temp
				       (first delayed-env-deltas)
				       (first new-delayed-bee-deltas))]
	   (recur (conj env-temps new-env-temp)
		  new-env-temp
		  new-delayed-env-temp
		  (rest env-deltas)
		  (rest delayed-env-deltas)
		  (rest new-delayed-bee-deltas)))
	 env-temps))))

;; (defn test-experiment []
;;   (let [experiment $default-experiment
;;         bees (:bees experiment)
;;         times (:time-seq experiment)
;;         delta-temp-env  (delta-seq (map (:external-temperature experiment) times))]
;;     (controlled-seq 0.0 delta-temp-env bees)))

(defn plot-environment-temp
  "Returns a plot containing the title of the experiment, the
  environment temperature and labels for the axes.
  "
  [experiment]
  (let [experiment (or experiment $default-experiment)
        times (:time-seq experiment)
        temperatures (map (:external-temperature experiment) times)
        title (:title experiment)]
    (xy-plot times temperatures
     	     :title title 
     	     :x-label "Time (min)" :y-label "Temperature (°C)")))

(defn run-experiment
  "Runs an experiment and shows the resulting plot.  If the modify
  parameter is supplied the experiment is modified by adding the
  key/value pairs of modify before the experiment is executed.  Beware
  that modification does not re-create the bees even if some parameter
  influencing their properties has changed.
  "
  [experiment & {:keys [modify] :or {modify {}}}]
  (let [experiment (conj experiment modify)
	plot (plot-environment-temp experiment)
        bees (:bees experiment)
        times (:time-seq experiment)
        delta-temp-env  (delta-seq (map (:external-temperature experiment) times))
        result-temp (controlled-seq (:delay experiment) 0.0 delta-temp-env bees)]
    ;; (println delta-temp-env)
    ;; (println result-temp)
    (add-lines plot times result-temp)
    (view plot)))

(defn run-experiments
  "Run all experiments by applying run-experiment.
  "
  [experiments & {:keys [modify] :or {modify {}}}]
  (map #(run-experiment % :modify modify) experiments))

(defn -main [& args]
  (run-experiments $default-experiments))
