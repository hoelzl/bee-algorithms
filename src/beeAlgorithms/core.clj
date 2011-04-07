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

  This also determines which percentage of the *total-bee-temp-delta*
  the bees apply in a single step.  The total number of steps in an
  experiment is (* *end-time* *number-of-bees*)."}
     
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

;;; Forward declaration so that we have all parameters visible here.
(declare ^{:doc "The default action performed by bees."}
	 *default-bee-action-fun*)

(def ^{:doc "The width of the generated plots."}
     *plot-width* 1200)

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

(defn simple-hysterisis-bee-action
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

(def *default-bee-action-fun* simple-hysterisis-bee-action)

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
   delta-temp-distribution
   action-fun-distribution]
  (for [i (range number-of-bees)]
    (let [start-cooling (abs (draw start-cooling-distribution))
	  stop-cooling (min (draw stop-cooling-distribution) start-cooling)]
      {:type ::bee
       :index i
       :start-cooling start-cooling
       :stop-cooling stop-cooling
       :start-heating (- start-cooling)
       :stop-heating (- stop-cooling)
       :delta-temp (draw delta-temp-distribution)
       :action-fun (draw action-fun-distribution)
       :previous-action (atom ::none)})))

(defn make-experiment
  "Returns a new experiment.
  "
  [& {:keys [number-of-bees
	     start-cooling-distribution
	     stop-cooling-distribution
	     delta-temp-distribution
	     bee-action-fun-distribution
	     external-temperature
	     delay
	     end-time
	     time-step
	     title],
      :or {number-of-bees *number-of-bees*
	   start-cooling-distribution (normal-distribution 2.0 1.0)
	   stop-cooling-distribution (normal-distribution 0.0 0.1)
	   delta-temp-distribution [(/ (single-bee-temp-delta-per-step)
				       *number-of-bees*)]
	   bee-action-fun-distribution [*default-bee-action-fun*]
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
		    delta-temp-distribution
		    bee-action-fun-distribution)
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
      :start-cooling-distribution (normal-distribution 1.0 0.2)
      :stop-cooling-distribution (normal-distribution -1.0 0.2)
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
      :start-cooling-distribution (normal-distribution 0.0 0.2)
      :stop-cooling-distribution (normal-distribution 0.0 0.2)
      :title "Tight Normal Distribution at 0.0"))

(def $extra-tight-zero-random-experiment
     (make-experiment
      :start-cooling-distribution (normal-distribution 0.0 0.1)
      :stop-cooling-distribution (normal-distribution 0.0 0.1)
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
     [$fixed-experiment
      $default-experiment $loose-experiment
      $symmetric-fixed-experiment $loose-symmetric-random-experiment
      $zero-fixed-experiment $loose-zero-random-experiment])

(defn perform-bee-actions
  "Perform bee-action at temperature for all bees.
  "
  [bees temperature]
  (sum (map #((:action-fun %) % temperature) bees)))

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
	       bee-delta (perform-bee-actions bees delayed-env-temp)
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
  [experiment & {:keys [title] :or {title false}}]
  (let [experiment (or experiment $default-experiment)
        times (:time-seq experiment)
        temperatures (map (:external-temperature experiment) times)
        title (or title (:title experiment))]
    (xy-plot times temperatures
     	     :title title 
     	     :x-label "Time (min)" :y-label "Temperature (°C)")))

(defn add-experiment-plot
  "Adds an experiment to an existing plot."
  [experiment plot]
  (println "Adding plot for" (:title experiment))

  (let [bees (:bees experiment)
        times (:time-seq experiment)
        delta-temp-env  (delta-seq (map (:external-temperature experiment) times))
        result-temp (controlled-seq (:delay experiment) 0.0 delta-temp-env bees)]
    (add-lines plot times result-temp)))

(defn run-experiment
  "Runs an experiment and shows the resulting plot.  If the modify
  parameter is supplied the experiment is modified by adding the
  key/value pairs of modify before the experiment is executed.  Beware
  that modification does not re-create the bees even if some parameter
  influencing their properties has changed.
  "
  [experiment & {:keys [modify] :or {modify {}}}]
  (let [experiment (conj experiment modify)
	plot (plot-environment-temp experiment)]
    (add-experiment-plot experiment plot)
    (view plot :width *plot-width*)))

(defn run-experiments
  "Run all experiments by applying run-experiment.
  "
  [experiments & {:keys [modify, title, single-plot?]
		  :or {modify {}
		       title false
		       single-plot? false}}]
  (let [experiments (map #(conj % modify) experiments)]
    (if (and (seq experiments) single-plot?)
      (let [plot (plot-environment-temp (first experiments) :title title)]
	(dorun (map #(add-experiment-plot % plot) experiments))
	(view plot :width *plot-width*))
      (map #(run-experiment %)
	   (reverse experiments)))))

(defn single-plot-1 []
  (run-experiments [$fixed-experiment
		    $default-experiment
		    $loose-experiment]
		   :single-plot? true
		   :title "Simple Hysterisis"))

(defn single-plot-2 []
  (run-experiments [$symmetric-fixed-experiment
		    $tight-symmetric-random-experiment
		    $loose-symmetric-random-experiment]
		   :single-plot? true
		   :title "Symmetric (-1, 1)"))

(defn single-plot-3 []
  (run-experiments [$zero-fixed-experiment
		    $tight-zero-random-experiment
		    $loose-zero-random-experiment]
		   :single-plot? true
		   :title "No Hysterisis (0, 0)"))

(defn -main [& args]
  (run-experiments $default-experiments)
  (single-plot-3)
  (single-plot-2)
  (single-plot-1))
