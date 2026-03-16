;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3.1-pro
(cl:in-package cl-user)
(defpackage #:project-euler-0255 (:use cl iterate alexandria) (:export #:solve))
(in-package #:project-euler-0255)

#||
(cl:comment "CLIF logic for Project Euler 255")
(cl:text
(Constraint "Avoid O(N) evaluation by grouping identical next-states")
(Equivalence "All numbers n in an interval that map to the same ceil(n/x) and thus same x_next can be processed together")
(Optimization "Iterate directly over x_next rather than q to further merge contiguous intervals, reducing tree width by a factor of 2")
(Target "Calculate exact sum of iterations for all 10^13 <= n < 10^14 and return average rounded to 10 decimal places")
)
||#


(defun format-ratio-to-10-decimal-places (numerator denominator)
  "Formats a rational number to a string with exactly 10 decimal places, correctly rounded."
  (let* ((scaled-numerator (* numerator 10000000000))
         (rounded (round scaled-numerator denominator))
         (integer-part (floor rounded 10000000000))
         (fractional-part (mod rounded 10000000000)))
    (format nil "~D.~10,'0D" integer-part fractional-part)))

(defun total-iterations (interval-n-min interval-n-max x-current depth)
  "Recursively calculates the total number of Heron method iterations for the interval."
  (let ((sum 0)
        ;; q = ceiling(n / x) translates to integer arithmetic: floor((n + x - 1) / x)
        (q-min (floor (+ interval-n-min x-current -1) x-current))
        (q-max (floor (+ interval-n-max x-current -1) x-current)))
    
    (let ((x-next-min (ash (+ x-current q-min) -1))
          (x-next-max (ash (+ x-current q-max) -1)))
      
      ;; Iterate over unique x-next values to maximize interval merging
      (iterate (for x-next from x-next-min to x-next-max)
        
        ;; Print debug at the outermost loop to observe progress without IO bottleneck
        (when (and (= depth 0) (= (mod x-next 1000000) 0))
          (format t "debug: Processing top-level x-next = ~A / ~A~%" x-next x-next-max))
        
        ;; The valid range of q that produces this specific x-next
        (let* ((q-start (max q-min (- (ash x-next 1) x-current)))
               (q-end   (min q-max (1+ (- (ash x-next 1) x-current)))))
          
          (when (<= q-start q-end)
            ;; Calculate the precise interval of n that corresponds to these q values
            (let ((next-n-min (max interval-n-min (1+ (* (1- q-start) x-current))))
                  (next-n-max (min interval-n-max (* q-end x-current))))
              
              (when (<= next-n-min next-n-max)
                (let ((count (- next-n-max next-n-min -1)))
                  ;; Add 1 iteration cost for each number in this sub-interval
                  (incf sum count)
                  
                  ;; If we haven't reached the fixed point, recurse deeper
                  (when (/= x-next x-current)
                    (incf sum (total-iterations next-n-min next-n-max x-next (1+ depth)))))))))))
    sum))

(defun solve ()
  "Calculates the average number of iterations for a 14-digit number."
  (let* ((n-min 10000000000000)
         (n-max 99999999999999)
         (x-initial 7000000)
         (total-count (- n-max n-min -1)))
    
    (format t "debug: starting the interval based DFS for total iterations...~%")
    (let ((total-iter-sum (total-iterations n-min n-max x-initial 0)))
      (format t "debug: DFS finished. Total iterations = ~A~%" total-iter-sum)
      (format-ratio-to-10-decimal-places total-iter-sum total-count))))


#+| Do it | (solve )
#|------------------------------------------------------------|
Timing the evaluation of (solve)
debug: starting the interval based DFS for total iterations...
debug: Processing top-level x-next = 5000000 / 10642857
debug: Processing top-level x-next = 6000000 / 10642857
debug: Processing top-level x-next = 7000000 / 10642857
debug: Processing top-level x-next = 8000000 / 10642857
debug: Processing top-level x-next = 9000000 / 10642857
debug: Processing top-level x-next = 10000000 / 10642857
debug: DFS finished. Total iterations = 400266100622279

User time    =        5.119
System time  =        0.052
Elapsed time =        5.093
Allocation   = 292960 bytes
3701 Page faults
GC time      =        0.000
 |------------------------------------------------------------|#
;;→ "4.4474011180"
:ok