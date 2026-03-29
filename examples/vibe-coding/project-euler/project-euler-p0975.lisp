;;; -*- mode: Lisp; coding: utf-8 -*-
;;; llm-model: gemini-3.1-pro
(cl:in-package cl-user)
(defpackage #:project-euler-0975 (:use cl series alexandria) (:export #:solve))
(in-package #:project-euler-0975)
(eval-when (:compile-toplevel :load-toplevel :execute) (series::install))

;; ------------------------------------------------------------
;; Generator Utilities (Descriptive Scopes)
;; ------------------------------------------------------------

(defun generate-prime-list (limit-number)
  "Generates a list of prime numbers up to LIMIT-NUMBER using a Series-based sieve."
  (let ((sieve-array (make-array (1+ limit-number) :element-type 'bit :initial-element 0)))
    (collect-ignore
     (mapping ((prime-candidate (scan-range :from 2 :upto (isqrt limit-number))))
       (when (zerop (sbit sieve-array prime-candidate))
         (collect-ignore
          (mapping ((multiple-index (scan-range :from (* prime-candidate prime-candidate) 
                                                :upto limit-number 
                                                :by prime-candidate)))
            (setf (sbit sieve-array multiple-index) 1))))))
    (collect (choose-if (lambda (number-p) (zerop (sbit sieve-array number-p)))
                        (scan-range :from 2 :upto limit-number)))))

(defun generate-critical-points (sum-parameters difference-parameters)
  "Generates sorted critical points for the function H based on sum and difference of parameters."
  (let* ((points-from-sum
          (collect (mapping ((index-k (scan-range :from 1)))
                     (let ((point (/ (* 2.0d0 index-k) sum-parameters)))
                       (if (< point 1.0d0) point (terminate-producing))))))
         (points-from-diff
          (if (> difference-parameters 0)
              (collect (mapping ((index-m (scan-range :from 0)))
                         (let ((point (/ (+ (* 2.0d0 index-m) 1.0d0) difference-parameters)))
                           (if (< point 1.0d0) point (terminate-producing)))))
              nil)))
    (sort (append points-from-sum points-from-diff) #'<)))

(defun make-height-evaluator (parameter-a parameter-b)
  "Creates a closure to exact-evaluate the height H_{a,b}(position)."
  (let ((sum-ab (coerce (+ parameter-a parameter-b) 'double-float))
        (float-a (coerce parameter-a 'double-float))
        (float-b (coerce parameter-b 'double-float)))
    (lambda (position)
      (let ((angle-a (* float-a pi position))
            (angle-b (* float-b pi position)))
        (- 0.5d0 (/ (+ (* float-b (cos angle-a))
                       (* float-a (cos angle-b)))
                    (* 2.0d0 sum-ab)))))))

(defun build-zigzag-values (critical-points height-evaluator)
  "Evaluates the height at critical points and extracts the alternating extrema, ignoring inflections."
  (let* ((evaluated-points
          (cons 0.0d0
                (append (collect (mapping ((point (scan critical-points)))
                                   (funcall height-evaluator point)))
                        (list 1.0d0))))
         (deduplicated-points
          (let ((previous-point nil))
            (collect (choose-if (lambda (current-point)
                                  (when (or (null previous-point) 
                                            (> (abs (- current-point previous-point)) 1d-12))
                                    (setf previous-point current-point)
                                    t))
                                (scan evaluated-points))))))
    (if (< (length deduplicated-points) 3)
        deduplicated-points
        (let ((extremum-sequence (list (car deduplicated-points))))
          (iterate ((prev (scan deduplicated-points))
                    (curr (scan (cdr deduplicated-points)))
                    (next (scan (cddr deduplicated-points))))
            (when (not (or (and (< prev curr) (< curr next))
                           (and (> prev curr) (> curr next))))
              (push curr extremum-sequence)))
          (push (car (last deduplicated-points)) extremum-sequence)
          (nreverse extremum-sequence)))))

;; ------------------------------------------------------------
;; Main Structural Simulation (Topological Invariant State Machine)
;; ------------------------------------------------------------

(defun simulate-path-variation (parameter-a parameter-b parameter-c parameter-d)
  "Simulates the discrete topological path z = H_{a,b}(x) = H_{c,d}(y) and computes Total Variation F."
  (let* ((sum-ab (+ parameter-a parameter-b))
         (sum-cd (+ parameter-c parameter-d))
         (diff-ab (abs (- parameter-a parameter-b)))
         (diff-cd (abs (- parameter-c parameter-d)))
         
         (critical-x-raw (generate-critical-points sum-ab diff-ab))
         (critical-y-raw (generate-critical-points sum-cd diff-cd))
         (evaluator-x (make-height-evaluator parameter-a parameter-b))
         (evaluator-y (make-height-evaluator parameter-c parameter-d))
         
         (extrema-x (coerce (build-zigzag-values critical-x-raw evaluator-x) 'vector))
         (extrema-y (coerce (build-zigzag-values critical-y-raw evaluator-y) 'vector))
         
         (index-x 0)
         (index-y 0)
         (current-z 0.0d0)
         (total-variation 0.0d0))
    
    (tagbody
     state-machine-loop
       (when (>= current-z 0.9999999d0)
         (go end-machine))
       
       (let* ((z-x-start (aref extrema-x index-x))
              (z-x-end (aref extrema-x (1+ index-x)))
              (z-y-start (aref extrema-y index-y))
              (z-y-end (aref extrema-y (1+ index-y)))
              
              ;; Topological Invariant: The cross-derivative determines the transition cell direction
              (sign-x (if (> z-x-end z-x-start) 1 -1))
              (sign-y (if (> z-y-end z-y-start) 1 -1))
              
              (z-x-min (min z-x-start z-x-end))
              (z-x-max (max z-x-start z-x-end))
              (z-y-min (min z-y-start z-y-end))
              (z-y-max (max z-y-start z-y-end))
              
              (overlap-min (max z-x-min z-y-min))
              (overlap-max (min z-x-max z-y-max))
              
              ;; Path must transit from one end of the overlap strictly to the other
              (next-z (if (< (abs (- current-z overlap-min)) 1d-12)
                          overlap-max
                          overlap-min)))
         
         (incf total-variation (abs (- next-z current-z)))
         
         ;; Strict logic check: which geometric boundary did we precisely hit?
         (let* ((target-x-limit (if (> next-z current-z) z-x-max z-x-min))
                (target-y-limit (if (> next-z current-z) z-y-max z-y-min))
                (hit-x-boundary-p (< (abs (- next-z target-x-limit)) 1d-12))
                (hit-y-boundary-p (< (abs (- next-z target-y-limit)) 1d-12)))
           
           (when hit-x-boundary-p
             (incf index-x sign-y))
           (when hit-y-boundary-p
             (incf index-y sign-x)))
         
         (setf current-z next-z))
       (go state-machine-loop)
     end-machine)
    
    total-variation))

;; ------------------------------------------------------------
;; Main Solver API
;; ------------------------------------------------------------

(defun solve ()
  "Computes G(500, 1000) using the discrete path projection logic."
  (let* ((primes-list (generate-prime-list 1000))
         (target-primes (collect (choose-if (lambda (prime) (and (>= prime 500) (<= prime 1000))) 
                                            (scan primes-list))))
         (total-sum 0.0d0))
    
    (iterate ((prime-p (scan target-primes)))
      (iterate ((prime-q (scan target-primes)))
        (when (< prime-p prime-q)
          (let ((variation-f (simulate-path-variation prime-p prime-q prime-p (- (* 2 prime-q) prime-p))))
            (incf total-sum variation-f)))))
    
    (format t "G(500, 1000) = ~,5F~%" total-sum)
    total-sum))

#+| Do it | (project-euler-0975:solve)
#|------------------------------------------------------------|
Timing the evaluation of (solve)
G(500, 1000) = 88597366.47748

User time    =  0:01:16.684
System time  =        1.009
Elapsed time =  0:01:24.633
Allocation   = 22874280656 bytes
70201 Page faults
GC time      =        0.741
 |------------------------------------------------------------|#
;;→ 8.859736647748152D7
:ok
