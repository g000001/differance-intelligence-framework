;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3-flash-preview
(cl:in-package cl-user)
(defpackage #:project-euler-0108 (:use cl iterate alexandria) (:export #:solve))
(in-package #:project-euler-0108)

(defmacro optimized-code-p (boole)
  (typecase boole
    (null nil)
    (T `(declaim (optimize (speed 3) (safety 0) (debug 0) #+lispworks (hcl:fixnum-safety 0))))))

(optimized-code-p T)


(defun solve ()
  "Finds the least value of n for which the number of distinct solutions exceeds 1000.
Uses a depth-first search over prime exponents with the constraint a1 >= a2 >= ... >= ak."
  (let ((limit-min-n #.(expt 2 60)) ; Initial infinity for pruning
        (target-dn2 2001)           ; (d(n^2)+1)/2 > 1000 => d(n^2) >= 2001
        (primes '(2 3 5 7 11 13 17 19 23 29 31 37 41 43 47)))
    
    (format t "Target: d(n^2) >= ~D~%" target-dn2)

    (labels ((dfs (prime-idx current-n current-dn2 last-exponent)
               ;; If target reached, update global minimum
               (if (>= current-dn2 target-dn2)
                   (progn
                     (when (< current-n limit-min-n)
                       (setf limit-min-n current-n)
                       (format t "New candidate: n = ~D, solutions = ~D~%" 
                               current-n (ash (1+ current-dn2) -1)))
                     (return-from dfs))
                   ;; Otherwise, try adding more primes or increasing exponents
                   (unless (>= prime-idx (length primes))
                     (let ((p (nth prime-idx primes)))
                       (iterate (for a from 1 to last-exponent)
                                (for p-pow initially p then (* p-pow p))
                                (for next-n = (* current-n p-pow))
                                ;; Pruning: stop if current n already exceeds best found
                                (while (< next-n limit-min-n))
                                ;; Recurse to next prime with non-increasing exponent constraint
                                (dfs (1+ prime-idx) 
                                     next-n 
                                     (* current-dn2 (1+ (* 2 a))) 
                                     a)))))))
      
      ;; Start DFS: prime index 0, n=1, d(n^2)=1, max initial exponent 30
      (dfs 0 1 1 30)
      
      (format t "----------------------------------------~%")
      (format t "Result: ~D~%" limit-min-n)
      limit-min-n)))

#+| Do it | (solve )
#|------------------------------------------------------------|
Timing the evaluation of (solve)
Target: d(n^2) >= 2001
New candidate: n = 510510, solutions = 1094
New candidate: n = 180180, solutions = 1013
----------------------------------------
Result: 180180

User time    =        0.000
System time  =        0.000
Elapsed time =        0.001
Allocation   = 632 bytes
1 Page faults
GC time      =        0.000
 |------------------------------------------------------------|#
;;→ 180180
:ok