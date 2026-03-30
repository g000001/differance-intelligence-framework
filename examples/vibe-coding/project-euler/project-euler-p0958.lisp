;;; -*- mode: Lisp; coding: utf-8 -*-
;;; llm-model: gemini-3.1-pro
(cl:in-package cl-user)
(defpackage #:project-euler-0958 (:use cl series alexandria) (:export #:solve))
(in-package #:project-euler-0958)
(eval-when (:compile-toplevel :load-toplevel :execute) (series::install))

(defconstant +max-buckets+ 200)
(defvar *buckets* (make-array +max-buckets+ :initial-element nil))

;; ------------------------------------------------------------
;; Exact Arithmetic & Precomputation
;; ------------------------------------------------------------

(defvar *fib* (make-array 100 :element-type '(unsigned-byte 64)))

(defun init-fib ()
  "Precomputes the Fibonacci sequence for the Absolute Maximum Growth bound."
  (setf (aref *fib* 0) 0)
  (setf (aref *fib* 1) 1)
  (loop for i from 2 to 90 do
        (setf (aref *fib* i) (+ (aref *fib* (1- i)) (aref *fib* (- i 2))))))

(defun heuristic-r (u v target-n)
  "A* Admissible Heuristic: Computes strict minimum cost to reach target-n."
  (declare (type (unsigned-byte 64) u v target-n)
           (optimize (speed 3) (safety 0)))
  (let ((fib *fib*))
    (declare (type (simple-array (unsigned-byte 64) (100)) fib))
    (loop for r of-type fixnum from 0
          do (when (>= (+ (* u (aref fib (1+ r)))
                          (* v (aref fib r)))
                       target-n)
               (return r)))))

;; ------------------------------------------------------------
;; Phase Space Search (A* on Continuant Tree) with Monotonic Pruning
;; ------------------------------------------------------------

(defun solve-f (limit-n)
  "Finds the minimal m coprime to n minimizing subtractive Euclidean steps."
  (init-fib)
  (loop for i from 0 below +max-buckets+ do (setf (aref *buckets* i) nil))
  
  (let ((best-m most-positive-fixnum)
        (best-cost +max-buckets+) ; Boundary defense: automatically prune anything >= 200
        (min-f 0))
    (declare (type fixnum min-f best-cost)
             (type (unsigned-byte 64) best-m))
    
    ;; Root Expansion
    (loop for q of-type (unsigned-byte 64) from 2
          do (let ((u-new q)
                   (v-new 1))
               (when (> u-new limit-n) (return))
               
               (if (= u-new limit-n)
                   (progn
                     (if (< q best-cost)
                         (setf best-cost q best-m v-new)
                         (when (= q best-cost)
                           (setf best-m (min best-m v-new))))
                     (return)) ;; Exceeds limit-n if q increases further
                   (let* ((cost-new q)
                          (r (heuristic-r u-new v-new limit-n))
                          (f-new (+ cost-new r)))
                     (declare (type fixnum cost-new r f-new))
                     ;; Monotonicity Pruning: f(q) never decreases. Cut off completely.
                     (if (>= f-new best-cost)
                         (return)
                         (push (list cost-new u-new v-new)
                               (aref *buckets* f-new)))))))
    
    ;; A* Search Core
    (loop
      (loop while (and (< min-f +max-buckets+) (null (aref *buckets* min-f)))
            do (incf min-f))
      
      (when (or (>= min-f +max-buckets+) (> min-f best-cost))
        (return best-m))
      
      (let ((current-bucket (aref *buckets* min-f)))
        (setf (aref *buckets* min-f) nil)
        
        (loop for state in current-bucket
              do (let ((cost (first state))
                       (u (second state))
                       (v (third state)))
                   (declare (type fixnum cost)
                            (type (unsigned-byte 64) u v))
                   
                   (when (< cost best-cost)
                     (loop for q of-type (unsigned-byte 64) from 1
                           do (let ((u-new (+ (* q u) v)))
                                (declare (type (unsigned-byte 64) u-new))
                                (when (> u-new limit-n) (return))
                                
                                (if (= u-new limit-n)
                                    (let ((final-cost (+ cost q)))
                                      (if (< final-cost best-cost)
                                          (setf best-cost final-cost best-m u)
                                          (when (= final-cost best-cost)
                                            (setf best-m (min best-m u))))
                                      (return))
                                    (let* ((cost-new (+ cost q))
                                           (r (heuristic-r u-new u limit-n))
                                           (f-new (+ cost-new r)))
                                      (declare (type fixnum cost-new r f-new))
                                      ;; Monotonicity Pruning
                                      (if (>= f-new best-cost)
                                          (return)
                                          (push (list cost-new u-new u)
                                                (aref *buckets* f-new))))))))))))))

;; ------------------------------------------------------------
;; Main Solver API
;; ------------------------------------------------------------

(defun solve ()
  "Entry point for Project Euler 958."
  (format t "Trace f(7) = ~A (Expected 2)~%" (solve-f 7))
  (format t "Trace f(89) = ~A (Expected 34)~%" (solve-f 89))
  (format t "Trace f(8191) = ~A (Expected 1856)~%" (solve-f 8191))
  
  (format t "Trace f(10^9+39) = ~A (Expected 295627107)~%" (solve-f (+ (expt 10 9) 39)))

  (let* ((target-n (+ (expt 10 12) 39))
         (ans (solve-f target-n)))
    (format t "f(10^12+39) = ~A~%" ans)
    ans))

#+| Do it | (project-euler-0958:solve)