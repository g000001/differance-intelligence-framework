;;; -*- mode: Lisp; coding: utf-8 -*-
;;; llm-model: gemini-3.1-pro
(cl:in-package cl-user)
(defpackage #:project-euler-0586 (:use cl series alexandria) (:export #:solve))
(in-package #:project-euler-0586)
(eval-when (:compile-toplevel :load-toplevel :execute) (series::install))

(declaim (optimize (speed 3) (safety 0) (debug 0)))

;; ------------------------------------------------------------
;; Exact Phase Space Definition & Quadratic Field Isomorphism
;; ------------------------------------------------------------

(defconstant +max-x+ #.(expt 10 5))
(defconstant +max-s-prime+ 4050000)

(declaim (type (unsigned-byte 64) *total-count*))
(defvar *total-count* 0)
(defvar *m-array* (make-array (1+ +max-x+) :element-type '(unsigned-byte 32) :initial-element 0))
(defvar *s-primes* (make-array 150000 :element-type '(unsigned-byte 32) :fill-pointer 0))

(defun is-prime (n)
  (declare (type fixnum n))
  (if (< n 2) (return-from is-prime nil))
  (if (= n 2) (return-from is-prime t))
  (if (zerop (mod n 2)) (return-from is-prime nil))
  (let ((limit (isqrt n))
        (prime-p t))
    (iterate ((i (scan-range :from 3 :upto limit :by 2)))
      (when (zerop (mod n i))
        (setf prime-p nil)
        (terminate-producing)))
    prime-p))

(defun div-out (x p)
  (if (zerop (mod x p)) (div-out (truncate x p) p) x))

(defun reduce-inert (n t-primes)
  (let ((res n))
    (iterate ((p (scan t-primes)))
      (setf res (div-out res p)))
    res))

(defun init-m-array ()
  "Precomputes the number of valid multipliers v = 5^c * Q^2 where Q is composed of inert primes."
  (let ((valid-v (make-array (1+ +max-x+) :element-type 'bit :initial-element 0))
        (t-primes (make-array 100 :element-type 'fixnum :fill-pointer 0)))
    (iterate ((p (scan-range :from 2 :upto 316)))
      (when (and (or (= (mod p 5) 2) (= (mod p 5) 3))
                 (is-prime p))
        (vector-push p t-primes)))
    
    (iterate ((q (scan-range :from 1 :upto 316)))
      (when (= (reduce-inert q t-primes) 1)
        (let ((q2 (* q q)))
          (declare (type (unsigned-byte 64) q2))
          (iterate ((c (scan-range :from 0)))
            (let ((v (* q2 (expt 5 c))))
              (declare (type (unsigned-byte 64) v))
              (if (<= v +max-x+)
                  (setf (sbit valid-v v) 1)
                  (terminate-producing)))))))
    
    (let ((sum 0))
      (iterate ((i (scan-range :from 1 :upto +max-x+)))
        (incf sum (sbit valid-v i))
        (setf (aref *m-array* i) sum)))))

(defun init-s-primes ()
  "Sieves all split primes p = 1, 4 (mod 5) bounding the combinatorial search."
  (let ((sieve (make-array (1+ +max-s-prime+) :element-type 'bit :initial-element 0)))
    (setf (sbit sieve 0) 1 (sbit sieve 1) 1)
    (iterate ((i (scan-range :from 2 :upto (isqrt +max-s-prime+))))
      (when (zerop (sbit sieve i))
        (iterate ((j (scan-range :from (* i i) :upto +max-s-prime+ :by i)))
          (setf (sbit sieve j) 1))))
    (iterate ((i (scan-range :from 2 :upto +max-s-prime+)))
      (when (and (zerop (sbit sieve i))
                 (or (= (mod i 5) 1) (= (mod i 5) 4)))
        (vector-push i *s-primes*)))))

;; ------------------------------------------------------------
;; Deep DFS Shape Walker (Amalgamating Combinatorial Boundaries)
;; ------------------------------------------------------------

(defun run-dfs (limit-n exponents depth current-p start-idx path)
  (declare (type fixnum depth start-idx)
           (type (unsigned-byte 64) current-p limit-n)
           (type (simple-array fixnum (*)) exponents path))
  (let ((len (length exponents)))
    (if (= depth len)
        (let ((x (truncate limit-n current-p)))
          (when (<= x +max-x+)
            (incf *total-count* (aref *m-array* x))))
        (let* ((exp (aref exponents depth))
               (min-rem 1))
          (declare (type (unsigned-byte 64) min-rem))
          ;; Calculate bounding asymptote for safe early pruning
          (iterate ((k (scan-range :from (1+ depth) :below len))
                    (p-idx (scan-range :from 0)))
            (setf min-rem (* min-rem (expt (aref *s-primes* p-idx) (aref exponents k)))))

          (let ((max-p-pow (truncate limit-n (* current-p min-rem))))
            (when (< max-p-pow 1) (return-from run-dfs))
            ;; Limit derivation bypassing 64-bit overflow phantom errors
            (let ((p-limit (floor (expt (coerce max-p-pow 'double-float) (/ 1.0d0 exp)))))
              (incf p-limit 2)
              (iterate ((i (scan-range :from start-idx :below (length *s-primes*))))
                (let ((p (aref *s-primes* i)))
                  (if (> p p-limit)
                      (terminate-producing)
                      (let ((p-pow (expt p exp)))
                        (declare (type (unsigned-byte 64) p-pow))
                        (if (> p-pow max-p-pow)
                            (terminate-producing)
                            (let ((used nil))
                              (iterate ((d (scan-range :from 0 :below depth)))
                                (when (= (aref path d) p)
                                  (setf used t)
                                  (terminate-producing)))
                              (unless used
                                (setf (aref path depth) p)
                                ;; Symmetrical redundancy lock for identical exponents
                                (let ((next-start (if (and (< (1+ depth) len)
                                                           (= (aref exponents (1+ depth)) exp))
                                                      (1+ i)
                                                      0)))
                                  (run-dfs limit-n exponents (1+ depth) (* current-p p-pow) next-start path)))))))))))))))

(defun solve-f (limit-n shapes)
  "Resolves f(n, r) using the pre-collapsed quotient forms."
  (setf *total-count* 0)
  (iterate ((shape (scan shapes)))
    (let ((path (make-array (length shape) :element-type 'fixnum :initial-element 0))
          (shape-arr (make-array (length shape) :element-type 'fixnum :initial-contents shape)))
      (run-dfs limit-n shape-arr 0 1 0 path)))
  *total-count*)

;; ------------------------------------------------------------
;; Main Solver API
;; ------------------------------------------------------------

(defun solve ()
  "Entry point for Project Euler 586."
  (init-m-array)
  (init-s-primes)
  
  ;; Defensive Trace Execution verifying the Isomorphism
  (format t "Trace f(10^5, 4) = ~D (Expected 237)~%"
          (solve-f #.(expt 10 5) '((7) (3 1) (1 1 1) (8) (2 2))))
  (format t "Trace f(10^8, 6) = ~D (Expected 59517)~%"
          (solve-f #.(expt 10 8) '((11) (5 1) (3 2) (2 1 1) (12))))
          
  ;; Final Manifestation at Level 34 scale
  (let ((ans (solve-f #.(expt 10 15) 
                      '((9 3 1) (9 1 1 1) (7 4 1) (4 3 3) (4 3 1 1) (4 1 1 1 1) (8 2 2) (2 2 2 2)))))
    (format t "f(10^15, 40) = ~D~%" ans)
    ans))

#+| Do it | (project-euler-0586:solve)
#|------------------------------------------------------------|
Timing the evaluation of (solve)
Trace f(10^5, 4) = 237 (Expected 237)
Trace f(10^8, 6) = 59517 (Expected 59517)
f(10^15, 40) = 260861263

User time    =       22.300
System time  =        0.157
Elapsed time =       22.409
Allocation   = 5216700936 bytes
431 Page faults
GC time      =        0.050
 |------------------------------------------------------------|#
;;→ 260861263
:ok