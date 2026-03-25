;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3.5-pro
(cl:in-package cl-user)
(defpackage #:project-euler-0432 (:use cl series alexandria) (:export #:solve))
(in-package #:project-euler-0432)
(eval-when (:compile-toplevel :load-toplevel :execute) (series::install))

(defconstant +mod+ #.(expt 10 9))
(defconstant +m-size+ 20000000)

;; Pre-calculated arrays (Total allocation < 85MB)
(defvar *phi-sum* (make-array (1+ +m-size+) :element-type '(unsigned-byte 32)))
(defvar *memo-large* (make-array 5010 :element-type 'fixnum :initial-element -1))
(defvar *primes* (make-array 7 :element-type 'fixnum :initial-contents '(2 3 5 7 11 13 17)))

(defun build-phi ()
  "Builds the prefix sums of Euler's totient function using a linear sieve."
  (iterate ((i (scan-range :from 1 :upto +m-size+)))
    (setf (aref *phi-sum* i) i))
  (iterate ((i (scan-range :from 2 :upto +m-size+)))
    (when (= (aref *phi-sum* i) i)
      (iterate ((j (scan-range :from i :upto +m-size+ :by i)))
        (setf (aref *phi-sum* j) (- (aref *phi-sum* j) (truncate (aref *phi-sum* j) i))))))
  (let ((sum 0))
    (declare (type fixnum sum))
    (iterate ((i (scan-range :from 1 :upto +m-size+)))
      (setf sum (mod (+ sum (aref *phi-sum* i)) +mod+))
      (setf (aref *phi-sum* i) sum))))

(defun get-phi-sum (x m)
  "Calculates Phi(x) mod 10^9 using the Du-sieve technique with O(1) state caching."
  (declare (type fixnum x m))
  (if (<= x +m-size+)
      (aref *phi-sum* x)
      ;; Number theory shortcut: x is always of the form floor(m / c)
      (let ((c (truncate m x)))
        (declare (type fixnum c))
        (if (not (= (aref *memo-large* c) -1))
            (aref *memo-large* c)
            (let* ((x-even (if (evenp x) x (1+ x)))
                   (x-odd  (if (evenp x) (1+ x) x))
                   (term1  (mod (truncate x-even 2) +mod+))
                   (term2  (mod x-odd +mod+))
                   (ans    (mod (* term1 term2) +mod+))
                   (l      2))
              (declare (type fixnum term1 term2 ans l))
              (iterate ()
                (when (> l x) (terminate-producing))
                (let* ((q (truncate x l))
                       (r (truncate x q))
                       (count (mod (- r l -1) +mod+))
                       (sub (mod (* count (get-phi-sum q m)) +mod+)))
                  (declare (type fixnum q r count sub))
                  ;; Ensuring modulus arithmetic stays strictly positive
                  (setf ans (mod (+ (- ans sub) +mod+) +mod+))
                  (setf l (1+ r))))
              (setf (aref *memo-large* c) ans)
              ans)))))

(defun dfs (idx current-d m)
  "Recursively generates 17-smooth numbers and aggregates their contributions."
  (declare (type fixnum idx current-d m))
  (if (= idx 7)
      (get-phi-sum (truncate m current-d) m)
      (let ((ans 0)
            (p (aref *primes* idx))
            (d current-d))
        (declare (type fixnum ans p d))
        (iterate ()
          (when (> d m) (terminate-producing))
          (setf ans (mod (+ ans (dfs (1+ idx) d m)) +mod+))
          (setf d (* d p)))
        ans)))

(defun solve-for (m)
  ;; n = 510510, phi(510510) = 92160
  (let ((phi-n 92160)
        (ans (dfs 0 1 m)))
    (mod (* phi-n ans) +mod+)))

(defun solve ()
  (format t "Initializing sieve for M=~A...~%" +m-size+)
  (build-phi)
  
  (format t "Validating with S(510510, 10^6)...~%")
  (iterate ((i (scan-range :from 0 :below 5010)))
    (setf (aref *memo-large* i) -1))
  (let ((test-ans (solve-for 1000000)))
    (format t "S(510510, 10^6) mod 10^9 = ~A (Expected 821125120)~%" test-ans)
    (assert (= test-ans 821125120)))
    
  (format t "Computing S(510510, 10^11)...~%")
  (iterate ((i (scan-range :from 0 :below 5010)))
    (setf (aref *memo-large* i) -1))
  (let ((final-ans (solve-for #.(expt 10 11))))
    (format t "Final Answer (last 9 digits): ~9,'0d~%" final-ans)
    final-ans))

#+| Do it | (project-euler-0432:solve)
#|------------------------------------------------------------|
Timing the evaluation of (solve)
Initializing sieve for M=20000000...
Validating with S(510510, 10^6)...
S(510510, 10^6) mod 10^9 = 821125120 (Expected 821125120)
Computing S(510510, 10^11)...
Final Answer (last 9 digits): 754862080

User time    =       16.228
System time  =        0.139
Elapsed time =       16.338
Allocation   = 402136 bytes
4110 Page faults
GC time      =        0.000
 |------------------------------------------------------------|#
;;→ 754862080
