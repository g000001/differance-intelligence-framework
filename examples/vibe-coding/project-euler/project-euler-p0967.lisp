;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3.1-pro
(cl:in-package cl-user)
(defpackage #:project-euler-0967 (:use cl series alexandria) (:export #:solve))
(in-package #:project-euler-0967)
(eval-when (:compile-toplevel :load-toplevel :execute) (series::install))
(declaim (optimize (speed 3) (safety 0) (debug 0)))


(defconstant $limit-n #.(expt 10 18))
(defconstant $limit-b 120)

(defvar *h-table* (make-array '(30 30) :element-type 'fixnum))

(defstruct (subset-item (:type list))
  (item-d 0 :type integer)
  (item-k1 0 :type fixnum)
  (item-k2 0 :type fixnum))

(defun mul-eisen (eisen-x eisen-y)
  (let ((real-x (car eisen-x))
        (imag-x (cdr eisen-x))
        (real-y (car eisen-y))
        (imag-y (cdr eisen-y)))
    (cons (- (* real-x real-y) (* imag-x imag-y))
          (- (+ (* real-x imag-y) (* imag-x real-y)) (* imag-x imag-y)))))

(defun power-eisen (base-eisen power-n)
  (let ((result-eisen '(1 . 0))
        (current-base base-eisen))
    (iterate ((index (scan-range :from 0 :below power-n)))
      (setf result-eisen (mul-eisen result-eisen current-base)))
    result-eisen))

(defun init-h-table ()
  (iterate ((count-k1 (scan-range :from 0 :upto 29)))
    (iterate ((count-k2 (scan-range :from 0 :upto 29)))
      (let* ((power-1 (power-eisen '(-1 . 1) count-k1))
             (power-2 (power-eisen '(-2 . -1) count-k2))
             (product-g1 (mul-eisen power-1 power-2))
             (real-part (car product-g1))
             (imag-part (cdr product-g1))
             (sum-g1-g2 (- (* 2 real-part) imag-part))
             (term-g0 (if (and (= count-k1 0) (= count-k2 0)) 1 0)))
        (setf (aref *h-table* count-k1 count-k2) (/ (+ term-g0 sum-g1-g2) 3))))))

(defun primes-up-to (max-limit)
  (let ((prime-flags (make-array (1+ max-limit) :element-type 'boolean :initial-element t)))
    (when (>= max-limit 0) (setf (aref prime-flags 0) nil))
    (when (>= max-limit 1) (setf (aref prime-flags 1) nil))
    (iterate ((prime-candidate (scan-range :from 2 :upto max-limit)))
      (when (aref prime-flags prime-candidate)
        (iterate ((multiple (scan-range :from (* prime-candidate 2) :upto max-limit :by prime-candidate)))
          (setf (aref prime-flags multiple) nil))))
    (collect (choose-if (lambda (check-index) (aref prime-flags check-index))
                        (scan-range :from 2 :upto max-limit)))))

(defun get-valid-primes (max-b)
  (sort (remove 3 (primes-up-to max-b)) #'>))

(defun generate-subsets (prime-list upper-limit)
  (let ((result-array (make-array 1000 :adjustable t :fill-pointer 0)))
    (labels ((dfs-search (prime-index current-product current-k1 current-k2)
               (if (= prime-index (length prime-list))
                   (vector-push-extend (make-subset-item :item-d current-product :item-k1 current-k1 :item-k2 current-k2) result-array)
                   (let* ((current-prime (nth prime-index prime-list))
                          (modulus-3 (mod current-prime 3)))
                     (when (<= (* current-product current-prime) upper-limit)
                       (if (= modulus-3 1)
                           (dfs-search (1+ prime-index) (* current-product current-prime) (1+ current-k1) current-k2)
                           (dfs-search (1+ prime-index) (* current-product current-prime) current-k1 (1+ current-k2))))
                     (dfs-search (1+ prime-index) current-product current-k1 current-k2)))))
      (dfs-search 0 1 0 0))
    result-array))

(defun solve-for (upper-limit max-b)
  (init-h-table)
  (let* ((valid-primes-list (get-valid-primes max-b))
         (total-primes-length (length valid-primes-list))
         (half-length (floor total-primes-length 2))
         (primes-group-a (subseq valid-primes-list 0 half-length))
         (primes-group-b (subseq valid-primes-list half-length total-primes-length))
         (subsets-list-a (generate-subsets primes-group-a upper-limit))
         (subsets-list-b (generate-subsets primes-group-b upper-limit))
         (total-answer 0))
    (format t "Debug Log: Primes A ~A~%" primes-group-a)
    (format t "Debug Log: Primes B ~A~%" primes-group-b)
    (format t "Debug Log: Subsets A length = ~A~%" (length subsets-list-a))
    (format t "Debug Log: Subsets B length = ~A~%" (length subsets-list-b))
    
    (setf subsets-list-b (sort subsets-list-b #'< :key #'subset-item-item-d))
    
    (iterate ((current-item-a (scan 'vector subsets-list-a)))
      (let ((product-a (subset-item-item-d current-item-a))
            (count-k1-a (subset-item-item-k1 current-item-a))
            (count-k2-a (subset-item-item-k2 current-item-a)))
        (iterate ((current-item-b (until-if (lambda (check-item-b) (> (* product-a (subset-item-item-d check-item-b)) upper-limit))
                                            (scan 'vector subsets-list-b))))
          (let* ((product-b (subset-item-item-d current-item-b))
                 (total-k1 (+ count-k1-a (subset-item-item-k1 current-item-b)))
                 (total-k2 (+ count-k2-a (subset-item-item-k2 current-item-b)))
                 (h-value (aref *h-table* total-k1 total-k2)))
            (unless (zerop h-value)
              (incf total-answer (* h-value (floor upper-limit (* product-a product-b)))))))))
    
    (format t "Intermediate log: total-answer is ~A~%" total-answer)
    total-answer))

(defun solve ()
  (solve-for $limit-n $limit-b))

#+| Do it | (project-euler-0967:solve)
#|------------------------------------------------------------|
Timing the evaluation of (solve)
Debug Log: Primes A (113 109 107 103 101 97 89 83 79 73 71 67 61 59)
Debug Log: Primes B (53 47 43 41 37 31 29 23 19 17 13 11 7 5 2)
Debug Log: Subsets A length = 14913
Debug Log: Subsets B length = 32763
Intermediate log: total-answer is 357591131712034236

User time    =        2.916
System time  =        0.032
Elapsed time =        2.888
Allocation   = 4157808 bytes
381 Page faults
GC time      =        0.002
 |------------------------------------------------------------|#
;;→ 357591131712034236
:ok