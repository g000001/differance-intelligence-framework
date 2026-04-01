;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3.1-pro
(cl:in-package cl-user)
(defpackage #:project-euler-0780 (:use cl series alexandria) (:export #:solve))
(in-package #:project-euler-0780)
(eval-when (:compile-toplevel :load-toplevel :execute) (series::install))

(declaim (optimize (speed 3) (safety 0) (debug 0)))

(defconstant $mod-value 1000000007)

(defun mod-add (val-a val-b)
  (declare (type fixnum val-a val-b))
  (let ((sum (+ val-a val-b)))
    (if (>= sum $mod-value)
        (- sum $mod-value)
        sum)))

(defun mod-sub (val-a val-b)
  (declare (type fixnum val-a val-b))
  (let ((diff (- val-a val-b)))
    (if (< diff 0)
        (+ diff $mod-value)
        diff)))

(defun mod-mul (val-a val-b)
  (declare (type fixnum val-a val-b))
  (mod (* val-a val-b) $mod-value))

(defun triangular-number-mod (limit-m)
  "Computes T(M) = M(M+1)/2 modulo 1000000007 without floating point operations."
  (declare (type fixnum limit-m))
  (let ((m-mod (mod limit-m $mod-value))
        (m-plus-1-mod (mod (1+ limit-m) $mod-value)))
    (let ((product (mod-mul m-mod m-plus-1-mod)))
      (if (evenp product)
          (ash product -1)
          (ash (+ product $mod-value) -1)))))

(defun compute-x-array (limit-size)
  "Computes the multiplicative convolution sequence x(m) using a linear sieve to prevent allocation overhead."
  (let ((x-sequence (make-array (1+ limit-size) :element-type 'fixnum :initial-element 0))
        (prime-list (make-array limit-size :element-type 'fixnum :fill-pointer 0)))
    (setf (aref x-sequence 1) 1)
    
    (iterate ((index-i (scan-range :from 2 :upto limit-size)))
      (declare (type fixnum index-i))
      (when (= (aref x-sequence index-i) 0)
        (vector-push index-i prime-list)
        ;; Generative function constraint mapping for Eisenstein lattice classes
        (let ((chi-val (case (mod index-i 3) (1 1) (2 -1) (otherwise 0))))
          (setf (aref x-sequence index-i) (mod-add index-i (mod-sub 1 chi-val)))))
      
      (let ((prime-count (length prime-list)))
        (do ((index-j 0 (1+ index-j)))
            ((>= index-j prime-count))
          (let* ((prime-p (aref prime-list index-j))
                 (product (* index-i prime-p)))
            (declare (type fixnum prime-p product))
            (when (> product limit-size) (return))
            
            (if (zerop (mod index-i prime-p))
                (progn
                  (setf (aref x-sequence product) (mod-mul (aref x-sequence index-i) prime-p))
                  (return))
                (setf (aref x-sequence product) (mod-mul (aref x-sequence index-i) (aref x-sequence prime-p))))))))
    x-sequence))

(defun compute-g-total (target-limit)
  "Computes G(N) modulo 10^9+7 utilizing the Dirichlet hyperbola method to achieve O(sqrt(N)) time complexity."
  (let* ((max-k (floor target-limit 2))
         (sqrt-k (isqrt max-k))
         (x-sequence (compute-x-array sqrt-k))
         (total-accumulation 0))
    (declare (type fixnum max-k sqrt-k total-accumulation))
    
    ;; Part 1: Dense interval where m <= sqrt(K)
    (iterate ((index-m (scan-range :from 1 :upto sqrt-k)))
      (declare (type fixnum index-m))
      (let* ((x-val (aref x-sequence index-m))
             (t-val (triangular-number-mod (floor max-k index-m))))
        (setf total-accumulation (mod-add total-accumulation (mod-mul x-val t-val)))))
        
    ;; Part 2: Sparse interval utilizing constant blocks of floor(K/m)
    ;; By isolating the distinct values of V = floor(K/m), we jump over large segments.
    (let ((prefix-sum-x (make-array (1+ sqrt-k) :element-type 'fixnum :initial-element 0)))
      (iterate ((index-i (scan-range :from 1 :upto sqrt-k)))
        (setf (aref prefix-sum-x index-i) (mod-add (aref prefix-sum-x (1- index-i)) (aref x-sequence index-i))))
        
      (do ((block-v (floor max-k (1+ sqrt-k)) (1- block-v)))
          ((< block-v 1))
        (declare (type fixnum block-v))
        (let* ((m-start (1+ (floor max-k (1+ block-v))))
               (m-end (floor max-k block-v)))
          (when (> m-start sqrt-k)
            ;; Utilizing the Mobius-inversion symmetry boundaries strictly within fixnum registers
            (let ((t-val (triangular-number-mod block-v))
                  (range-sum (mod-sub (aref prefix-sum-x (min m-end sqrt-k))
                                      (aref prefix-sum-x (min (1- m-start) sqrt-k)))))
              (setf total-accumulation (mod-add total-accumulation (mod-mul t-val range-sum))))))))
              
    total-accumulation))

(defun solve ()
  (let ((target-n 1000000000))
    (format t "Testing G(6)...~%")
    (let ((ans-6 (compute-g-total 6)))
      (format t "G(6) = ~A~%" ans-6))
    
    (format t "Testing G(100)...~%")
    (let ((ans-100 (compute-g-total 100)))
      (format t "G(100) = ~A~%" ans-100))
      
    (format t "Solving for G(~A)...~%" target-n)
    (let ((final-answer (compute-g-total target-n)))
      (format t "Answer modulo 10^9+7: ~A~%" final-answer)
      final-answer)))