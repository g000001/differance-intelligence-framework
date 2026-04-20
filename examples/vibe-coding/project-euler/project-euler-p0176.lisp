;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3.1-pro
(cl:in-package cl-user)
(defpackage #:project-euler-0176 (:use cl iterate alexandria) (:export #:solve))
(in-package #:project-euler-0176)

(defmacro optimized-code-p (boole)
  (typecase boole
    (null nil)
    (T `(declaim (optimize (speed 3) (safety 0) (debug 0) #+lispworks (hcl:fixnum-safety 0))))))

(optimized-code-p nil)


(defun factorize (n)
  "整数Nを素因数分解し、因数のリストを返す"
  (let ((factors nil)
        (d 2))
    (iterate (while (> n 1))
      (if (zerop (mod n d))
          (progn
            (push d factors)
            (setf n (/ n d)))
          (incf d)))
    (nreverse factors)))

(defun solve ()
  (let* ((target-n 47547)
         (target-val (+ (* 2 target-n) 1))
         (factors (factorize target-val))
         (primes '(2 3 5 7 11 13 17 19 23 29))
         (k (length factors))
         (used-primes (subseq primes 0 k))
         (min-L nil))
    
    (format t "Target 2N+1 = ~A~%" target-val)
    (format t "Factors: ~A~%" factors)
    (format t "Used primes: ~A~%" used-primes)
    
    ;; 5つの因数のすべての割り当て順列 (5! = 120通り) を試行
    (alexandria:map-permutations
     (lambda (perm)
       (let* ((base-2-factor (car perm))
              (base-2-power (/ (+ base-2-factor 1) 2))
              (L-val (expt 2 base-2-power)))
         
         ;; 奇素数の底のべき乗を計算し乗算
         (iterate (for f in (cdr perm))
                  (for p in (cdr used-primes))
           (let ((power (/ (- f 1) 2)))
             (setf L-val (* L-val (expt p power)))))
         
         (when (or (null min-L) (< L-val min-L))
           (setf min-L L-val))))
     factors)
    
    (format t "--------------------------------------------------~%")
    (format t "Smallest cathetus L: ~A~%" min-L)
    min-L))


#+| Do it | (solve )
#|------------------------------------------------------------|
Timing the evaluation of (solve)
Target 2N+1 = 95095
Factors: (5 7 11 13 19)
Used primes: (2 3 5 7 11)
--------------------------------------------------
Smallest cathetus L: 96818198400000

User time    =        0.000
System time  =        0.000
Elapsed time =        0.000
Allocation   = 11088 bytes
14 Page faults
GC time      =        0.000
 |------------------------------------------------------------|#
;;→ 96818198400000
:ok