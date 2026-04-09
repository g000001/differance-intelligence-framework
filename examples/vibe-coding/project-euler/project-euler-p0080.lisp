;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3-flash-preview
(cl:in-package cl-user)
(defpackage #:project-euler-0080 (:use cl iterate alexandria) (:export #:solve))
(in-package #:project-euler-0080)

(defmacro optimized-code-p (boole)
  (typecase boole
    (null nil)
    (T `(declaim (optimize (speed 3) (safety 0) (debug 0))))))

(optimized-code-p t)

;; コンパイル時に 10^198 を計算し、定数として埋め込む
(defconstant $multiplier-10^198 #.(expt 10 198))

(defun sum-of-digits (target-number)
  "巨大整数の各桁の和を計算する"
  (iterate
    (for current-val initially target-number then quotient)
    (until (zerop current-val))
    (for (values quotient remainder) = (truncate current-val 10))
    (sum remainder)))

(defun solve ()
  "最初の100個の自然数について、無理数となる平方根の最初の100桁の数字の和の総和を求める"
  (let ((total-digit-sum 0))
    (format t "Starting exact integer projection calculation...~%")
    
    (iterate
      (for current-n from 1 to 100)
      (let ((integer-root (isqrt current-n)))
        
        ;; 完全平方数（有理数）を除外する
        (unless (= (* integer-root integer-root) current-n)
          
          ;; 浮動小数点を排除し、n * 10^198 の整数平方根（勝義的整数化）を求める
          (let* ((scaled-n (* current-n $multiplier-10^198))
                 (big-root (isqrt scaled-n))
                 (digit-sum (sum-of-digits big-root)))
            
            (incf total-digit-sum digit-sum)
            
            ;; 境界値での自己検算用トレース (n=2 のとき 475 になることを確認)
            (when (or (= current-n 2) (= current-n 99))
              (format t "Trace: n = ~D, digit-sum = ~D~%" current-n digit-sum))))))
              
    (format t "Total Sum: ~D~%" total-digit-sum)
    total-digit-sum))



#+| Do it | (solve )
#|------------------------------------------------------------|
Timing the evaluation of (solve)
Starting exact integer projection calculation...
Trace: n = 2, digit-sum = 475
Trace: n = 99, digit-sum = 446
Total Sum: 40886

User time    =        0.002
System time  =        0.000
Elapsed time =        0.002
Allocation   = 390056 bytes
0 Page faults
GC time      =        0.000
 |------------------------------------------------------------|#
;;→ 40886
:ok
