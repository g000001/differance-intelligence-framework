;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3.1-pro
(cl:in-package cl-user)
(defpackage #:project-euler-0757 (:use cl iterate alexandria) (:export #:solve))
(in-package #:project-euler-0757)

(defmacro optimized-code-p (boole)
  (typecase boole
    (null nil)
    (T `(declaim (optimize (speed 3) (safety 0) (debug 0) #+lispworks (hcl:fixnum-safety 0))))))

(optimized-code-p T)


(defun count-stealthy (limit)
  (declare (type fixnum limit))
  ;; 安全係数をかけて配列の最大サイズをフェルミ推定 (約 12 * sqrt(limit))
  (let* ((max-size (floor (* 12.0d0 (sqrt (coerce limit 'double-float)))))
         (arr (make-array max-size :element-type 'fixnum :fill-pointer 0))
         (idx 0))
    (declare (type fixnum max-size idx))
    
    ;; 次元崩壊した不変量 N = x(x+1)y(y+1) の生成
    (iterate (for y from 1)
             (for y2 = (the fixnum (* y (the fixnum (1+ y)))))
             (while (<= (the fixnum (* y2 y2)) limit))
             (iterate (for x from y)
                      (for x2 = (the fixnum (* x (the fixnum (1+ x)))))
                      (for n = (the fixnum (* x2 y2)))
                      (while (<= n limit))
                      (setf (aref arr idx) n)
                      (incf idx)))
    
    (setf (fill-pointer arr) idx)
    (format t "Limit: 10^~A, Generated ~D candidates.~%" (round (log limit 10)) idx)
    
    ;; 重複の排除と集計
    (sort arr #'<)
    (let ((unique-count 0)
          (last-val -1))
      (declare (type fixnum unique-count last-val))
      (iterate (for i from 0 below idx)
               (for val = (the fixnum (aref arr i)))
               (when (/= val last-val)
                 (incf unique-count)
                 (setf last-val val)))
      unique-count)))

(defun solve ()
  ;; 制約のシミュレーションと検証 ($10^6$ のケース)
  (format t "Testing limit 10^6: ~D (expected 2851)~%" (count-stealthy #.(expt 10 6)))
  
  ;; 本番の巨大な制約への挑戦
  (let ((ans (count-stealthy #.(expt 10 14))))
    (format t "Result for 10^14: ~D~%" ans)
    ans))


#+| Do it | (solve )
#|------------------------------------------------------------|
Timing the evaluation of (solve)
Limit: 10^6, Generated 2990 candidates.
Testing limit 10^6: 2851 (expected 2851)
Limit: 10^14, Generated 75782308 candidates.
Result for 10^14: 75737353

User time    =       47.502
System time  =        0.526
Elapsed time =       47.948
Allocation   = 960934376 bytes
152135 Page faults
GC time      =        0.000
 |------------------------------------------------------------|#
;;→ 75737353
:ok