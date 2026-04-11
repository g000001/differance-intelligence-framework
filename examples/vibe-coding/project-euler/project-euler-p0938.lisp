;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3-flash-preview
(cl:in-package cl-user)
(defpackage #:project-euler-0938 (:use cl iterate alexandria) (:export #:solve))
(in-package #:project-euler-0938)

(defmacro optimized-code-p (boole)
  (typecase boole
    (null nil)
    (T `(declaim (optimize (speed 3) (safety 0) (debug 0) #+lispworks (hcl:fixnum-safety 0))))))

(optimized-code-p t)

(defun compute-p (r-val b-limit)
  "R枚の赤カードとB枚の黒カードから開始し、最終的に黒のみが残る確率P(R, B)を計算する。
   Rは偶数であることを前提とし、k = R/2 とする。"
  (let* ((k-limit (truncate r-val 2))
         ;; dp[b] は P(2k, b) を保持する。初期状態は k=0 即ち P(0, b) = 1.0
         (dp (make-array (1+ b-limit) :element-type 'double-float :initial-element 1.0d0)))
    (declare (type (simple-array double-float (*)) dp)
             (type fixnum k-limit b-limit))
    
    ;; 境界条件: P(R, 0) = 0
    (setf (aref dp 0) 0.0d0)
    
    (iterate (for k from 1 to k-limit)
             (let ((2k-1-f (float (1- (* 2 k)) 1.0d0))
                   (prev-p 0.0d0)) ; prev-p は P(2k, b-1) を表す。初期値 P(2k, 0) = 0
               (declare (type double-float 2k-1-f prev-p))
               (iterate (for b from 1 to b-limit)
                        (declare (type fixnum b))
                        (let* ((2b-f (float (+ b b) 1.0d0))
                               ;; 漸化式: P(2k, b) = ( (2k-1)*P(2k-2, b) + 2b*P(2k, b-1) ) / (2k-1 + 2b)
                               (num (+ (* 2k-1-f (aref dp b))
                                       (* 2b-f prev-p)))
                               (den (+ 2k-1-f 2b-f))
                               (current-p (/ num den)))
                          (declare (type double-float 2b-f num den current-p))
                          (setf (aref dp b) current-p)
                          (setf prev-p current-p))))
             ;; 進捗ログ (巨大な計算の安心材料)
             (when (zerop (mod k 2000))
               (format t "Progress: k = ~D / ~D done.~%" k k-limit)))
    (aref dp b-limit)))

(defun solve ()
  "Project Euler P938 を解く。テストケースを確認後、本番の値を計算する。"
  (format t "--- Project Euler 938 ---~%")
  
  ;; テストケースの検証
  (let ((t1 (compute-p 2 2))
        (t2 (compute-p 10 9))
        (t3 (compute-p 34 25)))
    (format t "Test P(2, 2)   = ~,10F (Expected: 0.4666666667)~%" t1)
    (format t "Test P(10, 9)  = ~,10F (Expected: 0.4118903397)~%" t2)
    (format t "Test P(34, 25) = ~,10F (Expected: 0.3665688069)~%" t3))
  
  ;; 本番計算
  (let ((r-target 24690)
        (b-target 12345))
    (format t "Calculating P(~D, ~D)...~%" r-target b-target)
    (time
     (let ((result (compute-p r-target b-target)))
       (format t "Final Answer: ~,10F~%" result)))))

#+| Do it | (solve )
#|------------------------------------------------------------|
Timing the evaluation of (solve)
--- Project Euler 938 ---
Test P(2, 2)   = 0.4666666667 (Expected: 0.4666666667)
Test P(10, 9)  = 0.4118903397 (Expected: 0.4118903397)
Test P(34, 25) = 0.3665688069 (Expected: 0.3665688069)
Calculating P(24690, 12345)...
Timing the evaluation of (let ((result (compute-p r-target b-target))) (format t "Final Answer: ~,10F~%" result))
Progress: k = 2000 / 12345 done.
Progress: k = 4000 / 12345 done.
Progress: k = 6000 / 12345 done.
Progress: k = 8000 / 12345 done.
Progress: k = 10000 / 12345 done.
Progress: k = 12000 / 12345 done.
Final Answer: 0.2928967987

User time    =        4.390
System time  =        0.059
Elapsed time =        4.385
Allocation   = 17069138928 bytes
422 Page faults
GC time      =        0.128

User time    =        4.391
System time  =        0.059
Elapsed time =        4.385
Allocation   = 17069207088 bytes
424 Page faults
GC time      =        0.128
 |------------------------------------------------------------|#
;;→ nil
:ok