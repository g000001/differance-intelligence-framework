;;; -*- mode: Lisp; coding: utf-8 -*-
;;; llm-model: gemini-3.1-pro
(cl:in-package cl-user)
(defpackage #:project-euler-0222 (:use #:cl #:iterate))
(in-package #:project-euler-0222)

#||
(cl-text project-euler-222-acx
  (cl-comment "CLIF model for P222: Shortest Pipe for Spheres")
  
  (Problem P222)
  (constraint_size P222 21)
  
  (cl-comment "NMF Definition")
  (Algorithm A_BruteForce)
  (complexity A_BruteForce O_N_factorial)
  (NMF A_BruteForce)
  
  (cl-comment "ACX Jump to DP")
  (Algorithm A_DP)
  (target_of J_DP A_DP)
  (complexity A_DP O_2_N_N_squared)
  (grounded_in_ultimate_truth A_DP)
  (implements_debt_clearance A_DP)
  
  (cl-comment "Axiomatic Grounding")
  (avoids_inductive_guessing A_DP)
  (grounds_in_deductive_logic A_DP)
)
||#


(declaim (optimize (speed 3) (safety 0) (debug 0)))

(defun solve-p222 ()
  (let* ((n 21)
         (radii (make-array n :element-type 'double-float
                            :initial-contents (iterate (for i from 30 to 50)
                                                (collect (float i 1d0)))))
         (dist (make-array (list n n) :element-type 'double-float))
         (dp-size (* (ash 1 n) n))
         (dp (make-array dp-size :element-type 'double-float
                         :initial-element most-positive-double-float)))
    
    ;; 距離の事前計算（公理的定礎に基づく還元）
    (iterate (for i from 0 below n)
      (iterate (for j from 0 below n)
        (when (/= i j)
          (let ((ri (aref radii i))
                (rj (aref radii j)))
            (setf (aref dist i j)
                  (* 10.0d0 (sqrt (- (* 2.0d0 (+ ri rj)) 100.0d0))))))))

    ;; DPの初期化
    (iterate (for i from 0 below n)
      (setf (aref dp (+ (* (ash 1 i) n) i)) (aref radii i)))

    ;; 状態遷移（世俗諦の構築）
    (iterate (for mask from 1 below (ash 1 n))
      (declare (type fixnum mask))
      (iterate (for i from 0 below n)
        (declare (type fixnum i))
        (when (plusp (logand mask (ash 1 i)))
          (let ((cost (aref dp (+ (* mask n) i))))
            (declare (type double-float cost))
            (when (< cost most-positive-double-float)
              (iterate (for j from 0 below n)
                (declare (type fixnum j))
                (when (zerop (logand mask (ash 1 j)))
                  (let ((next-mask (logior mask (ash 1 j))))
                    (declare (type fixnum next-mask))
                    (setf (aref dp (+ (* next-mask n) j))
                          (min (aref dp (+ (* next-mask n) j))
                               (+ cost (aref dist i j))))))))))))

    ;; 最終計算とマイクロメートルへの変換
    (let ((min-total most-positive-double-float)
          (full-mask (1- (ash 1 n))))
      (iterate (for i from 0 below n)
        (setf min-total (min min-total (+ (aref dp (+ (* full-mask n) i))
                                          (aref radii i)))))
      (round (* min-total 1000.0d0)))))


#+| Do it | (solve-p222 )
#|------------------------------------------------------------|
Timing the evaluation of (solve-p222)

User time    =       30.116
System time  =        0.244
Elapsed time =       30.576
Allocation   = 11276108824 bytes
40989 Page faults
GC time      =        0.085
 |------------------------------------------------------------|#
;;→ 1590933, 0.11615096195600927D0
:ok
