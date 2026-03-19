;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3.5-pro
(cl:in-package cl-user)
(defpackage #:project-euler-0339 (:use cl iterate alexandria) (:export #:solve))
(in-package #:project-euler-0339)

(declaim (inline bignum-log bignum-ratio-to-float))

(defun bignum-log (n)
  "Calculates the natural logarithm of a Bignum safely."
  (declare (type integer n))
  (let ((len (integer-length n)))
    (if (<= len 1000)
        (log (float n 1.0d0))
        (let* ((shift (- len 53))
               (mantissa (float (ash n (- shift)) 1.0d0)))
          (+ (log mantissa) (* shift 0.6931471805599453d0))))))

(defun bignum-ratio-to-float (num den)
  "Calculates (num / den) as a double-float with precise round-to-nearest."
  (declare (type integer num den))
  (let* ((len-num (integer-length num))
         (len-den (integer-length den)))
    (let* ((k (- (+ len-den 60) len-num))
           (k (if (minusp k) 0 k))
           (scaled-num (ash num k))
           (q (round scaled-num den))) ; Exact round-to-nearest even
      (scale-float (float q 1.0d0) (- k)))))

(defun solve (&optional (n 10000))
  (declare (type fixnum n))
  (let* ((size (1+ (* 2 n)))
         (max-b (make-array size :element-type 'double-float :initial-element 0.0d0))
         (f (make-array size :element-type 'double-float :initial-element 0.0d0))
         (D-arr (make-array size :element-type 'integer :initial-element 1))
         (log-D (make-array size :element-type 'double-float :initial-element 0.0d0)))
    
    (iterate (for i from 0 to (1- size))
      (setf (aref max-b i) (float i 1.0d0)))
      
    (setf (aref D-arr 1) 1)
    (setf (aref log-D 1) 0.0d0)

    (format t "Calculating E(~A) using Correct Initial Move Constraint...~%" n)
    
    (iterate (for s from 1 to (* 2 n))
      (declare (type fixnum s))
      
      ;; Pascals Triangle property: B(s-1, w-1) = B(s-2, w-1) + B(s-2, w-2)
      (setf (aref D-arr s) (ash 1 (1- s)))
      (setf (aref log-D s) (* (1- s) 0.6931471805599453d0))
      
      (iterate (for w from (1- s) downto 2)
        (declare (type fixnum w))
        (let ((new-val (+ (aref D-arr w) (aref D-arr (1- w)))))
          (setf (aref D-arr w) new-val)
          (setf (aref log-D w) (bignum-log new-val))))
      
      (setf (aref f 0) (float s 1.0d0))
      (setf (aref f s) 0.0d0)
      
      (when (> s 1)
        (let ((w-star 1)
              (min-log 1d300))
          
          ;; 1. 最適な停止境界 w* を探す
          (iterate (for w from 1 to (1- s))
            (declare (type fixnum w))
            (let* ((pc (aref max-b (- s w)))
                   (num (- (float s 1.0d0) pc)))
              (declare (type double-float pc num))
              
              (when (<= num 0.0d0) (setf num 1d-300))
              
              (let ((log-r (- (log num) (aref log-D w))))
                (declare (type double-float log-r))
                (when (< log-r min-log)
                  (setf min-log log-r)
                  (setf w-star w)))))
            
          ;; 2. 閉じた漸化式で期待値を確定する
          (let* ((pc-star (aref max-b (- s w-star)))
                 (num (- (float s 1.0d0) pc-star))
                 (D-star (aref D-arr w-star)))
            (declare (type double-float pc-star num))
            
            (iterate (for w from 1 to (1- s))
              (declare (type fixnum w))
              (if (<= w w-star)
                  (let ((ratio (bignum-ratio-to-float (aref D-arr w) D-star)))
                    (declare (type double-float ratio))
                    (setf (aref f w) (- (float s 1.0d0) (* num ratio))))
                  (setf (aref f w) (aref max-b (- s w))))))))
                  
      ;; 3. 過去の最大期待値のキャッシュを更新
      (iterate (for w from 1 to (1- s))
        (declare (type fixnum w))
        (let ((b (- s w)))
          (when (> (aref f w) (aref max-b b))
            (setf (aref max-b b) (aref f w)))))
            
      (when (zerop (mod s 2000))
        (format t "Processed Total Sheep S = ~A~%" s)))
            
    (format t "Done.~%")
    ;; 真のショートカット：初期状態 (n,n) からはまず1回羊が鳴いて遷移するため、
    ;; その直後の状態である (n+1, n-1) と (n-1, n+1) の期待値の平均を取る
    (format nil "~,6F" (+ (* 0.5d0 (aref f (1+ n))) (* 0.5d0 (aref f (1- n)))))))


#+| Do it | (solve )
#|------------------------------------------------------------|
Timing the evaluation of (solve)
Calculating E(10000) using Correct Initial Move Constraint...
Processed Total Sheep S = 2000
Processed Total Sheep S = 4000
Processed Total Sheep S = 6000
Processed Total Sheep S = 8000
Processed Total Sheep S = 10000
Processed Total Sheep S = 12000
Processed Total Sheep S = 14000
Processed Total Sheep S = 16000
Processed Total Sheep S = 18000
Processed Total Sheep S = 20000
Done.

User time    =  0:29:20.711
System time  =  0:03:18.049
Elapsed time =  0:34:33.473
Allocation   = 942472969896 bytes
72095007 Page faults
GC time      =  0:05:45.1946
 |------------------------------------------------------------|#
;;→ "19823.542204"
:ok
