;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3.5-pro
(cl:in-package cl-user)
(defpackage #:project-euler-0339 (:use cl iterate alexandria) (:export #:solve))
(in-package #:project-euler-0339)

(declaim (inline bignum-to-float-parts log-ratio bignum-ratio-to-float))

(defun bignum-to-float-parts (n)
  "Extracts the mantissa and base-2 exponent from a Bignum for overflow-safe math."
  (declare (type integer n))
  (let ((len (integer-length n)))
    (if (<= len 53)
        (values (float n 1.0d0) 0)
        (let* ((shift (- len 53))
               (mantissa (float (ash n (- shift)) 1.0d0)))
          (values mantissa shift)))))

(defun log-ratio (num bignum-den)
  "Calculates log(num / bignum-den) safely avoiding double-float infinity."
  (declare (type double-float num)
           (type integer bignum-den))
  (multiple-value-bind (m s) (bignum-to-float-parts bignum-den)
    (- (log num) (+ (log m) (* s #.(log 2.0d0))))))

(defun bignum-ratio-to-float (num den)
  "Calculates (num / den) as a double-float precisely without creating huge rationals."
  (declare (type integer num den))
  (multiple-value-bind (m-num s-num) (bignum-to-float-parts num)
    (multiple-value-bind (m-den s-den) (bignum-to-float-parts den)
      (scale-float (/ m-num m-den) (- s-num s-den)))))

(defun solve (&optional (n 10000))
  (declare (type fixnum n))
  (let* ((size (1+ (* 2 n)))
         (max-b (make-array size :element-type 'double-float :initial-element 0.0d0))
         (f (make-array size :element-type 'double-float :initial-element 0.0d0))
         (D-arr (make-array size :element-type 'integer :initial-element 1)))
    
    (iterate (for i from 0 to (1- size))
      (setf (aref max-b i) (float i 1.0d0)))

    (format t "Calculating E(~A) using Closed-form Boundary Evaluation...~%" n)
    
    (iterate (for s from 1 to (* 2 n))
      (declare (type fixnum s))
      
      (setf (aref f 0) (float s 1.0d0))
      (setf (aref f s) 0.0d0)
      
      (when (> s 1)
        (let ((w-star 1)
              (min-log 1d300)
              (C 1)
              (D 1))
          
          (setf (aref D-arr 0) 1)
          
          ;; 1. 最適な停止境界 w* を探す
          (iterate (for w from 1 to (1- s))
            (declare (type fixnum w))
            (setf (aref D-arr w) D)
            
            (let* ((pc (aref max-b (- s w)))
                   (num (- (float s 1.0d0) pc)))
              (declare (type double-float pc num))
              
              (when (<= num 0.0d0) (setf num 1d-300))
              
              (let ((log-r (log-ratio num D)))
                (declare (type double-float log-r))
                (if (< log-r min-log)
                    (progn
                      (setf min-log log-r)
                      (setf w-star w))
                    ;; 最小値が更新されなくなり、凸性を越えたら探索を打ち切る
                    (when (> w (+ w-star 5))
                      (finish)))))
            
            ;; 組み合わせと累積和の定数時間更新
            (setf C (floor (* C (- s w)) w))
            (setf D (+ D C)))
            
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
    (format nil "~,6F" (aref f n))))


#+| Do it | (solve )