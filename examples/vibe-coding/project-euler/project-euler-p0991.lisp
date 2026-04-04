;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3.1-pro
(cl:in-package cl-user)
(defpackage #:project-euler-0991 (:use cl iterate alexandria) (:export #:solve))
(in-package #:project-euler-0991)

(defmacro optimized-code-p (boole)
  (typecase boole
    (null nil)
    (T `(declaim (optimize (speed 3) (safety 0) (debug 0) #+lispworks (hcl:fixnum-safety 0))))))

(optimized-code-p T)

(defun solve ()
  (let ((max-sum 10000000)
        (c1 (/ (- 5d0 (sqrt 21d0)) 2d0))
        (c2 (- 2d0 (sqrt 3d0)))
        (c3 (+ 2d0 (sqrt 3d0)))
        (total-sum 0))
    (declare (type fixnum max-sum)
             (type double-float c1 c2 c3))
    
    (iter (for y from 1 to 3162)
          (declare (type fixnum y))
          (when (= (mod y 500) 0)
            (format t "Debug: Processing Y = ~D~%" y))
          
          ;; 第一の解領域: (5 - sqrt(21)) / 2 < D / Y < 2 - sqrt(3)
          (let ((d-min1 (1+ (floor (* y c1))))
                (d-max1 (floor (* y c2))))
            (declare (type fixnum d-min1 d-max1))
            (iter (for d from d-min1 to d-max1)
                  (declare (type fixnum d))
                  (when (= (gcd y d) 1)
                    (let ((s (- (* 5 d y) (* d d))))
                      (declare (type fixnum s))
                      (when (<= s max-sum)
                        (let ((k (floor max-sum s)))
                          (declare (type fixnum k))
                          (incf total-sum (* s (floor (* k (1+ k)) 2)))))))))
          
          ;; 第二の解領域: 2 + sqrt(3) < D / Y < 4
          (let ((d-min2 (1+ (floor (* y c3))))
                (d-max2 (1- (* 4 y))))
            (declare (type fixnum d-min2 d-max2))
            (iter (for d from d-min2 to d-max2)
                  (declare (type fixnum d))
                  (when (= (gcd y d) 1)
                    (let ((s (- (* 5 d y) (* d d))))
                      (declare (type fixnum s))
                      (when (<= s max-sum)
                        (let ((k (floor max-sum s)))
                          (declare (type fixnum k))
                          (incf total-sum (* s (floor (* k (1+ k)) 2))))))))))
    
    (format t "Debug: Finished. Total Sum = ~D~%" total-sum)
    total-sum))


#+| Do it | (solve )
#|------------------------------------------------------------|
Timing the evaluation of (solve)
Debug: Processing Y = 500
Debug: Processing Y = 1000
Debug: Processing Y = 1500
Debug: Processing Y = 2000
Debug: Processing Y = 2500
Debug: Processing Y = 3000
Debug: Finished. Total Sum = 23871972654940

User time    =        0.352
System time  =        0.018
Elapsed time =        0.260
Allocation   = 2148576 bytes
3861 Page faults
GC time      =        0.000
 |------------------------------------------------------------|#
;;→ 23871972654940
:ok