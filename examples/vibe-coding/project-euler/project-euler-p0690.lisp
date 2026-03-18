;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3.1-pro
(cl:in-package cl-user)
(defpackage #:project-euler-0690 (:use cl iterate alexandria) (:export #:solve))
(in-package #:project-euler-0690)

(defconstant +mod+ 1000000007)
(defconstant +inv2+ 500000004)

(declaim (inline mod+ mod- mod*))
(defun mod+ (a b) (mod (+ a b) +mod+))
(defun mod- (a b) (mod (+ (- a b) +mod+) +mod+))
(defun mod* (a b) (mod (* a b) +mod+))

(defun mod-inv (n)
  (let ((res 1) (b (mod n +mod+)) (e (- +mod+ 2)))
    (declare (type (unsigned-byte 64) res b e))
    (iterate (while (> e 0))
      (when (oddp e) (setq res (mod* res b)))
      (setq b (mod* b b))
      (setq e (ash e -1)))
    res))

(defun poly-mul (A B limit)
  (let ((res (make-array (1+ limit) :element-type '(unsigned-byte 64) :initial-element 0)))
    (iterate (for i from 0 to limit)
      (when (> (aref A i) 0)
        (iterate (for j from 0 to (- limit i))
          (setf (aref res (+ i j))
                (mod+ (aref res (+ i j)) (mod* (aref A i) (aref B j)))))))
    res))

(defun euler-transform (A limit)
  "Calculates F(x) = exp( sum_{k=1} A(x^k)/k )."
  (let ((F (make-array (1+ limit) :element-type '(unsigned-byte 64) :initial-element 0))
        (s (make-array (1+ limit) :element-type '(unsigned-byte 64) :initial-element 0)))
    (setf (aref F 0) 1)
    (iterate (for n from 1 to limit)
      (let ((term (mod* n (aref A n))))
        (iterate (for j from n to limit by n)
          (setf (aref s j) (mod+ (aref s j) term)))))
    (iterate (for n from 1 to limit)
      (let ((sum 0))
        (iterate (for k from 1 to n)
          (setq sum (mod+ sum (mod* (aref F (- n k)) (aref s k)))))
        (setf (aref F n) (mod* sum (mod-inv n)))))
    F))

(defun solve ()
  (let* ((n 2019)
         (T1 (make-array (1+ n) :element-type '(unsigned-byte 64) :initial-element 1)))
    (setf (aref T1 0) 0) ; T1 = x + x^2 + x^3 + ...
    
    (format t "Transforming Base Trees...~%")
    (let* ((F (euler-transform T1 n))
           (T2 (make-array (1+ n) :element-type '(unsigned-byte 64) :initial-element 0))
           (D (make-array (1+ n) :element-type '(unsigned-byte 64) :initial-element 0))
           (U0 (make-array (1+ n) :element-type '(unsigned-byte 64) :initial-element 0))
           (U1 (make-array (1+ n) :element-type '(unsigned-byte 64) :initial-element 0)))
      
      (iterate (for i from 1 to n)
        (setf (aref T2 i) (aref F (1- i)))
        (setf (aref D i) (mod- (aref T2 i) (aref T1 i))))
        
      (format t "Computing U0 (Diameter <= 3)...~%")
      (iterate (for i from 1 to n)
        (let* ((x4_1mx2 (if (>= i 4) (- i 3) 0))
               (x4_1mxsq (if (and (>= i 4) (evenp i)) 1 0)))
          (setf (aref U0 i)
                (mod+ (aref T1 i)
                      (mod* (mod+ x4_1mx2 x4_1mxsq) +inv2+)))))
                      
      (format t "Computing U1 (Diameter 4)...~%")
      (iterate (for i from 1 to n)
        (let ((x3_1mx2 (if (>= i 3) (- i 2) 0)))
          (setf (aref U1 i)
                (mod- (aref T2 i) (mod+ (aref T1 i) x3_1mx2)))))
                
      (format t "Computing U_ge2 (Diameter >= 5)...~%")
      (let ((Q (make-array (1+ n) :element-type '(unsigned-byte 64) :initial-element 0)))
        (setf (aref Q 0) 1)
        (iterate (for i from 1 to n)
          (let ((sum 0))
            (iterate (for j from 1 to i)
              (setq sum (mod+ sum (mod* (aref T2 j) (aref Q (- i j))))))
            (setf (aref Q i) sum)))
            
        (let* ((D2 (poly-mul D D n))
               (term1 (poly-mul D2 Q n))
               (N2 (make-array (1+ n) :element-type '(unsigned-byte 64) :initial-element 0))
               (Q2 (make-array (1+ n) :element-type '(unsigned-byte 64) :initial-element 0)))
          
          (iterate (for i from 0 to (floor n 2))
            (setf (aref N2 (* 2 i)) (aref D i))
            (setf (aref Q2 (* 2 i)) (aref Q i)))
            
          (let* ((N2_T2 (poly-mul N2 T2 n))
                 (Num2 (make-array (1+ n) :element-type '(unsigned-byte 64) :initial-element 0)))
            (iterate (for i from 0 to n)
              (setf (aref Num2 i) (mod+ (aref N2 i) (aref N2_T2 i))))
              
            (let* ((term2 (poly-mul Num2 Q2 n))
                   (U_ge2 (make-array (1+ n) :element-type '(unsigned-byte 64) :initial-element 0))
                   (U (make-array (1+ n) :element-type '(unsigned-byte 64) :initial-element 0)))
              
              (iterate (for i from 1 to n)
                (setf (aref U_ge2 i) (mod* (mod+ (aref term1 i) (aref term2 i)) +inv2+))
                (setf (aref U i) (mod+ (aref U0 i) (mod+ (aref U1 i) (aref U_ge2 i)))))
                
              (format t "Final Euler Transform for Forests...~%")
              (let ((TomForests (euler-transform U n)))
                (format t "Done.~%")
                (aref TomForests n)))))))))


#+| Do it | (solve )
#|------------------------------------------------------------|
Timing the evaluation of (solve)
Transforming Base Trees...
Computing U0 (Diameter <= 3)...
Computing U1 (Diameter 4)...
Computing U_ge2 (Diameter >= 5)...
Final Euler Transform for Forests...
Done.

User time    =        0.660
System time  =        0.014
Elapsed time =        0.620
Allocation   = 409648 bytes
300 Page faults
GC time      =        0.000
 |------------------------------------------------------------|#
;;→ 415157690
