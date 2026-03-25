;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3.5-pro
(cl:in-package cl-user)
(defpackage #:project-euler-0922 (:use cl iterate alexandria) (:export #:solve))
(in-package #:project-euler-0922)

(defconstant $modulo 1000000007)

(defun make-u64-array (size &key (initial-element 0))
  (make-array size :element-type '(unsigned-byte 64) :initial-element initial-element))

(defun make-u64-2d-array (rows cols &key (initial-element 0))
  (make-array (list rows cols) :element-type '(unsigned-byte 64) :initial-element initial-element))

(defun power-mod (base exp m)
  "Calculates (base^exp) modulo m efficiently."
  (let ((res 1)
        (b (mod base m)))
    (iterate (while (> exp 0))
      (when (oddp exp)
        (setf res (mod (* res b) m)))
      (setf b (mod (* b b) m))
      (setf exp (ash exp -1)))
    res))

(defun mod-inverse (n m)
  "Fermat's little theorem for modular inverse."
  (power-mod n (- m 2) m))

(defun fwht (a)
  "In-place Fast Walsh-Hadamard Transform."
  (let* ((n (length a)))
    (iterate (with len = 1)
      (while (< len n))
      (let ((step (* 2 len)))
        (iterate (for i from 0 below n by step)
          (iterate (for j from 0 below len)
            (let* ((u (aref a (+ i j)))
                   (v (aref a (+ i j len)))
                   (sum (mod (+ u v) $modulo))
                   (diff (mod (+ (- u v) $modulo) $modulo)))
              (setf (aref a (+ i j)) sum)
              (setf (aref a (+ i j len)) diff)))))
      (incf len len))
    a))

(defun inv-fwht (a inv-n)
  "Inverse Fast Walsh-Hadamard Transform."
  (fwht a)
  (let ((n (length a)))
    (iterate (for i from 0 below n)
      (setf (aref a i) (mod (* (aref a i) inv-n) $modulo))))
  a)

(defun poly-mul (a b)
  "Multiplies two polynomials a and b modulo 10^9+7."
  (let* ((la (length a))
         (lb (length b))
         (lc (+ la lb -1))
         (c (make-u64-array lc)))
    (iterate (for i from 0 below la)
      (let ((u (aref a i)))
        (when (> u 0)
          (iterate (for j from 0 below lb)
            (let ((v (aref b j)))
              (when (> v 0)
                (setf (aref c (+ i j))
                      (mod (+ (aref c (+ i j)) (* u v)) $modulo))))))))
    c))

(defun poly-power (poly exp)
  "Computes polynomial^exp using doubling (repeated squaring)."
  (let ((res (make-u64-array 1 :initial-element 1))
        (base poly))
    (iterate (while (> exp 0))
      (when (oddp exp)
        (setf res (poly-mul res base)))
      (setf exp (ash exp -1))
      (when (> exp 0)
        (setf base (poly-mul base base))))
    res))

(defun solve-for (m-val w-val)
  "Solves the staircase game combination logic for m staircases up to weight w."
  (let* ((modulo $modulo)
         (max-d (- w-val 2))
         (max-k (- w-val 3))
         (fwht-len 1))
    ;; Find the smallest power of 2 greater than max-k for FWHT
    (iterate (while (<= fwht-len max-k))
      (setf fwht-len (* fwht-len 2)))
    
    (let ((p1 (make-u64-2d-array (+ (* 2 max-d) 1) fwht-len)))
      ;; 1. Initialize P1 based on Valid (D, K) counts
      (iterate (for d from (- max-d) to max-d)
        (iterate (for k from 0 to max-k)
          (let ((count (max 0 (floor (- w-val k 1 (abs d)) 2))))
            (setf (aref p1 (+ d max-d) k) count))))
      
      ;; 2. Perform FWHT on K dimension
      (iterate (for d-idx from 0 to (* 2 max-d))
        (let ((arr (make-u64-array fwht-len)))
          (iterate (for i from 0 below fwht-len)
            (setf (aref arr i) (aref p1 d-idx i)))
          (fwht arr)
          (iterate (for i from 0 below fwht-len)
            (setf (aref p1 d-idx i) (aref arr i)))))
      
      (let ((res-hat (make-array fwht-len)))
        ;; 3. For each frequency xi, exponentiate the D-polynomial to power m
        (iterate (for xi from 0 below fwht-len)
          (let ((poly (make-u64-array (+ (* 2 max-d) 1))))
            (iterate (for d-idx from 0 to (* 2 max-d))
              (setf (aref poly d-idx) (aref p1 d-idx xi)))
            (setf (aref res-hat xi) (poly-power poly m-val))))
            
        (let* ((res-d-len (length (aref res-hat 0)))
               (res (make-u64-2d-array res-d-len fwht-len)))
          (iterate (for xi from 0 below fwht-len)
            (let ((poly (aref res-hat xi)))
              (iterate (for d-idx from 0 below res-d-len)
                (setf (aref res d-idx xi) (aref poly d-idx)))))
                
          ;; 4. Inverse FWHT to return to K domain
          (let ((inv-n (mod-inverse fwht-len modulo)))
            (iterate (for d-idx from 0 below res-d-len)
              (let ((arr (make-u64-array fwht-len)))
                (iterate (for i from 0 below fwht-len)
                  (setf (aref arr i) (aref res d-idx i)))
                (inv-fwht arr inv-n)
                (iterate (for i from 0 below fwht-len)
                  (setf (aref res d-idx i) (aref arr i))))))
                  
          ;; 5. Aggregate final winning states
          (let ((ans 0)
                (center (* m-val max-d)))
            (iterate (for d-idx from 0 below res-d-len)
              (let ((real-d (- d-idx center)))
                (if (> real-d 0)
                    (iterate (for k from 0 below fwht-len)
                      (setf ans (mod (+ ans (aref res d-idx k)) modulo)))
                    (when (= real-d 0)
                      (iterate (for k from 1 below fwht-len)
                        (setf ans (mod (+ ans (aref res d-idx k)) modulo)))))))
            ans))))))

(defun solve ()
  (format t "Validating with known small value R(3, 9)...~%")
  (let ((test-ans (solve-for 3 9)))
    (format t "R(3, 9) = ~A (Expected 314104)~%" test-ans)
    (assert (= test-ans 314104)))
  
  (format t "Computing R(8, 64)...~%")
  (let ((final-ans (solve-for 8 64)))
    (format t "R(8, 64) mod 10^9+7 = ~A~%" final-ans)
    final-ans))

#+| Do it | (project-euler-0922:solve)
#|------------------------------------------------------------|
Timing the evaluation of (solve)
Validating with known small value R(3, 9)...
R(3, 9) = 314104 (Expected 314104)
Computing R(8, 64)...
R(8, 64) mod 10^9+7 = 858945298

User time    =        0.520
System time  =        0.015
Elapsed time =        0.476
Allocation   = 2729496 bytes
546 Page faults
GC time      =        0.000
 |------------------------------------------------------------|#
;;→ 858945298
:ok