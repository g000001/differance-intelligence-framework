;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3.1-pro
(cl:in-package cl-user)
(defpackage #:project-euler-0350 (:use cl iterate alexandria) (:export #:solve))
(in-package #:project-euler-0350)

(defmacro optimized-code-p (boole)
  (typecase boole
    (null nil)
    (T `(declaim (optimize (speed 3) (safety 0) (debug 0) #+lispworks (hcl:fixnum-safety 0))))))

(optimized-code-p T)

(defun power-mod (base exp m)
  "繰り返し二乗法による O(log n) 計算"
  (declare (type fixnum base m)
           (type integer exp))
  (let ((res 1)
        (b (mod base m))
        (e exp))
    (declare (type fixnum res b) (type integer e))
    (iterate (while (> e 0))
      (when (oddp e)
        (setq res (mod (* res b) m)))
      ;; 64bit(60bit) fixnum空間なら 104060401^2 は余裕で収まるためアロケーションゼロ
      (setq b (mod (* b b) m))
      (setq e (ash e -1)))
    res))

(defun get-coeff (e n m)
  "包除原理による素因数ごとの係数計算"
  (let* ((p1 (power-mod (+ e 1) n m))
         (p2 (power-mod e n m))
         (p3 (power-mod (- e 1) n m)))
    (declare (type fixnum p1 p2 p3))
    (mod (+ p1 p3 (* 2 m) (- (* 2 p2))) m)))

(defun solve (&optional (G #.(expt 10 6)) (L #.(expt 10 12)) (N #.(expt 10 18)))
  (declare (type integer G L N))
  (format t "Phase 1: Initializing configuration...~%")
  
  (let* ((M #.(expt 101 4))
         (K-max (truncate L G)))
    (declare (type fixnum M K-max))
    
    (let ((visited (make-array (1+ K-max) :element-type 'bit :initial-element 0))
          (primes (make-array (1+ K-max) :element-type 'fixnum :fill-pointer 0))
          (h (make-array (1+ K-max) :element-type 'fixnum))
          (i-rest (make-array (1+ K-max) :element-type 'fixnum))
          (lp-e (make-array (1+ K-max) :element-type 'fixnum))
          (e-coeffs (make-array 30 :element-type 'fixnum))) ; 変数名衝突を回避
          
      (format t "Phase 2: Precomputing prime power coefficients...~%")
      (iterate (for idx from 1 to 25)
        (setf (aref e-coeffs idx) (get-coeff idx N M)))
        
      (format t "Phase 3: Linear Sieve up to K_max = ~A...~%" K-max)
      (setf (aref h 1) 1
            (aref i-rest 1) 1
            (aref lp-e 1) 0)
            
      (iterate (for i from 2 to K-max)
        (when (zerop (sbit visited i))
          (vector-push i primes)
          (setf (aref h i) (aref e-coeffs 1)
                (aref i-rest i) 1
                (aref lp-e i) 1))
        (iterate (for p in-vector primes)
          (let ((next (* i p)))
            (declare (type fixnum next))
            (when (> next K-max) (finish))
            (setf (sbit visited next) 1)
            (if (zerop (mod i p))
                (let ((new-e (1+ (aref lp-e i))))
                  (setf (aref lp-e next) new-e
                        (aref i-rest next) (aref i-rest i)
                        (aref h next) (mod (* (aref h (aref i-rest next)) 
                                              (aref e-coeffs new-e)) 
                                           M))
                  (finish))
                (progn
                  (setf (aref lp-e next) 1
                        (aref i-rest next) i
                        (aref h next) (mod (* (aref h i) (aref e-coeffs 1)) M)))))))
                        
      (format t "Phase 4: Aggregating sum for F(~A, ~A, ~A)...~%" G L N)
      (let ((ans 0))
        (declare (type fixnum ans))
        (iterate (for k from 1 to K-max)
          (let ((coeff (- (truncate L k) G -1)))
            (declare (type integer coeff))
            (when (> coeff 0)
              (setq ans (mod (+ ans (* (aref h k) (mod coeff M))) M)))))
              
        (format t "Result: f(~A, ~A, ~A) mod ~A = ~A~%" G L N M ans)
        ans))))

#+| Do it | (solve )
#|------------------------------------------------------------|
Timing the evaluation of (solve)
Phase 1: Initializing configuration...
Phase 2: Precomputing prime power coefficients...
Phase 3: Linear Sieve up to K_max = 1000000...
Phase 4: Aggregating sum for F(1000000, 1000000000000, 1000000000000000000)...
Result: f(1000000, 1000000000000, 1000000000000000000) mod 104060401 = 84664213

User time    =        0.153
System time  =        0.024
Elapsed time =        0.128
Allocation   = 32354280 bytes
10561 Page faults
GC time      =        0.017
 |------------------------------------------------------------|#
;;→ 84664213
:ok