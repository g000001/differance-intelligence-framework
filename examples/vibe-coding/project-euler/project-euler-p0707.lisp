;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3-flash-preview
(cl:in-package cl-user)
(defpackage #:project-euler-0707 (:use cl iterate alexandria) (:export #:solve))
(in-package #:project-euler-0707)

(defmacro optimized-code-p (boole)
  (typecase boole
    (null nil)
    (T `(declaim (optimize (speed 3) (safety 0) (debug 0))))))

(optimized-code-p T)



(defun poly-mul (a b)
  "GF(2)上での多項式乗算 (bignumビット演算)"
  (declare (type unsigned-byte a b))
  (let ((res 0))
    (iterate (for i from 0 to (integer-length b))
      (when (logbitp i b)
        (setq res (logxor res (ash a i)))))
    res))

(defun poly-mod (a p-mod)
  "GF(2)上での多項式剰余 (ユークリッド除算)"
  (declare (type unsigned-byte a p-mod))
  (let ((deg-p (1- (integer-length p-mod))))
    (iterate (for i from (integer-length a) downto (1+ deg-p))
      (when (logbitp (1- i) a)
        (setq a (logxor a (ash p-mod (- (1- i) deg-p))))))
    a))

(defun poly-gcd (a b)
  "GF(2)上での多項式GCD"
  (declare (type unsigned-byte a b))
  (iterate (while (not (zerop b)))
    (let ((rem (poly-mod a b)))
      (setq a b
            b rem)))
  a)

(defun mat-mul-sym (A B p-mod)
  "対称行列の積 (modulo p-mod)"
  (let ((a11 (first A)) (a12 (second A)) (a22 (third A))
        (b11 (first B)) (b12 (second B)) (b22 (third B)))
    (list (poly-mod (logxor (poly-mul a11 b11) (poly-mul a12 b12)) p-mod)
          (poly-mod (logxor (poly-mul a11 b12) (poly-mul a12 b22)) p-mod)
          (poly-mod (logxor (poly-mul a12 b12) (poly-mul a22 b22)) p-mod))))

(defun get-P (w)
  "パスグラフの特性多項式 P_w(x) を生成"
  (let ((p0 1)
        (p1 2)) ; 2 represents x
    (iterate (for i from 2 to w)
      (let ((p2 (logxor (ash p1 1) p0)))
        (setq p0 p1
              p1 p2)))
    p1))

(defun power-mod (base exp m)
  (let ((res 1)
        (b (mod base m))
        (e exp))
    (iterate (while (> e 0))
      (when (oddp e)
        (setq res (mod (* res b) m)))
      (setq b (mod (* b b) m))
      (setq e (ash e -1)))
    res))

(defun solve-for (w n)
  (let* ((p-mod (get-P w))
         (f1 1)
         (f2 1)
         (A1 (list 3 1 0)) ; M^1 = (z, 1, 0) since z = x+1 -> 3 in binary
         (A2 (list 3 1 0))
         (sum 0))
    
    ;; k=1
    (let* ((d1 (1- (integer-length (poly-gcd (first A1) p-mod))))
           (e1 (mod (- (* w f1) d1) 1000000006)))
      (setq sum (mod (+ sum (power-mod 2 e1 1000000007)) 1000000007)))
      
    ;; k=2
    (when (>= n 2)
      (let* ((d2 (1- (integer-length (poly-gcd (first A2) p-mod))))
             (e2 (mod (- (* w f2) d2) 1000000006)))
        (setq sum (mod (+ sum (power-mod 2 e2 1000000007)) 1000000007))))
        
    ;; k >= 3
    (iterate (for i from 3 to n)
      (let ((f3 (mod (+ f1 f2) 1000000006))
            (A3 (mat-mul-sym A2 A1 p-mod)))
        (let* ((d3 (1- (integer-length (poly-gcd (first A3) p-mod))))
               (e3 (mod (- (* w f3) d3) 1000000006)))
          
          (when (or (= i 3) (= i 5))
            (format t "Intermediate log k=~A: d=~A, F=~A~%" i d3 (power-mod 2 e3 1000000007)))
            
          (setq sum (mod (+ sum (power-mod 2 e3 1000000007)) 1000000007)))
          
        (setq f1 f2
              f2 f3
              A1 A2
              A2 A3)))
    sum))

(defun solve (&optional (w 199) (n 199))
  (format t "Phase 1: Generating characteristic polynomial P_~A(x)...~%" w)
  (let ((ans (solve-for w n)))
    (format t "Phase 2: Result computation complete.~%")
    (format t "Result: S(~A,~A) = ~A~%" w n ans)
    ans))


#+| Do it | (solve )
#|------------------------------------------------------------|
Timing the evaluation of (solve)
Phase 1: Generating characteristic polynomial P_199(x)...
Intermediate log k=3: d=2, F=137435472
Intermediate log k=5: d=4, F=489625830
Phase 2: Result computation complete.
Result: S(199,199) = 652907799

User time    =        0.016
System time  =        0.001
Elapsed time =        0.010
Allocation   = 8664664 bytes
268 Page faults
GC time      =        0.001
 |------------------------------------------------------------|#
;;→ 652907799
:ok