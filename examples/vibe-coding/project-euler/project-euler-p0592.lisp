;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3.5-pro
(cl:in-package cl-user)
(defpackage #:project-euler-0592 (:use cl iterate alexandria) (:export #:solve))
(in-package #:project-euler-0592)

(defparameter *M* 281474976710656) ; 16^12 = 2^48

(declaim (inline mod-M))
(defun mod-M (x)
  (mod x *M*))

(defparameter *C* (make-array '(48 48) :element-type 'integer :initial-element 0))

(defun init-C ()
  "Precompute Binomial Coefficients modulo 2^48"
  (iterate (for i from 0 to 47)
    (setf (aref *C* i 0) 1)
    (iterate (for j from 1 to i)
      (setf (aref *C* i j) (mod-M (+ (aref *C* (1- i) (1- j))
                                     (aref *C* (1- i) j)))))))

(defun poly-mul (p q)
  "Multiply two polynomials modulo 2^48, keeping up to degree 47"
  (let ((res (make-array 48 :element-type 'integer :initial-element 0)))
    (iterate (for i from 0 to 47)
      (let ((p-i (aref p i)))
        (unless (zerop p-i)
          (iterate (for j from 0 to (- 47 i))
            (setf (aref res (+ i j))
                  (mod-M (+ (aref res (+ i j))
                            (* p-i (aref q j)))))))))
    res))

(defun poly-shift (p a)
  "Calculate P(x+a) modulo 2^48, using precomputed binomial coefficients"
  (let ((res (make-array 48 :element-type 'integer :initial-element 0))
        (a-mod (mod-M a)))
    (iterate (for i from 0 to 47)
      (let ((c (aref p i)))
        (unless (zerop c)
          (let ((a-pow 1))
            (iterate (for j from i downto 0)
              (setf (aref res j)
                    (mod-M (+ (aref res j)
                              (* c (mod-M (* (aref *C* i j) a-pow))))))
              (setf a-pow (mod-M (* a-pow a-mod))))))))
    res))

(defun poly-mul-x+c (p c)
  "Calculate P(x) * (x + c) modulo 2^48"
  (let ((res (make-array 48 :element-type 'integer :initial-element 0))
        (c-mod (mod-M c)))
    (iterate (for i from 0 to 47)
      (let ((p-i (aref p i)))
        (unless (zerop p-i)
          (when (< (1+ i) 48)
            (setf (aref res (1+ i)) (mod-M (+ (aref res (1+ i)) p-i))))
          (setf (aref res i) (mod-M (+ (aref res i) (* p-i c-mod)))))))
    res))

(defun make-poly-1 ()
  (let ((res (make-array 48 :element-type 'integer :initial-element 0)))
    (setf (aref res 0) 1)
    res))

(defparameter *poly-memo* nil)

(defun poly-A (m)
  "Calculate A_m(x) = product_{i=1}^m (x + 2i - 1) using divide and conquer"
  (or (gethash m *poly-memo*)
      (setf (gethash m *poly-memo*)
            (cond
              ((= m 0) (make-poly-1))
              ((oddp m)
               (poly-mul-x+c (poly-A (1- m)) (- (* 2 m) 1)))
              (t
               (let* ((k (ash m -1))
                      (pk (poly-A k))
                      (pk-shift (poly-shift pk (* 2 k))))
                 (poly-mul pk pk-shift)))))))

(defun factorial-N (n)
  (let ((res 1))
    (iterate (for i from 1 to n)
      (setf res (* res i)))
    res))

(defun solve ()
  (init-C)
  ;; Memoization table must be cleared and bound dynamically or globally 
  ;; for safety across multiple executions in the same Lisp image
  (setf *poly-memo* (make-hash-table :test 'equal))
  
  (format t "Calculating f(20!)...~%")
  
  (let* ((K (factorial-N 20))
         (nu2 (- K (logcount K)))    ;; Legendre's formula: nu_2(K!) = K - popcount(K)
         (Z (ash nu2 -2))            ;; Total trailing 16-base zeros = floor(nu_2 / 4)
         (R (- nu2 (* 4 Z)))         ;; Remaining powers of 2
         (ans 1))
    
    (iterate (for x initially K then (ash x -1))
             (while (> x 0))
             (let* ((m (ceiling x 2))
                    (poly (poly-A m))
                    (f-odd (aref poly 0))) ; The constant term evaluates A_m(0)
               (setf ans (mod-M (* ans f-odd)))))
    
    ;; Final assembly: Odd_part * 2^R modulo 2^48
    (let ((final-ans (mod-M (* ans (ash 1 R)))))
      (format t "Done. The last twelve hex digits are:~%")
      (format nil "~12,'0X" final-ans))))


#+| Do it | (solve )
#|------------------------------------------------------------|
Timing the evaluation of (solve)
Calculating f(20!)...
Done. The last twelve hex digits are:

User time    =        0.076
System time  =        0.012
Elapsed time =        0.056
Allocation   = 7679664 bytes
3592 Page faults
GC time      =        0.002
 |------------------------------------------------------------|#
;;→ "13415DF2BE9C"
:ok