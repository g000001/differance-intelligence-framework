;;; -*- mode: Lisp; coding: utf-8 -*-
;;; llm-model: gemini-3.1-pro
(cl:in-package cl-user)
(defpackage #:project-euler-0354 (:use cl iterate alexandria) (:export #:solve))
(in-package #:project-euler-0354)

#||
(cl:comment "PE 354 CLIF Logic Definition")
(forall (L)
  (iff (= (B L) 450)
       (exists (M P k Q)
         (and (= (* 3 M) (* L L))
              (<= M (floor (* 25 (expt 10 22)) 3))
              (= M (* P (expt 3 k) (expt Q 2)))
              (is-product-of-primes-1-mod-3 P)
              (has-divisor-profile P 75)
              (is-product-of-primes-2-mod-3 Q)))))
||#


(defvar *limit-p* 35500000) ; p3が到達しうる最大値(約3486万)をカバー
(defvar *limit-q* 1850000)  ; Qの最大値(約183万)をカバー
(defvar *primes-1mod3* (make-array 0 :fill-pointer 0 :adjustable t))
(defvar *c-array* (make-array *limit-q* :element-type 'fixnum :initial-element 0))

(defun precompute ()
  (setf (fill-pointer *primes-1mod3*) 0)
  (fill *c-array* 0)
  
  (format t "Precomputing primes up to ~A...~%" *limit-p*)
  (let ((sieve (make-array *limit-p* :element-type 'bit :initial-element 1)))
    (setf (aref sieve 0) 0
          (aref sieve 1) 0)
    (iterate (for p from 2 below *limit-p*)
      (when (= (aref sieve p) 1)
        (when (= (mod p 3) 1)
          (vector-push-extend p *primes-1mod3*))
        ;; 素数の篩はisqrt(*limit-p*)まで回せば十分
        (when (<= p 5958)
          (iterate (for m from (* p p) below *limit-p* by p)
            (setf (aref sieve m) 0))))))

  (format t "Precomputing valid-Q array up to ~A...~%" *limit-q*)
  (let ((valid-q (make-array *limit-q* :element-type 'bit :initial-element 1)))
    (setf (aref valid-q 0) 0) ; Q=0 は無効
    ;; 3の倍数を排除
    (iterate (for m from 3 below *limit-q* by 3)
      (setf (aref valid-q m) 0))
    ;; p ≡ 1 (mod 3) の倍数を排除
    (iterate (for p in-vector *primes-1mod3*)
      (when (>= p *limit-q*) (finish))
      (iterate (for m from p below *limit-q* by p)
        (setf (aref valid-q m) 0)))
    ;; 累積和を計算
    (iterate (for i from 1 below *limit-q*)
      (setf (aref *c-array* i)
            (+ (aref *c-array* (1- i)) (aref valid-q i))))))

(defun count-s (x-value)
  (iterate
    (with total = 0)
    (with curr-x = x-value)
    (while (> curr-x 0))
    (let ((y-val (isqrt curr-x)))
      (when (= y-val 0) (finish))
      (incf total (aref *c-array* y-val))
      (setf curr-x (floor curr-x 3)))
    (finally (return total))))

(defun search-case-2 (limit-m primes-array)
  (iterate
    (with ans = 0)
    (with n-primes = (length primes-array))
    (for index-i from 0 below n-primes)
    (for p1 = (aref primes-array index-i))
    (for p1-24 = (expt p1 24))
    (while (<= p1-24 limit-m))
    (iterate
      (for index-j from 0 below n-primes)
      (for p2 = (aref primes-array index-j))
      (for p-val = (* p1-24 (expt p2 2)))
      (while (<= p-val limit-m))
      (when (/= index-i index-j)
        (incf ans (count-s (floor limit-m p-val)))))
    (finally (return ans))))

(defun search-case-3 (limit-m primes-array)
  (iterate
    (with ans = 0)
    (with n-primes = (length primes-array))
    (for index-i from 0 below n-primes)
    (for p1 = (aref primes-array index-i))
    (for p1-14 = (expt p1 14))
    (while (<= p1-14 limit-m))
    (iterate
      (for index-j from 0 below n-primes)
      (for p2 = (aref primes-array index-j))
      (for p-val = (* p1-14 (expt p2 4)))
      (while (<= p-val limit-m))
      (when (/= index-i index-j)
        (incf ans (count-s (floor limit-m p-val)))))
    (finally (return ans))))

(defun search-case-4 (limit-m primes-array)
  (iterate
    (with ans = 0)
    (with n-primes = (length primes-array))
    (for index-i from 0 below n-primes)
    (for p1 = (aref primes-array index-i))
    (for p1-4 = (expt p1 4))
    (while (<= p1-4 limit-m))
    (iterate
      (for index-j from (1+ index-i) below n-primes)
      (for p2 = (aref primes-array index-j))
      (for p14-p24 = (* p1-4 (expt p2 4)))
      (while (<= p14-p24 limit-m))
      (iterate
        (for index-k from 0 below n-primes)
        (for p3 = (aref primes-array index-k))
        (for p-val = (* p14-p24 (expt p3 2)))
        (while (<= p-val limit-m))
        (when (and (/= index-k index-i) (/= index-k index-j))
          (incf ans (count-s (floor limit-m p-val))))))
    (finally (return ans))))

(defun solve ()
  (precompute)
  (format t "Precomputation done. Primes 1 mod 3 count: ~A~%" (length *primes-1mod3*))
  
  (let* ((limit-m #.(floor (* 25 (expt 10 22)) 3))
         (total-ans 0)
         (ans2 0)
         (ans3 0)
         (ans4 0))
    (format t "Searching Case 2 (p1^24 * p2^2)...~%")
    (setf ans2 (search-case-2 limit-m *primes-1mod3*))
    (format t "Case 2 count: ~A~%" ans2)
    (incf total-ans ans2)
    
    (format t "Searching Case 3 (p1^14 * p2^4)...~%")
    (setf ans3 (search-case-3 limit-m *primes-1mod3*))
    (format t "Case 3 count: ~A~%" ans3)
    (incf total-ans ans3)
    
    (format t "Searching Case 4 (p1^4 * p2^4 * p3^2)...~%")
    (setf ans4 (search-case-4 limit-m *primes-1mod3*))
    (format t "Case 4 count: ~A~%" ans4)
    (incf total-ans ans4)
    
    (format t "Final Total Count: ~A~%" total-ans)
    total-ans))

#+| Do it | (solve )
#|------------------------------------------------------------|
Timing the evaluation of (solve)
Precomputing primes up to 35500000...
Precomputing valid-Q array up to 1850000...
Precomputation done. Primes 1 mod 3 count: 1087505
Searching Case 2 (p1^24 * p2^2)...
Case 2 count: 2
Searching Case 3 (p1^14 * p2^4)...
Case 3 count: 3763
Searching Case 4 (p1^4 * p2^4 * p3^2)...
Case 4 count: 58061369
Final Total Count: 58065134

User time    =        3.529
System time  =        0.049
Elapsed time =        3.507
Allocation   = 191954216 bytes
11867 Page faults
GC time      =        0.024
 |------------------------------------------------------------|#
;;→ 58065134
:ok