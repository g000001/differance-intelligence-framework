;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3-flash-preview
(cl:in-package cl-user)
(defpackage #:project-euler-0952 (:use cl iterate alexandria) (:export #:solve))
(in-package #:project-euler-0952)

(defconstant $const-p 1000000007)

(defun make-fixnum-array (size &key (initial-element 0))
  (make-array size :element-type 'fixnum :initial-element initial-element))

(defun build-spf (limit)
  (let ((spf (make-fixnum-array (1+ limit))))
    (iterate (for i from 2 to limit)
      (setf (aref spf i) i))
    (iterate (for i from 2 to (isqrt limit))
      (when (= (aref spf i) i)
        (iterate (for j from (* i i) to limit by i)
          (when (= (aref spf j) j)
            (setf (aref spf j) i)))))
    spf))

(defun get-primes (spf limit)
  (let* ((count (iterate (for i from 2 to limit) (count (= (aref spf i) i))))
         (primes (make-fixnum-array count))
         (idx 0))
    (iterate (for i from 2 to limit)
      (when (= (aref spf i) i)
        (setf (aref primes idx) i)
        (incf idx)))
    primes))

(defun fast-mod-pow (base exp mod-val)
  (let ((res 1)
        (b (mod base mod-val)))
    (iterate
      (while (> exp 0))
      (when (oddp exp)
        (setf res (mod (* res b) mod-val)))
      (setf b (mod (* b b) mod-val))
      (setf exp (ash exp -1)))
    res))

(defun solve (&optional (limit-n 10000000))
  (format t "Building SPF for N=~A...~%" limit-n)
  (let* ((spf (build-spf limit-n))
         (primes (get-primes spf limit-n))
         (max-o-v (make-fixnum-array (1+ limit-n)))
         (exp-m (make-fixnum-array (1+ limit-n))))
    
    (format t "SPF and Primes ready. Prime count: ~A~%" (length primes))
    (format t "Calculating orders and exponent evaluations...~%")
    
    (iterate (for q in-vector primes)
      (if (= q 2)
          (let ((v2 0)
                (k 2))
            (iterate
              (while (<= k limit-n))
              (incf v2 (floor limit-n k))
              (setf k (* k 2)))
            ;; p = 10^9+7 = 3 mod 4, and v_2(p^2-1) = 4. Fixed behavior for V_2.
            (setf (aref exp-m 2) (if (<= v2 1) 0
                                     (if (<= v2 4) 1
                                         (- v2 3)))))
          (let ((o-q (1- q))
                (temp-o (1- q)))
            ;; Calculate o_q (multiplicative order of p mod q)
            (iterate
              (while (> temp-o 1))
              (let ((l (aref spf temp-o)))
                (iterate
                  (while (and (= (mod o-q l) 0)
                              (= (fast-mod-pow $const-p (/ o-q l) q) 1)))
                  (setf o-q (/ o-q l)))
                (iterate
                  (while (= (mod temp-o l) 0))
                  (setf temp-o (/ temp-o l)))))
            
            ;; Prime factorization of o_q and update maximum exponents
            (let ((temp o-q))
              (iterate
                (while (> temp 1))
                (let ((l (aref spf temp))
                      (c 0))
                  (iterate
                    (while (= (mod temp l) 0))
                    (incf c)
                    (setf temp (/ temp l)))
                  (setf (aref max-o-v l) (max (aref max-o-v l) c)))))
            
            ;; Calculate V_q
            (let ((vq 0)
                  (k q))
              (iterate
                (while (<= k limit-n))
                (incf vq (floor limit-n k))
                (setf k (* k q)))
              
              ;; Calculate e_q = V_q - k_q via bounded modular exponentiation
              (if (> vq 0)
                  (let ((m 1)
                        (q-pow q))
                    (iterate
                      (while (< m vq))
                      (let ((next-q-pow (* q-pow q)))
                        (if (= (fast-mod-pow $const-p o-q next-q-pow) 1)
                            (progn
                              (incf m)
                              (setf q-pow next-q-pow))
                            (finish))))
                    (setf (aref exp-m q) (- vq m)))
                  (setf (aref exp-m q) 0))))))
    
    (format t "Computing final answer...~%")
    (let ((ans 1))
      (iterate (for q in-vector primes)
        ;; Overall exponent for prime q is max of its required exponent and its occurrence in o_l
        (let ((exp (max (aref exp-m q) (aref max-o-v q))))
          (when (> exp 0)
            (setf ans (mod (* ans (fast-mod-pow q exp $const-p)) $const-p)))))
      
      (format t "Finished. Answer: ~A~%" ans)
      ans)))


#+| Do it | (solve )
#|------------------------------------------------------------|
Timing the evaluation of (solve)
Building SPF for N=10000000...
SPF and Primes ready. Prime count: 664579
Calculating orders and exponent evaluations...
Computing final answer...
Finished. Answer: 794394453

User time    =        4.438
System time  =        0.098
Elapsed time =        4.421
Allocation   = 477980056 bytes
63832 Page faults
GC time      =        0.004
 |------------------------------------------------------------|#
;;→ 794394453
:ok