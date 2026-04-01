;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3.1-pro
(cl:in-package cl-user)
(defpackage #:project-euler-0554-prog (:use cl) (:export #:solve))
(in-package #:project-euler-0554-prog)

(defparameter $modulo-p 100000007)

(defun get-fibs (max-index)
  "Generates Fibonacci numbers up to max-index using prog."
  (prog ((fibs (make-array (1+ max-index) :initial-element 0))
         (index 3))
        (setf (aref fibs 1) 1)
        (setf (aref fibs 2) 1)
      L-START
        (when (> index max-index) (go L-END))
        (setf (aref fibs index) (+ (aref fibs (1- index)) (aref fibs (- index 2))))
        (incf index)
        (go L-START)
      L-END
        (return fibs)))

(defun valid-lucas-digits? (number modulo-val)
  "Checks if all base-P digits of NUMBER are <= P/2 using prog."
  (prog ((temp number)
         (half-p (floor modulo-val 2))
         quotient remainder)
      L-START
        (when (<= temp 0) (return t))
        (setf (values quotient remainder) (floor temp modulo-val))
        (when (> remainder half-p) (return nil))
        (setq temp quotient)
        (go L-START)))

(defun get-required-ks (fibs modulo-val)
  "Extracts necessary P-adic digits using prog state machines."
  (prog ((k-list (list 0))
         (index 2)
         fib-val temp quotient remainder)
      L-OUTER
        (when (> index 90) (go L-END))
        (setq fib-val (aref fibs index))
        (when (valid-lucas-digits? fib-val modulo-val)
          (setq temp fib-val))
      L-INNER
        (when (<= temp 0) (go L-NEXT))
        (setf (values quotient remainder) (floor temp modulo-val))
        (push remainder k-list)
        (push (* 2 remainder) k-list)
        (setq temp quotient)
        (go L-INNER)
      L-NEXT
        (incf index)
        (go L-OUTER)
      L-END
        (return (sort (remove-duplicates k-list) #'<))))

(defun compute-factorials (k-list modulo-val)
  "Computes factorial values dynamically using prog."
  (prog ((fact-hash (make-hash-table :test 'eql))
         (current-fact 1)
         (current-index 0)
         (rest-k k-list)
         k i)
        (setf (gethash 0 fact-hash) 1)
      L-LIST
        (when (null rest-k) (return fact-hash))
        (setq k (car rest-k))
        (setq rest-k (cdr rest-k))
        (when (<= k 0) (go L-LIST))
        (setq i (1+ current-index))
      L-FACT
        (when (> i k) (go L-FACT-END))
        (setq current-fact (mod (* current-fact i) modulo-val))
        (incf i)
        (go L-FACT)
      L-FACT-END
        (setf (gethash k fact-hash) current-fact)
        (setq current-index k)
        (go L-LIST)))

(defun power-mod (base exp modulo-val)
  "Computes modular exponentiation via prog."
  (prog ((result 1)
         (b (mod base modulo-val))
         (e exp))
      L-START
        (when (<= e 0) (return result))
        (when (oddp e) (setq result (mod (* result b) modulo-val)))
        (setq b (mod (* b b) modulo-val))
        (setq e (ash e -1))
        (go L-START)))

(defun mod-inverse (number modulo-val)
  (power-mod number (- modulo-val 2) modulo-val))

(defun compute-centaurs (fib-val modulo-val fact-hash)
  "Evaluates C(F) utilizing linear state jumps."
  (prog ((binom-product 1)
         temp quotient remainder num den den-inv den2-inv binom
         n-mod n2-mod term1 term2 term3 poly-part result)
        
        (unless (valid-lucas-digits? fib-val modulo-val)
          (setq binom-product 0)
          (go L-POLY))
          
        (setq temp fib-val)
      L-LUCAS
        (when (<= temp 0) (go L-POLY))
        (setf (values quotient remainder) (floor temp modulo-val))
        (setq num (gethash (* 2 remainder) fact-hash))
        (setq den (gethash remainder fact-hash))
        (setq den-inv (mod-inverse den modulo-val))
        (setq den2-inv (mod (* den-inv den-inv) modulo-val))
        (setq binom (mod (* num den2-inv) modulo-val))
        (setq binom-product (mod (* binom-product binom) modulo-val))
        (setq temp quotient)
        (go L-LUCAS)
        
      L-POLY
        (setq n-mod (mod fib-val modulo-val))
        (setq n2-mod (mod (* n-mod n-mod) modulo-val))
        (setq term1 (mod (* 8 binom-product) modulo-val))
        (setq term2 (mod (* 3 n2-mod) modulo-val))
        (setq term3 (mod (* 2 n-mod) modulo-val))
        (setq poly-part (mod (+ term2 term3 7) modulo-val))
        (setq result (mod (- term1 poly-part) modulo-val))
        (return result)))

(defun solve-euler-0554 ()
  (prog ((modulo-val $modulo-p)
         (fibs (get-fibs 90))
         (total-sum 0)
         (index 2)
         k-list fact-hash fib-val c-val)
        
        (setq k-list (get-required-ks fibs modulo-val))
        (setq fact-hash (compute-factorials k-list modulo-val))
        
      L-START
        (when (> index 90) (return total-sum))
        (setq fib-val (aref fibs index))
        (setq c-val (compute-centaurs fib-val modulo-val fact-hash))
        (setq total-sum (mod (+ total-sum c-val) modulo-val))
        (incf index)
        (go L-START)))

(defun solve ()
  (format t "--- Mathematical Grounding Validation (PROG Edition) ---~%")
  (prog ((modulo-val $modulo-p)
         (k-list (sort (remove-duplicates (list 0 1 2 2 4 10 20)) #'<))
         fact-hash ans)
        (setq fact-hash (compute-factorials k-list modulo-val))
        (format t "Testing C(1)... Expected: 4, Got: ~A~%" (compute-centaurs 1 modulo-val fact-hash))
        (format t "Testing C(2)... Expected: 25, Got: ~A~%" (compute-centaurs 2 modulo-val fact-hash))
        (format t "Testing C(10)... Expected: 1477721, Got: ~A~%" (compute-centaurs 10 modulo-val fact-hash))
        (format t "-----------------------------------------~%")
        (format t "Solving for Sum C(F_i) i=2..90 mod 10^8+7...~%")
        (setq ans (solve-euler-0554))
        (format t "Answer modulo 10^8+7: ~A~%" ans)
        (return ans)))


#+| Do it | (solve )
#|------------------------------------------------------------|
Timing the evaluation of (solve)
--- Mathematical Grounding Validation (PROG Edition) ---
Testing C(1)... Expected: 4, Got: 4
Testing C(2)... Expected: 25, Got: 25
Testing C(10)... Expected: 1477721, Got: 1477721
-----------------------------------------
Solving for Sum C(F_i) i=2..90 mod 10^8+7...
Answer modulo 10^8+7: 89539872

User time    =        1.540
System time  =        0.020
Elapsed time =        1.508
Allocation   = 126696 bytes
326 Page faults
GC time      =        0.000
 |------------------------------------------------------------|#
;;→ 89539872
:ok