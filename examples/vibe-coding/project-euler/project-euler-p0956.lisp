;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3-flash-preview
(cl:in-package cl-user)
(defpackage #:project-euler-0956 (:use cl iterate alexandria) (:export #:solve))
(in-package #:project-euler-0956)


(defun power (a b m)
  "Modular exponentiation: a^b mod m"
  (let ((res 1)
        (a (mod a m)))
    (iterate (while (> b 0))
      (when (oddp b) (setf res (mod (* res a) m)))
      (setf a (mod (* a a) m))
      (setf b (ash b -1)))
    res))

(defun inverse (a m)
  "Modular inverse using Extended Euclidean Algorithm"
  (multiple-value-bind (g x y) (gcd-extended a m)
    (declare (ignore g y))
    (mod x m)))

(defun gcd-extended (a b)
  (if (zerop a)
      (values b 0 1)
      (multiple-value-bind (g x1 y1) (gcd-extended (mod b a) a)
        (values g (- y1 (* (truncate b a) x1)) x1))))

(defun get-primes (n)
  "Sieve of Eratosthenes to find primes up to n"
  (let ((sieve (make-array (1+ n) :element-type 'bit :initial-element 1)))
    (setf (bit sieve 0) 0 (bit sieve 1) 0)
    (iterate (for i from 2 to (floor (sqrt n)))
      (when (= (bit sieve i) 1)
        (iterate (for j from (* i i) to n by i)
          (setf (bit sieve j) 0))))
    (iterate (for i from 2 to n)
      (when (= (bit sieve i) 1) (collect i)))))

(defun count-factors (k p)
  "Count the number of times prime p divides k"
  (iterate (while (and (> k 0) (zerop (mod k p))))
    (counting t)
    (setf k (/ k p))))

(defun calc-vp (p n)
  "Calculate the exponent of prime p in n-superduperfactorial"
  (iterate (for k from 1 to n)
    (let ((vk (count-factors k p)))
      (when (> vk 0)
        (let ((count (/ (* (- n k -1) (- n k -2)) 2)))
          (sum (* vk count)))))))

(defun find-primitive-root (m)
  "Find a primitive root of prime m"
  (let ((phi (1- m))
        ;; Factors of 999,999,000 = 2^3 * 3^3 * 5^3 * 7 * 11 * 13 * 37
        (factors '(2 3 5 7 11 13 37)))
    (iterate (for g from 2)
      (when (iterate (for f in factors)
              (always (/= (power g (/ phi f) m) 1)))
        (return g)))))

(defun solve ()
  (let* ((n 1000)
         (m 1000)
         (mod-val 999999001)
         (primes (get-primes n))
         (vps (mapcar (lambda (p) (calc-vp p n)) primes))
         (g (find-primitive-root mod-val))
         ;; omega is a primitive m-th root of unity in Z_mod-val
         (omega (power g (/ (1- mod-val) m) mod-val))
         (total-sum 0))
    
    (format t "Pre-computation finished. Starting main loop...~%")
    
    (iterate (for j from 0 below m)
      (let ((x (power omega j mod-val))
            (fx 1))
        (iterate (for p in primes) (for vp in vps)
          (let* ((px (mod (* p x) mod-val))
                 (poly (if (= px 1)
                           (mod (1+ vp) mod-val)
                           (let ((num (mod (1- (power px (1+ vp) mod-val)) mod-val))
                                 (den (mod (1- px) mod-val)))
                             (mod (* num (inverse den mod-val)) mod-val)))))
            (setf fx (mod (* fx poly) mod-val))))
        (setf total-sum (mod (+ total-sum fx) mod-val))
        (when (zerop (mod j 100))
          (format t "Progress: ~D/1000~%" j))))
    
    (let ((ans (mod (* total-sum (inverse m mod-val)) mod-val)))
      (format t "Result: ~D~%" ans)
      ans)))


#+| Do it | (solve )
#|------------------------------------------------------------|
Timing the evaluation of (solve)
Pre-computation finished. Starting main loop...
Progress: 0/1000
Progress: 100/1000
Progress: 200/1000
Progress: 300/1000
Progress: 400/1000
Progress: 500/1000
Progress: 600/1000
Progress: 700/1000
Progress: 800/1000
Progress: 900/1000
Result: 882086212

User time    =        0.303
System time  =        0.015
Elapsed time =        0.215
Allocation   = 1992296 bytes
3891 Page faults
GC time      =        0.000
 |------------------------------------------------------------|#
;;→ 882086212
:ok
