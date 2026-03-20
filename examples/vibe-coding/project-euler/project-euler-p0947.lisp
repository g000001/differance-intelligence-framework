;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3.1-pro
(cl:in-package cl-user)
(defpackage #:project-euler-0947 (:use cl iterate alexandria) (:export #:solve))
(in-package #:project-euler-0947)

#||
(clif-logic
  (formal-problem "Project Euler 947: Fibonacci Periods")
  (invariants
    (pisano-period-bound (<= (pi m) (* 6 m)))
    (multiplicativity-of-C (equal (C d m) (prod_{p^e || m} (C d p^e))))
    (kernel-size-formula (equal (s m) (sum_{d | pi(m)} (* (C d m) (expt d 2) (W (/ (pi m) d))))))
    (w-function (equal (W n) (prod_{q | n} (- 1 (expt q 2)))))
    (period-gcd-reduction (equal (C d p^e) (C (gcd d (pi p^e)) p^e))))
  (optimizations
    (constant-time-kernel-evaluation "Size of kernel evaluated via Smith Normal Form modulo p^e in O(1) after matrix exponentiation.")
    (matrix-pow-elimination "Matrix exponentiation is only executed during precomputation for prime powers, eliminating inner-loop matrix multiplications.")
    (sieve-factorization "SPF (Smallest Prime Factor) array used to factorize numbers in O(log n), replacing trial division.")
    (garbage-collection "Zero allocation in the inner loops. Used global pre-allocated arrays instead of consing.")))
||#

(defconstant $M 1000000)
(defconstant $MOD 999999893)
(defconstant $max-spf 6000005)
(defparameter *spf* (make-array $max-spf :element-type 'fixnum))
(defparameter *pi* (make-array (1+ $M) :element-type 'fixnum))
(defparameter *W* (make-array 0 :element-type 'fixnum)) ; initialized later

(defparameter *factor-p* (make-array 20 :element-type 'fixnum))
(defparameter *factor-c* (make-array 20 :element-type 'fixnum))
(defparameter *factor-p-pi* (make-array 50 :element-type 'fixnum))

(defstruct pe-info
  (divs (make-array 0 :element-type 'fixnum))
  (vals (make-array 0 :element-type 'fixnum)))

(defparameter *pe-info* (make-array (1+ $M) :initial-element nil))

(defun init-spf ()
  (iterate (for i from 0 below $max-spf)
    (setf (aref *spf* i) i))
  (iterate (for i from 2 to (isqrt $max-spf))
    (when (= (aref *spf* i) i)
      (iterate (for j from (* i i) below $max-spf by i)
        (when (= (aref *spf* j) j)
          (setf (aref *spf* j) i))))))

(defun map-divisors (n func)
  (let ((num-factors 0)
        (temp n))
    (declare (type fixnum num-factors temp))
    (iterate (while (> temp 1))
      (let ((p (aref *spf* temp))
            (count 0))
        (declare (type fixnum p count))
        (iterate (while (= 0 (mod temp p)))
          (incf count)
          (setq temp (truncate temp p)))
        (setf (aref *factor-p* num-factors) p)
        (setf (aref *factor-c* num-factors) count)
        (incf num-factors)))
    (labels ((gen (idx current)
               (declare (type fixnum idx current))
               (if (= idx num-factors)
                   (funcall func current)
                   (let* ((p (aref *factor-p* idx))
                          (c (aref *factor-c* idx))
                          (mul 1))
                     (declare (type fixnum p c mul))
                     (iterate (for i from 0 to c)
                       (gen (1+ idx) (* current mul))
                       (setq mul (* mul p)))))))
      (gen 0 1))))

(defun matrix-pow (d m)
  (declare (type fixnum d m))
  (let ((a00 1) (a01 0)
        (a10 0) (a11 1)
        (b00 0) (b01 1)
        (b10 1) (b11 1))
    (declare (type fixnum a00 a01 a10 a11 b00 b01 b10 b11))
    (iterate (while (> d 0))
      (when (oddp d)
        (let ((n00 (mod (+ (* a00 b00) (* a01 b10)) m))
              (n01 (mod (+ (* a00 b01) (* a01 b11)) m))
              (n10 (mod (+ (* a10 b00) (* a11 b10)) m))
              (n11 (mod (+ (* a10 b01) (* a11 b11)) m)))
          (setq a00 n00 a01 n01 a10 n10 a11 n11)))
      (let ((n00 (mod (+ (* b00 b00) (* b01 b10)) m))
            (n01 (mod (+ (* b00 b01) (* b01 b11)) m))
            (n10 (mod (+ (* b10 b00) (* b11 b10)) m))
            (n11 (mod (+ (* b10 b01) (* b11 b11)) m)))
        (setq b00 n00 b01 n01 b10 n10 b11 n11))
      (setq d (ash d -1)))
    (values a00 a01 a10 a11)))

(defun compute-pi-prime (p)
  (cond
    ((= p 2) 3)
    ((= p 5) 20)
    (t
     (let* ((p-val (if (or (= (mod p 5) 1) (= (mod p 5) 4))
                       (- p 1)
                       (* 2 (+ p 1))))
            (d p-val)
            (num-f 0)
            (temp p-val))
       (declare (type fixnum p-val d num-f temp))
       (iterate (while (> temp 1))
         (let ((q (aref *spf* temp)))
           (setf (aref *factor-p-pi* num-f) q)
           (incf num-f)
           (iterate (while (= 0 (mod temp q)))
             (setq temp (truncate temp q)))))
       (iterate (for i from 0 below num-f)
         (let ((q (aref *factor-p-pi* i)))
           (iterate (while (= 0 (mod d q)))
             (let ((next-d (truncate d q)))
               (multiple-value-bind (a00 a01 a10 a11) (matrix-pow next-d p)
                 (if (and (= a00 1) (= a01 0) (= a10 0) (= a11 1))
                     (setq d next-d)
                     (finish)))))))
       d))))

(defun v-p (n p)
  (if (= n 0)
      1000000
      (let ((c 0))
        (iterate (while (= 0 (mod n p)))
          (incf c)
          (setq n (truncate n p)))
        c)))

(defun compute-kernel-size (a00 a01 a10 a11 p e pe)
  (declare (type fixnum a00 a01 a10 a11 p e pe))
  (let* ((a00-mod (mod (- a00 1) pe))
         (a01-mod (mod a01 pe))
         (a10-mod (mod a10 pe))
         (a11-mod (mod (- a11 1) pe)))
    (let ((v00 (v-p a00-mod p))
          (v01 (v-p a01-mod p))
          (v10 (v-p a10-mod p))
          (v11 (v-p a11-mod p)))
      (let ((g (min v00 (min v01 (min v10 v11)))))
        (if (>= g e)
            (* pe pe)
            (let* ((pg (expt p g))
                   (b00 (truncate a00-mod pg))
                   (b01 (truncate a01-mod pg))
                   (b10 (truncate a10-mod pg))
                   (b11 (truncate a11-mod pg))
                   (det (mod (- (* b00 b11) (* b01 b10)) (expt p (- e g)))))
              (if (= det 0)
                  (expt p (+ e g))
                  (expt p (+ (* 2 g) (v-p det p))))))))))

(defun precompute-pe-info (M-val)
  (iterate (for m from 2 to M-val)
    (let ((p (aref *spf* m)))
      (let ((e 0) (temp m) (pe 1))
        (iterate (while (= 0 (mod temp p)))
          (incf e)
          (setq temp (truncate temp p))
          (setq pe (* pe p)))
        (when (= temp 1)
          (let* ((pi-pe (aref *pi* m))
                 (divs-list nil)
                 (vals-list nil))
            (map-divisors pi-pe
              (lambda (d)
                (push d divs-list)
                (multiple-value-bind (a00 a01 a10 a11) (matrix-pow d m)
                  (push (mod (compute-kernel-size a00 a01 a10 a11 p e m) $MOD) vals-list))))
            (setf (aref *pe-info* m)
                  (make-pe-info :divs (make-array (length divs-list) :element-type 'fixnum :initial-contents divs-list)
                                :vals (make-array (length vals-list) :element-type 'fixnum :initial-contents vals-list)))))))))

(defun get-C (pe d)
  (declare (type fixnum pe d))
  (let* ((info (aref *pe-info* pe))
         (divs (pe-info-divs info))
         (vals (pe-info-vals info))
         (len (length divs)))
    (declare (type (simple-array fixnum (*)) divs vals)
             (type fixnum len))
    (iterate (for i from 0 below len)
      (when (= (aref divs i) d)
        (return (aref vals i))))))

(defun solve ()
  "Evaluates S(10^6) modulo 999999893."
  (init-spf)
  (setf (aref *pi* 1) 1)
  (iterate (for m from 2 to $M)
    (let ((p (aref *spf* m)))
      (let ((e 0) (temp m) (pe 1))
        (declare (type fixnum e temp pe))
        (iterate (while (= 0 (mod temp p)))
          (incf e)
          (setq temp (truncate temp p))
          (setq pe (* pe p)))
        (if (= temp 1)
            (if (= e 1)
                (setf (aref *pi* m) (compute-pi-prime p))
                (setf (aref *pi* m) (* p (aref *pi* (truncate m p)))))
            (setf (aref *pi* m) (lcm (aref *pi* pe) (aref *pi* temp)))))))
  
  (let ((max-P 0))
    (iterate (for m from 1 to $M)
      (setq max-P (max max-P (aref *pi* m))))
    
    (let ((w-arr (make-array (1+ max-P) :element-type 'fixnum :initial-element 1)))
      (iterate (for p from 2 to max-P)
        (when (= (aref *spf* p) p)
          (let ((factor (mod (- 1 (mod (* p p) $MOD)) $MOD)))
            (iterate (for i from p to max-P by p)
              (setf (aref w-arr i) (mod (* (aref w-arr i) factor) $MOD))))))
      (setq *W* w-arr)))
      
  (precompute-pe-info $M)
  
  (let ((total-S 0)
        (m-pes (make-array 20 :element-type 'fixnum)))
    (iterate (for m from 1 to $M)
      (let ((num-m-pes 0)
            (temp m))
        (iterate (while (> temp 1))
          (let ((p (aref *spf* temp))
                (pe 1))
            (iterate (while (= 0 (mod temp p)))
              (setq pe (* pe p))
              (setq temp (truncate temp p)))
            (setf (aref m-pes num-m-pes) pe)
            (incf num-m-pes)))
            
        (let ((pi-m (aref *pi* m))
              (s-m 0))
          (map-divisors pi-m
            (lambda (d)
              (let ((c-val 1))
                (iterate (for i from 0 below num-m-pes)
                  (let* ((pe (aref m-pes i))
                         (pi-pe (aref *pi* pe))
                         (g (gcd d pi-pe)))
                    (setq c-val (mod (* c-val (get-C pe g)) $MOD))))
                (let* ((w-val (aref *W* (truncate pi-m d)))
                       (d-mod (mod d $MOD))
                       (d2 (mod (* d-mod d-mod) $MOD))
                       (term (mod (* c-val d2) $MOD)))
                  (setq term (mod (* term w-val) $MOD))
                  (setq s-m (mod (+ s-m term) $MOD))))))
          (setq total-S (mod (+ total-S s-m) $MOD))
          (when (= 0 (mod m 100000))
            (format t "[Log] Processed m = ~D, Current Total = ~D~%" m total-S)
            (finish-output)))))
    total-S))


#+| Do it | (solve )
#|------------------------------------------------------------|
Timing the evaluation of (solve)
[Log] Processed m = 100000, Current Total = 480895508
[Log] Processed m = 200000, Current Total = 674627180
[Log] Processed m = 300000, Current Total = 745688185
[Log] Processed m = 400000, Current Total = 164084901
[Log] Processed m = 500000, Current Total = 923861667
[Log] Processed m = 600000, Current Total = 832999026
[Log] Processed m = 700000, Current Total = 50012992
[Log] Processed m = 800000, Current Total = 883487299
[Log] Processed m = 900000, Current Total = 379953793
[Log] Processed m = 1000000, Current Total = 213731313

User time    =       42.330
System time  =        0.395
Elapsed time =       42.592
Allocation   = 267238352 bytes
65241 Page faults
GC time      =        0.168
 |------------------------------------------------------------|#
;;→ 213731313
:ok