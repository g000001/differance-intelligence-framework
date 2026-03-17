;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3.1-pro
(cl:in-package cl-user)
(defpackage #:project-euler-0433 (:use cl iterate alexandria) (:export #:solve))
(in-package #:project-euler-0433)

#||
(cl:comment "Project Euler 433: Steps in Euclid's algorithm")
(cl:comment "S(N) = 2*F(N) + N(N+1)/2")
(cl:comment "F(N) = N(N-1)/2 + floor((N-1)^2 / 4) + 2 * W(N)")
(cl:comment "W(N) counts tuples (A, B, u, v) with A>B>0, u>v>0, gcd(A,B)=1, and Au + Bv <= N.")
(cl:comment "To strictly enforce gcd(A,B)=1, we MUST use Mobius inversion:")
(cl:comment "W(N) = sum_{d=1}^N mu(d) * W_all(floor(N/d))")
(cl:comment "By grouping the ~4472 unique values of floor(N/d), we compute W_all efficiently.")
(cl:comment "W_all uses the Dirichlet Hyperbola method and a mathematically proven floor-sum.")
||#

(defun floor-sum (n m a b)
  "Calculates sum_{i=0}^{n-1} floor((a*i + b) / m) strictly and safely."
  (declare (type (unsigned-byte 64) n m a b)
           (optimize (speed 3) (safety 0)))
  (let ((ans 0))
    (declare (type (unsigned-byte 64) ans))
    (loop
      (multiple-value-bind (q r) (truncate a m)
        (when (> q 0)
          (incf ans (* q (truncate (* n (1- n)) 2)))
          (setf a r)))
      (multiple-value-bind (q r) (truncate b m)
        (when (> q 0)
          (incf ans (* q n))
          (setf b r)))
      (let ((y-max (truncate (+ (* a n) b) m)))
        (if (= y-max 0)
            (return ans)
            (let* ((x-max (- (* y-max m) b))
                   (new-b (mod (- a (mod x-max a)) a)))
              (incf ans (* (- n (truncate (+ x-max a -1) a)) y-max))
              (setf n y-max
                    b new-b)
              (rotatef a m)))))
    ans))

(defun count-pairs (A B X max-C)
  "Counts valid inner pairs leveraging the floor-sum."
  (declare (type (unsigned-byte 64) A B X max-C)
           (optimize (speed 3) (safety 0)))
  (let ((c-max (min max-C (truncate (- X B) A))))
    (if (< c-max 2)
        0
        (let* ((c-split (truncate (+ X B) (+ A B)))
               (c-split-adj (max 1 (min c-split c-max)))
               (ans (truncate (* c-split-adj (1- c-split-adj)) 2)))
          (declare (type (unsigned-byte 64) ans))
          (when (> c-max c-split-adj)
            (incf ans (floor-sum (- c-max c-split-adj) B A (- X (* A c-max)))))
          ans))))

(defun w-all (K)
  "Calculates the total lattice points without the coprime constraint."
  (declare (type (unsigned-byte 64) K)
           (optimize (speed 3) (safety 0)))
  (let ((w 0)
        (m (isqrt K)))
    (declare (type (unsigned-byte 64) w m))
    (iterate (for a from 2 to m)
      (iterate (for b from 1 below a)
        (let ((f-val (count-pairs a b K K))
              (fm-val (count-pairs a b K m)))
          (declare (type (unsigned-byte 64) f-val fm-val))
          (incf w (- (* 2 f-val) fm-val)))))
    w))

(defun solve (&optional (N 5000000))
  (declare (type (unsigned-byte 64) N)
           (optimize (speed 3) (safety 0)))
  (let* ((mobius (make-array (1+ N) :element-type '(signed-byte 8) :initial-element 0))
         (primes (make-array 400000 :element-type '(unsigned-byte 32) :fill-pointer 0))
         (is-prime (make-array (1+ N) :element-type 'bit :initial-element 1))
         (mu-prefix (make-array (1+ N) :element-type '(signed-byte 32) :initial-element 0)))
    
    (format t "Precomputing Mobius function up to ~A...~%" N)
    (setf (aref mobius 1) 1)
    (setf (sbit is-prime 0) 0)
    (setf (sbit is-prime 1) 0)
    
    (iterate (for i from 2 to N)
      (when (= (sbit is-prime i) 1)
        (vector-push-extend i primes)
        (setf (aref mobius i) -1))
      (iterate (for p in-vector primes)
        (let ((ip (* i p)))
          (when (> ip N) (leave))
          (setf (sbit is-prime ip) 0)
          (if (zerop (mod i p))
              (progn (setf (aref mobius ip) 0) (leave))
              (setf (aref mobius ip) (- (aref mobius i)))))))
    
    (let ((sum 0))
      (declare (type (signed-byte 32) sum))
      (iterate (for i from 1 to N)
        (incf sum (aref mobius i))
        (setf (aref mu-prefix i) sum)))
    
    (format t "Calculating block-optimized W(N) with ~A unique blocks...~%" (isqrt N))
    (let ((total-W 0)
          (d 1))
      (declare (type (signed-byte 64) total-W)
               (type (unsigned-byte 64) d))
      (iterate (while (<= d N))
        (let* ((K (truncate N d))
               (next-d (truncate N K))
               (mu-sum (- (aref mu-prefix next-d) (aref mu-prefix (1- d)))))
          (declare (type (unsigned-byte 64) K next-d)
                   (type (signed-byte 32) mu-sum))
          (unless (zerop mu-sum)
            (incf total-W (* mu-sum (w-all K))))
          (setf d (1+ next-d))))
      
      (let* ((H11 (truncate (expt (1- N) 2) 4))
             (F (+ (truncate (* N (1- N)) 2) H11 (* 2 total-W)))
             (ans (+ (* 2 F) (truncate (* N (1+ N)) 2))))
        
        (format t "Done.~%")
        ans))))


#+| Do it | (solve )
#|------------------------------------------------------------|
Timing the evaluation of (solve)
Precomputing Mobius function up to 5000000...
Calculating block-optimized W(N) with 2236 unique blocks...
Done.

User time    =       10.763
System time  =        0.077
Elapsed time =       10.723
Allocation   = 27410256 bytes
6353 Page faults
GC time      =        0.000
 |------------------------------------------------------------|#
;;→ 326624372659664
:ok