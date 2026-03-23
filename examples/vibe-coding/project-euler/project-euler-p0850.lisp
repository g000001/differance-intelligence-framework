;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3-flash-preview
(cl:in-package cl-user)
(defpackage #:project-euler-0850 (:use cl iterate alexandria) (:export #:solve))
(in-package #:project-euler-0850)

#||
Project Euler 850: Fractions of Powers
For odd k, {i^k/n} + {(n-i)^k/n} = 1 if i^k/n is not an integer, and 0 otherwise.
This allows us to pair up the fractional parts, giving f_k(n) = (n - Z_k(n)) / 2,
where Z_k(n) is the number of 1 <= i <= n such that i^k == 0 mod n.
By prime factorization, Z_k(n) is multiplicative with Z_k(p^a) = p^{a - ceil(a/k)}.
We define g_k(n) such that Z_k = 1 * g_k.
g_k(n) is non-zero only for square-full numbers.
We can compute S_k(N) = sum_{d <= N} g_k(d) floor(N/d) via DFS over square-full numbers.
For k >= 45, g_k(d) is identical to g_inf(d) for all d <= N, drastically reducing computations.
The total time complexity collapses to O(sqrt(N) * number_of_k).
||#

(defconstant $mod 1955353558) ;; 2 * 977676779 to allow exact exact division by 2 via modulo arithmetic
(defconstant $n 33557799775533)

(defmacro mod+ (val-a val-b) `(mod (+ ,val-a ,val-b) $mod))
(defmacro mod- (val-a val-b) `(mod (- ,val-a ,val-b) $mod))
(defmacro mod* (val-a val-b) `(mod (* ,val-a ,val-b) $mod))

(defparameter *k-val* (floor (+ $n 1) 2))
(defparameter *max-p-limit* (isqrt $n))
(defparameter *num-primes-count* 0)
(defparameter *primes-arr* (make-array 500000 :element-type 'fixnum :fill-pointer 0))

(defparameter *total-s-inf* 0)
(defparameter *total-s-k-arr* (make-array 21 :element-type 'fixnum :initial-element 0))
;; Pre-allocated 2D stack matrix to achieve ZERO allocation during 12.5 million DFS calls
(defparameter *g-stack-matrix* (make-array '(20 21) :element-type 'fixnum :initial-element 0))

(defun sieve ()
  (let ((is-prime-arr (make-array (+ *max-p-limit* 1) :element-type 'bit :initial-element 1)))
    (setf (aref is-prime-arr 0) 0
          (aref is-prime-arr 1) 0)
    (iterate (for idx from 2 to *max-p-limit*)
      (when (= (aref is-prime-arr idx) 1)
        (vector-push idx *primes-arr*)
        (let ((step-val (if (= idx 2) 2 (* 2 idx)))
              (start-val (* idx idx)))
          (when (<= start-val *max-p-limit*)
            (iterate (for j-idx from start-val to *max-p-limit* by step-val)
              (setf (aref is-prime-arr j-idx) 0))))))
    (setf *num-primes-count* (length *primes-arr*))))

(defun g-inf-val (prime-p exp-a)
  (mod (if (= exp-a 2)
           (- prime-p 1)
           (if (= exp-a 3)
               (* prime-p (- prime-p 1))
               (- (expt prime-p (- exp-a 1)) (expt prime-p (- exp-a 2)))))
       $mod))

(defun g-k-val (odd-k prime-p exp-a)
  (mod (if (<= exp-a odd-k)
           (if (= exp-a 2)
               (- prime-p 1)
               (if (= exp-a 3)
                   (* prime-p (- prime-p 1))
                   (- (expt prime-p (- exp-a 1)) (expt prime-p (- exp-a 2)))))
           (let* ((exp-1 (- exp-a (floor (+ exp-a odd-k -1) odd-k)))
                  (exp-2 (- exp-a 1 (floor (+ exp-a odd-k -2) odd-k))))
             (- (expt prime-p exp-1) (expt prime-p exp-2))))
       $mod))

(defun dfs-square-full (p-idx current-d depth-level g-inf-curr)
  (let* ((n-over-d (floor $n current-d))
         (n-over-d-mod (mod n-over-d $mod)))
    (setf *total-s-inf* (mod+ *total-s-inf* (mod* g-inf-curr n-over-d-mod)))
    (iterate (for j-idx from 0 to 20)
      (setf (aref *total-s-k-arr* j-idx)
            (mod+ (aref *total-s-k-arr* j-idx)
                  (mod* (aref *g-stack-matrix* depth-level j-idx) n-over-d-mod)))))
  
  (iterate (for i-idx from p-idx below *num-primes-count*)
    (let* ((curr-p (aref *primes-arr* i-idx))
           (p-squared (* curr-p curr-p)))
      (if (> p-squared (floor $n current-d))
          (finish))
      (let ((p-power p-squared)
            (curr-exp 2))
        (iterate (while (<= (* current-d p-power) $n))
          (let ((next-g-inf (mod* g-inf-curr (g-inf-val curr-p curr-exp)))
                (next-depth (+ depth-level 1)))
            (iterate (for j-idx from 0 to 20)
              (setf (aref *g-stack-matrix* next-depth j-idx)
                    (mod* (aref *g-stack-matrix* depth-level j-idx)
                          (g-k-val (+ 3 (* 2 j-idx)) curr-p curr-exp))))
            (dfs-square-full (+ i-idx 1) (* current-d p-power) next-depth next-g-inf))
          
          ;; Ensure we don't accidentally create Bignums checking the next loop iteration
          (if (> p-power (floor $n (* current-d curr-p)))
              (finish))
          (setf p-power (* p-power curr-p))
          (incf curr-exp))))))

(defun solve ()
  (sieve)
  (setf *total-s-inf* 0)
  (iterate (for j-idx from 0 to 20)
    (setf (aref *total-s-k-arr* j-idx) 0)
    (setf (aref *g-stack-matrix* 0 j-idx) 1))
  
  (dfs-square-full 0 1 0 1)
  
  (let ((total-z-mod (mod $n $mod))
        (num-inf-terms (max 0 (- *k-val* 22))))
    (setf total-z-mod (mod+ total-z-mod (mod* (mod num-inf-terms $mod) *total-s-inf*)))
    (iterate (for j-idx from 0 to (min 20 (- *k-val* 2)))
      (setf total-z-mod (mod+ total-z-mod (aref *total-s-k-arr* j-idx))))
      
    (let* ((n-val $n)
           (n-plus-1 (+ $n 1))
           (k-val *k-val*))
      (if (evenp n-val)
          (setf n-val (floor n-val 2))
          (setf n-plus-1 (floor n-plus-1 2)))
      (let* ((term-1 (mod n-val $mod))
             (term-2 (mod n-plus-1 $mod))
             (term-3 (mod k-val $mod))
             (k2-n-mod (mod* term-3 (mod* term-1 term-2)))
             (v-mod (mod- k2-n-mod total-z-mod))
             (ans-val (floor v-mod 2)))
        (if (oddp v-mod)
            (setf ans-val (floor (+ v-mod $mod) 2)))
        (let ((final-ans (mod ans-val (floor $mod 2))))
          (format t "Log: Modulo M remainder = ~A~%" final-ans)
          final-ans)))))


#+| Do it | (solve )
#|------------------------------------------------------------|
Timing the evaluation of (solve)
Log: Modulo M remainder = 764812036

User time    =       56.609
System time  =        0.463
Elapsed time =       56.518
Allocation   = 4296564816 bytes
6422 Page faults
GC time      =        0.000
 |------------------------------------------------------------|#
;;→ 764812036
:ok