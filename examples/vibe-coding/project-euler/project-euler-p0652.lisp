;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3.5-pro
(cl:in-package cl-user)
(defpackage #:project-euler-0652 (:use cl iterate alexandria) (:export #:solve))
(in-package #:project-euler-0652)

#||
(cl:comment "PE 652 Mathematical Constraints and Shortcuts")
(cl:comment "Invariant 1: The proto-logarithmic equivalence relation collapses to a canonical form. For two values (m, n) = (u^e, v^f) where u, v are fundamental bases (not perfect powers), the equivalence class g(m,n) is completely determined by the unordered pair of fundamental bases {u, v} and the rational ratio of their exponents e/f.")
(cl:comment "Invariant 2: When u = v, the equivalence class depends ONLY on the ratio e/f, and is completely independent of the base u.")
(cl:comment "Constraint 1: The number of fundamental bases u such that u^k <= N is given exactly by inclusion-exclusion: S_k = Sum_{d>=1} mu(d) * (floor(N^(1/kd)) - 1). The number of fundamental bases with exactly maximum exponent k is simply C_k = S_k - S_{k+1}.")
(cl:comment "Shortcut: We can completely avoid generating any numbers or simulating equivalences. We just count the number of distinct fractions e/f that can be formed for 1 <= e <= i and 1 <= f <= j. Let this count be F(i, j).")
(cl:comment "Optimization: The final answer is F(M, M) + Sum_{i} C_i (C_i - 1) F(i, i) + 2 * Sum_{i < j} C_i C_j F(i, j). For N = 10^18, the maximum possible exponent M is 59. The problem is reduced to computing a 59x59 matrix F and an array C of size 59, which reduces the O(N^2) complexity to O(log^4 N) and completes in a fraction of a millisecond.")
||#

(defun integer-root (n k)
  "Calculates floor(n^(1/k)) using exact integer arithmetic."
  (cond ((= k 1) n)
        ((= k 2) (isqrt n))
        (t
         (let ((low 1)
               ;; Upper bound for N=10^18 is 10^6 for k=3, so 2^20 is safe
               (high (ash 1 (ceiling 60 k))))
           (iterate (while (<= low high))
             (let ((mid (ash (+ low high) -1)))
               (if (<= (expt mid k) n)
                   (setf low (1+ mid))
                   (setf high (1- mid)))))
           (1- low)))))

(defun compute-mobius (max-d)
  "Computes the Mobius function up to max-d."
  (let ((mu (make-array (1+ max-d) :element-type 'fixnum :initial-element 0))
        (primes (make-array 0 :element-type 'fixnum :fill-pointer 0 :adjustable t))
        (is-prime (make-array (1+ max-d) :element-type 'bit :initial-element 1)))
    (setf (aref mu 1) 1)
    (setf (sbit is-prime 0) 0)
    (setf (sbit is-prime 1) 0)
    (iterate (for i from 2 to max-d)
      (when (= (sbit is-prime i) 1)
        (vector-push-extend i primes)
        (setf (aref mu i) -1))
      (iterate (for p in-vector primes)
        (while (<= (* i p) max-d))
        (setf (sbit is-prime (* i p)) 0)
        (if (zerop (mod i p))
            (progn
              (setf (aref mu (* i p)) 0)
              (leave))
            (setf (aref mu (* i p)) (- (aref mu i))))))
    mu))

(defun compute-s-array (n m mu)
  "Computes S_k, the number of fundamental bases u such that u^k <= N."
  (let ((s-array (make-array (1+ m) :element-type 'integer :initial-element 0)))
    (iterate (for k from 1 to m)
      (let ((sum 0))
        (iterate (for d from 1 to (floor m k))
          (let ((m-val (aref mu d)))
            (unless (zerop m-val)
              (incf sum (* m-val (1- (integer-root n (* k d))))))))
        (setf (aref s-array k) sum)))
    s-array))

(defun compute-c-array (s-array m)
  "Computes C_k, the number of fundamental bases with max exponent exactly k."
  (let ((c-array (make-array (1+ m) :element-type 'integer :initial-element 0)))
    (iterate (for k from 1 to m)
      (let ((s-k (aref s-array k))
            (s-k-next (if (<= (1+ k) m) (aref s-array (1+ k)) 0)))
        (setf (aref c-array k) (- s-k s-k-next))))
    c-array))

(defun compute-f-matrix (m)
  "Computes F(i, j), the number of distinct fractions e/f with 1 <= e <= i and 1 <= f <= j."
  (let ((f-matrix (make-array (list (1+ m) (1+ m)) :element-type 'fixnum :initial-element 0)))
    (iterate (for i from 1 to m)
      (iterate (for j from i to m)
        (let ((seen (make-array (list (1+ i) (1+ j)) :element-type 'bit :initial-element 0))
              (count 0))
          (iterate (for e from 1 to i)
            (iterate (for f from 1 to j)
              (let* ((g (gcd e f))
                     (re (/ e g))
                     (rf (/ f g)))
                (when (zerop (sbit seen re rf))
                  (setf (sbit seen re rf) 1)
                  (incf count)))))
          (setf (aref f-matrix i j) count)
          (setf (aref f-matrix j i) count))))
    f-matrix))

(defun solve ()
  (let* ((n #.(expt 10 18))
         (m 59)                  ; floor(log2(10^18))
         (modulo #.(expt 10 9))     ; Required to give the last 9 digits
         (mu (compute-mobius m))
         (s-array (compute-s-array n m mu))
         (c-array (compute-c-array s-array m))
         (f-matrix (compute-f-matrix m))
         ;; The $u=v$ classes are exactly represented by F(M, M)
         (total-classes (mod (aref f-matrix m m) modulo)))
         
    (format t "Precomputations finished. Maximum exponent M = ~A~%" m)
    
    (iterate (for i from 1 to m)
      (let* ((c-i-exact (aref c-array i))
             (c-i (mod c-i-exact modulo))
             (c-i-minus-1 (mod (1- c-i-exact) modulo)))
             
        ;; Contribution from identical exponent bounds: C_i * (C_i - 1) * F(i, i)
        (let ((term (mod (* c-i c-i-minus-1) modulo)))
          (setf term (mod (* term (aref f-matrix i i)) modulo))
          (setf total-classes (mod (+ total-classes term) modulo)))
          
        ;; Contribution from distinct exponent bounds: 2 * C_i * C_j * F(i, j)
        (iterate (for j from (1+ i) to m)
          (let* ((c-j (mod (aref c-array j) modulo))
                 (term (mod (* 2 c-i) modulo)))
            (setf term (mod (* term c-j) modulo))
            (setf term (mod (* term (aref f-matrix i j)) modulo))
            (setf total-classes (mod (+ total-classes term) modulo))))))
            
    (format t "Final Answer (last 9 digits): ~9,'0D~%" total-classes)
    total-classes))


#+| Do it | (solve )
#|------------------------------------------------------------|
Timing the evaluation of (solve)
Precomputations finished. Maximum exponent M = 59
Final Answer (last 9 digits): 983924497

User time    =        0.343
System time  =        0.020
Elapsed time =        0.287
Allocation   = 582424 bytes
575 Page faults
GC time      =        0.000
 |------------------------------------------------------------|#
;;→ 983924497
:ok