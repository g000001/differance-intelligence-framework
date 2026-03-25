;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3.5-pro
(cl:in-package cl-user)
(defpackage #:project-euler-0311 (:use cl iterate alexandria) (:export #:solve))
(in-package #:project-euler-0311)

#||
(cl:comment "PE 311 Mathematical Constraints and Shortcuts")
(cl:comment "Invariant 1: For a biclinic integral quadrilateral with AO=CO=u and BO=DO=v, Apollonius's theorem forces the sums of squares of opposite sides to be equal: AB^2+AD^2 = BC^2+CD^2 = 2u^2 + 2v^2 = M.")
(cl:comment "Invariant 2: The total sum of squares is 2M <= N, so M <= N/2. Furthermore, since u is an integer and 2v=BD is an integer, 2M = (2u)^2 + (2v)^2. This forces M to be strictly even: M = 2L, where L = u^2+v^2 for integers u,v.")
(cl:comment "Constraint 1: Any representation of L = u^2+v^2 with 0 < u < v gives a representation of M = (v-u)^2 + (v+u)^2. Let x = v-u and y = v+u. A strict ordering of x determines a strict ordering of all side lengths AB < BC < CD < AD.")
(cl:comment "Constraint 2: To form a valid non-degenerate convex quadrilateral, the diagonal representation (u_m, v_m) must satisfy v_m < v_i for the side representations. This enforces picking exactly 3 distinct valid representations out of k representations, yielding C(k, 3) combinations. If L is of the form 2*S^2, the representation u=v=S provides an extra valid diagonal option, yielding an additional C(k, 2) combinations.")
(cl:comment "Shortcut: We map the problem to summing these combinations over all valid L <= N/4. L takes the form 2^a * Q^2 * K, where Q has only prime factors p = 3 mod 4, and K has only prime factors p = 1 mod 4. Since K must have at least 5 divisors to yield combinations, we can decouple the sum: precompute the coefficient distribution F(W) for the 2^a * Q^2 part in O(M), and then run a highly restricted DFS over the very sparse valid K's. This reduces operations from 2.5 billion down to less than 10^7.")
||#

(declaim (inline C-ns C-even C-odd))

(defun C-ns (d)
  "Combinations when L is not a square."
  (let ((k (floor d 2)))
    (if (< k 3) 0
        (floor (* k (1- k) (- k 2)) 6))))

(defun C-even (d)
  "Combinations when L is a square but L/2 is not."
  (let ((k (floor (1- d) 2)))
    (if (< k 3) 0
        (floor (* k (1- k) (- k 2)) 6))))

(defun C-odd (d)
  "Combinations when L is not a square but L/2 is a square."
  (let* ((k (floor (1- d) 2))
         (term1 (if (< k 3) 0 (floor (* k (1- k) (- k 2)) 6)))
         (term2 (if (< k 2) 0 (floor (* k (1- k)) 2))))
    (+ term1 term2)))

(defun is-prime (n)
  "Simple trial division for small numbers."
  (cond ((< n 2) nil)
        ((= n 2) t)
        ((evenp n) nil)
        (t (iterate (for i from 3 to (isqrt n) by 2)
             (when (zerop (mod n i))
               (return-from is-prime nil)))
           t)))

;(sys::primep$fixnum)

(defun generate-Qs (max-q)
  "Generates all numbers Q <= max-q whose prime factors are all congruent to 3 mod 4."
  (let ((qs (make-array 0 :element-type 'fixnum :fill-pointer 0 :adjustable t))
        (primes-3mod4 (make-array 0 :element-type 'fixnum :fill-pointer 0 :adjustable t)))
    (iterate (for p from 3 to max-q by 2)
      (when (and (= (mod p 4) 3) (is-prime p))
        (vector-push-extend p primes-3mod4)))
    
    (labels ((gen (idx current-q)
               (vector-push-extend current-q qs)
               (iterate (for i from idx below (length primes-3mod4))
                 (let* ((p (aref primes-3mod4 i))
                        (next-q (* current-q p)))
                   (if (<= next-q max-q)
                       (gen i next-q)
                       (finish))))))
      (gen 0 1))
    
    (sort qs #'<)
    qs))

(defun generate-primes-1mod4 (limit)
  "Generates all prime numbers p = 1 mod 4 up to limit using a bitset sieve."
  (let* ((sieve-size (1+ (ash limit -1)))
         (sieve (make-array sieve-size :element-type 'bit :initial-element 0))
         (primes (make-array 0 :element-type 'fixnum :fill-pointer 0 :adjustable t)))
    
    (iterate (for i from 3 to (isqrt limit) by 2)
      (when (zerop (sbit sieve (ash i -1)))
        (iterate (for j from (* i i) to limit by (* i 2))
          (setf (sbit sieve (ash j -1)) 1))))
          
    (iterate (for i from 5 to limit by 4)
      (when (zerop (sbit sieve (ash i -1)))
        (vector-push-extend i primes)))
        
    primes))

(defun precompute-F (max-w qs)
  "Precomputes the prefix sums of counts of valid (a, Q) pairs yielding 2^a * Q^2 <= W."
  (let ((f-any (make-array (1+ max-w) :element-type '(unsigned-byte 32) :initial-element 0))
        (f-even (make-array (1+ max-w) :element-type '(unsigned-byte 32) :initial-element 0))
        (f-odd (make-array (1+ max-w) :element-type '(unsigned-byte 32) :initial-element 0)))
    
    ;; Build difference arrays
    (iterate (for i from 0 below (length qs))
      (let* ((q (aref qs i))
             (q2 (* q q)))
        (when (> q2 max-w) (finish))
        (iterate (for a from 0)
          (let ((val (ash q2 a)))
            (when (> val max-w) (finish))
            (incf (aref f-any val))
            (if (evenp a)
                (incf (aref f-even val))
                (incf (aref f-odd val)))))))
                
    ;; Accumulate into prefix sums
    (iterate (for i from 1 to max-w)
      (incf (aref f-any i) (aref f-any (1- i)))
      (incf (aref f-even i) (aref f-even (1- i)))
      (incf (aref f-odd i) (aref f-odd (1- i))))
      
    (values f-any f-even f-odd)))

(defun execute-dfs (prime-idx current-k current-d is-sq m num-primes primes f-even f-odd f-any)
  "Recursively explores sparse combinations of primes p = 1 mod 4 to compute the total valid quadrilaterals."
  (let ((branch-sum 0))
    (iterate (for i from prime-idx below num-primes)
      (let ((p (aref primes i)))
        ;; Lisp-critic fix: Use WHEN instead of IF without an else branch
        (when (> p (floor m current-k)) 
          (finish))
          
        (let ((power 1) 
              (k-new (* current-k p)))
          (iterate (while (<= k-new m))
            (let* ((d-new (* current-d (1+ power)))
                   (is-sq-new (and is-sq (evenp power)))
                   (m-k-floor (floor m k-new)))
                   
              (cond
                (is-sq-new
                 (let ((ce (c-even d-new)))
                   (when (> ce 0)
                     (incf branch-sum (* ce (aref f-even m-k-floor)))))
                 (let ((co (c-odd d-new)))
                   (when (> co 0)
                     (incf branch-sum (* co (aref f-odd m-k-floor))))))
                (t
                 (let ((cns (c-ns d-new)))
                   (when (> cns 0)
                     (incf branch-sum (* cns (aref f-any m-k-floor)))))))
              
              ;; Lisp-critic fix: Use WHEN for conditional recursion
              (when (and (< (1+ i) num-primes)
                         (<= (* k-new (aref primes (1+ i))) m))
                (incf branch-sum 
                      (execute-dfs (1+ i) k-new d-new is-sq-new m num-primes primes f-even f-odd f-any)))
              
              (incf power)
              (setf k-new (* k-new p)))))))
    branch-sum))

(defun solve ()
  "Calculates the number of distinct biclinic integral quadrilaterals for N = 10^10."
  (let* ((n #.(expt 10 10))
         (m (floor n 4))
         (max-w (floor m 325))
         (max-q (isqrt m))
         (limit (if (> m 0) (min m (floor m 25)) 0)))
         
    (when (= max-w 0)
      (format t "Final Answer B(~A): 0~%" n)
      (return-from solve 0))
      
    (format t "Precomputing step functions and sieving primes...~%")
    (let* ((qs (generate-qs max-q))
           (primes (generate-primes-1mod4 limit))
           (num-primes (length primes)))
           
      (multiple-value-bind (f-any f-even f-odd) (precompute-f max-w qs)
        (format t "Starting DFS to evaluate combination kernels...~%")
        
        (let ((total-sum (execute-dfs 0 1 1 t m num-primes primes f-even f-odd f-any)))
          (format t "Final Answer B(~A): ~A~%" n total-sum)
          total-sum)))))


#+| Do it | (solve )
#|------------------------------------------------------------|
Timing the evaluation of (solve)
Precomputing step functions and sieving primes...
Starting DFS to evaluate combination kernels...
Final Answer B(10000000000): 2466018557

User time    =        8.286
System time  =        0.142
Elapsed time =        8.356
Allocation   = 166020912 bytes
63065 Page faults
GC time      =        0.081
 |------------------------------------------------------------|#
;;→ 2466018557
:ok
