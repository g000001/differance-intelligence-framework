;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3.1-pro
(cl:in-package cl-user)
(defpackage #:project-euler-0626 (:use cl iterate alexandria) (:export #:solve))
(in-package #:project-euler-0626)

#||
(clif-logic
  (formal-problem "Project Euler 626: Counting equivalent binary matrices")
  (invariants
    (burnside-lemma
      (equal (c n)
             (sum_{g \in G} (fix g)) / |G|))
    (group-action
      (equal |G| (* (expt (factorial n) 2) (expt 2 (* 2 n)))))
    (parity-constraint
      (implies (valid-parity X Y)
               (and (implies (> (v2 k_i) (v2 l_j)) (= X_i 0))
                    (implies (< (v2 k_i) (v2 l_j)) (= Y_j 0))
                    (implies (= (v2 k_i) (v2 l_j)) (= X_i Y_j)))))
    (valid-parity-count
      (equal (N \lambda \mu)
             (let ((v_L (min_{i} (v2 k_i)))
                   (v_M (min_{j} (v2 l_j))))
               (cond ((< v_L v_M) (expt 2 (sum_{v < v_M} c_\lambda(v))))
                     ((> v_L v_M) (expt 2 (sum_{v < v_L} c_\mu(v))))
                     (t 2))))))
  (optimizations
    (symmetry-reduction "Exploited the symmetry S(λ, μ) = S(μ, λ) to cut the 627x627 state space in half, resulting in ~1.9x10^5 iterations.")
    (dimension-collapse "Collapsed the 2^40 possible parity assignments into an O(1) formula using 2-adic valuation properties.")
    (allocation-zero "Calculated GCDs and exponents using pre-compressed run-length encodings of integer partitions, keeping allocations strict to initialization phase.")))
||#


(defstruct part
  items z a c-v v-min)

(defun v2 (n)
  "Calculates the 2-adic valuation (number of trailing zeros) of n."
  (let ((v 0))
    (iterate (while (evenp n))
      (incf v)
      (setf n (ash n -1)))
    v))

(defun factorial (n)
  (let ((res 1))
    (iterate (for i from 1 to n)
      (setf res (* res i)))
    res))

(defun generate-partitions (n &optional (max-val n))
  "Generates all integer partitions of n."
  (cond ((= n 0) (list nil))
        ((< n 0) nil)
        (t
         (let ((result nil))
           (iterate (for i from (min n max-val) downto 1)
             (let ((sub-parts (generate-partitions (- n i) i)))
               (dolist (p sub-parts)
                 (push (cons i p) result))))
           (nreverse result)))))

(defun compress-partition (p)
  "Compresses a partition list into an alist of (length . count)."
  (let ((counts nil))
    (dolist (x p)
      (let ((entry (assoc x counts)))
        (if entry
            (incf (cdr entry))
            (push (cons x 1) counts))))
    counts))

(defun make-part-info (p)
  "Precalculates mathematical attributes (z, a, 2-adic properties) for a partition."
  (let* ((comp (compress-partition p))
         (z 1)
         (a 0)
         (c-v (make-array 6 :initial-element 0)))
    (dolist (kc comp)
      (let ((k (car kc))
            (c (cdr kc)))
        (incf a c)
        (setf z (* z (expt k c) (factorial c)))
        (incf (aref c-v (v2 k)) c)))
    (let ((v-min (position-if (lambda (x) (> x 0)) c-v)))
      (make-part :items comp :z z :a a :c-v c-v :v-min v-min))))

(defun calc-g (p1 p2)
  "Calculates the sum of GCDs between all cycle pairs of two partitions."
  (let ((g 0))
    (dolist (kc1 (part-items p1))
      (dolist (kc2 (part-items p2))
        (incf g (* (cdr kc1) (cdr kc2) (gcd (car kc1) (car kc2))))))
    g))

(defun calc-n-val (p1 p2)
  "Calculates the number of valid parity assignments using 2-adic invariants."
  (let ((vl (part-v-min p1))
        (vm (part-v-min p2)))
    (cond ((< vl vm)
           (let ((sum 0))
             (iterate (for v from 0 below vm)
               (incf sum (aref (part-c-v p1) v)))
             (expt 2 sum)))
          ((> vl vm)
           (let ((sum 0))
             (iterate (for v from 0 below vl)
               (incf sum (aref (part-c-v p2) v)))
             (expt 2 sum)))
          (t 2))))

(defun mod-inv (a m)
  "Calculates the modular inverse using the Extended Euclidean Algorithm."
  (let ((t0 0) (t1 1) (r0 m) (r1 (mod a m)))
    (iterate (while (> r1 0))
      (let* ((q (floor r0 r1))
             (t2 (- t0 (* q t1)))
             (r2 (- r0 (* q r1))))
        (setf t0 t1 t1 t2 r0 r1 r1 r2)))
    (if (< t0 0) (+ t0 m) t0)))

(defun mod-pow (base exp m)
  "Calculates modular exponentiation, supporting negative exponents."
  (if (< exp 0)
      (mod-pow (mod-inv base m) (- exp) m)
      (let ((res 1)
            (b (mod base m)))
        (iterate (while (> exp 0))
          (when (oddp exp)
            (setf res (mod (* res b) m)))
          (setf b (mod (* b b) m))
          (setf exp (ash exp -1)))
        res)))

(defun solve ()
  (let* ((n 20)
         (mod 1001001011)
         (raw-parts (generate-partitions n))
         (parts-list (mapcar #'make-part-info raw-parts))
         (parts (coerce parts-list 'vector))
         (num-parts (length parts))
         (total-sum 0))
    
    (iterate (for i from 0 below num-parts)
      (let ((p1 (aref parts i)))
        ;; Using symmetry: S(λ, μ) = S(μ, λ), iterating j from i to cut work in half
        (iterate (for j from i below num-parts)
          (let* ((p2 (aref parts j))
                 (g (calc-g p1 p2))
                 (n-val (calc-n-val p1 p2))
                 ;; Negative powers can occur safely for some edge partitions, handled by mod-pow
                 (power (- (+ g) (part-a p1) (part-a p2)))
                 (term (mod (* (mod n-val mod) (mod-pow 2 power mod)) mod)))
            
            (setf term (mod (* term (mod-inv (part-z p1) mod)) mod))
            (setf term (mod (* term (mod-inv (part-z p2) mod)) mod))
            
            (if (= i j)
                (setf total-sum (mod (+ total-sum term) mod))
                (setf total-sum (mod (+ total-sum (* 2 term)) mod)))))))
    
    total-sum))



#+| Do it | (solve )
#|------------------------------------------------------------|
Timing the evaluation of (solve)

User time    =        0.333
System time  =        0.017
Elapsed time =        0.284
Allocation   = 670632 bytes
3789 Page faults
GC time      =        0.000
 |------------------------------------------------------------|#
;;→ 695577663
:ok