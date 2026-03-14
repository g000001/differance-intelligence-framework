;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3.1-pro
(cl:in-package cl-user)
(defpackage #:project-euler-0730 (:use cl iterate alexandria))
(in-package #:project-euler-0730)

#||
(cl-text EULER-ACX-DIFD-INTEGRATION
  (cl-comment "
  =============================================================================
  ARX-Core: Structural Gravity Protocol for PE 0730 (Discrete DIFD)
  =============================================================================
  Formalization of the Alethetic Reset: Transitioning from Recursive Tree 
  Traversal to Iterative Flat-Memory Projection (Overcoming Recursion Limit NMF).
  ")

  (cl-comment "1. NMF (Non-Middle Fallacy) Detection")
  (forall (?solver)
    (if (and (Solves ?solver PE0730)
             (UsesDeepRecursion ?solver (BarningHallTree)))
        (and (NMF ?solver)
             (ProducesHallucination (StackOverflowCrash ?solver))
             (ProducesHallucination (TightLoopIllusion ?solver)))))

  (cl-comment "2. ACX Jump: Orthogonal Projection via Iterative Stack")
  (cl-comment "While Barning-Hall trees mathematically factorize the solution space,
               branches U1 and U3 grow additively (linearly) rather than geometrically.
               This causes the tree depth to reach O(sqrt(n)) ~ 10000.
               Deep recursion in Lisp blows the stack, presenting as a 'tight loop crash'.
               The resolution is to project the search onto an explicit flat-memory array.")
  (forall (?tree)
    (if (BarningHallTree ?tree)
        (and (RequiresExplicitStack ?tree)
             (EliminatesFunctionCallOverhead ?tree))))

  (cl-comment "3. Middle Way Manifestation")
  (cl-comment "Preallocate fixnum arrays of size 1,000,000 to safely absorb
               the skewed linear branches of the automorphism group.
               The search achieves O(S) time with O(1) memory overhead per node.")
)
||#


(declaim (inline is-fundamental))
(defun is-fundamental (p q r)
  "Checks if (p, q, r) is a fundamental root by applying the inverse Barning-Hall matrices.
   If ANY inverse transformation yields a strictly positive integer vector, it is NOT fundamental."
  (declare (type fixnum p q r))
  
  ;; U1^-1
  (let ((p1 (+ p (ash q 1) (* -2 r)))
        (q1 (+ (* -2 p) (- q) (ash r 1)))
        (r1 (+ (* -2 p) (* -2 q) (* 3 r))))
    (declare (type fixnum p1 q1 r1))
    (when (and (> p1 0) (> q1 0) (> r1 0))
      (return-from is-fundamental nil)))

  ;; U2^-1
  (let ((p2 (+ p (ash q 1) (* -2 r)))
        (q2 (+ (ash p 1) q (* -2 r)))
        (r2 (+ (* -2 p) (* -2 q) (* 3 r))))
    (declare (type fixnum p2 q2 r2))
    (when (and (> p2 0) (> q2 0) (> r2 0))
      (return-from is-fundamental nil)))

  ;; U3^-1
  (let ((p3 (+ (- p) (* -2 q) (ash r 1)))
        (q3 (+ (ash p 1) q (* -2 r)))
        (r3 (+ (* -2 p) (* -2 q) (* 3 r))))
    (declare (type fixnum p3 q3 r3))
    (when (and (> p3 0) (> q3 0) (> r3 0))
      (return-from is-fundamental nil)))
      
  t)

(defun find-fundamental-roots ()
  "Finds all fundamental roots (p, q, r) for k in [0, 100]."
  (let ((roots nil))
    (iterate (for p from 1 to 200)
      (iterate (for q from 1 to 200)
        (let* ((p2q2 (+ (* p p) (* q q)))
               (r-min (isqrt p2q2)))
          (declare (type fixnum p2q2 r-min))
          (when (< (* r-min r-min) p2q2) (incf r-min))
          (iterate (for r from r-min)
            (declare (type fixnum r))
            (let ((k (- (* r r) p2q2)))
              (declare (type fixnum k))
              (when (> k 100) (finish))
              (when (and (= 1 (gcd p (gcd q r)))
                         (is-fundamental p q r))
                (push (list p q r) roots)))))))
    roots))


(defun solve-0730 ()
  "Calculates S(10^2, 10^8) using an iterative Barning-Hall tree traversal to prevent stack overflows."
  (let ((n 100000000)
        (total-sum 0))
    (declare (type fixnum n total-sum))
    
    (let ((roots (find-fundamental-roots))
          ;; 1,000,000 is safely > 50x the max recursion depth to prevent any overflow.
          (stack-p (make-array 1000000 :element-type 'fixnum))
          (stack-q (make-array 1000000 :element-type 'fixnum))
          (stack-r (make-array 1000000 :element-type 'fixnum))
          (sp 0))
      (declare (type fixnum sp))

      ;; Push all valid fundamental roots to the explicit stack
      (dolist (root roots)
        (let ((p (first root))
              (q (second root))
              (r (third root)))
          (declare (type fixnum p q r))
          (when (<= (+ p q r) n)
            (setf (aref stack-p sp) p)
            (setf (aref stack-q sp) q)
            (setf (aref stack-r sp) r)
            (incf sp))))

      ;; Iterative DFS
      (iterate (while (> sp 0))
        (decf sp)
        (let ((p (aref stack-p sp))
              (q (aref stack-q sp))
              (r (aref stack-r sp)))
          (declare (type fixnum p q r))
          
          ;; Problem constraint: p <= q <= r. The tree generates all valid pairs, 
          ;; so we only count the ones naturally ordered.
          (when (<= p q)
            (incf total-sum))

          ;; Branch 1 (U1)
          (let* ((p1 (+ p (* -2 q) (ash r 1)))
                 (q1 (+ (ash p 1) (- q) (ash r 1)))
                 (r1 (+ (ash p 1) (* -2 q) (* 3 r))))
            (declare (type fixnum p1 q1 r1))
            (when (and (> p1 0) (> q1 0) (> r1 0) (<= (+ p1 q1 r1) n))
              (setf (aref stack-p sp) p1)
              (setf (aref stack-q sp) q1)
              (setf (aref stack-r sp) r1)
              (incf sp)))

          ;; Branch 2 (U2)
          (let* ((p2 (+ p (ash q 1) (ash r 1)))
                 (q2 (+ (ash p 1) q (ash r 1)))
                 (r2 (+ (ash p 1) (ash q 1) (* 3 r))))
            (declare (type fixnum p2 q2 r2))
            (when (and (> p2 0) (> q2 0) (> r2 0) (<= (+ p2 q2 r2) n))
              (setf (aref stack-p sp) p2)
              (setf (aref stack-q sp) q2)
              (setf (aref stack-r sp) r2)
              (incf sp)))

          ;; Branch 3 (U3)
          (let* ((p3 (+ (- p) (ash q 1) (ash r 1)))
                 (q3 (+ (* -2 p) q (ash r 1)))
                 (r3 (+ (* -2 p) (ash q 1) (* 3 r))))
            (declare (type fixnum p3 q3 r3))
            (when (and (> p3 0) (> q3 0) (> r3 0) (<= (+ p3 q3 r3) n))
              (setf (aref stack-p sp) p3)
              (setf (aref stack-q sp) q3)
              (setf (aref stack-r sp) r3)
              (incf sp))))))
      
    total-sum))


#+| Do it | (solve-0730 )
#||
Timing the evaluation of (project-euler-0730::solve-0730)

User time    =  0:11:04.726
System time  =        7.265
Elapsed time =  0:12:35.896
Allocation   = 37265208 bytes
43270 Page faults
GC time      =        0.023
1315965924
||#

:ok