;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3.1-pro-preview
(cl:in-package cl-user)
(defpackage #:project-euler-0750 (:use cl iterate alexandria))
(in-package #:project-euler-0750)

#||
(cl-text euler-acx-p750-absolute-truth

  (cl-comment "=== Project Euler 750: The Absolute Truth (Asymmetric Interval DP) ===")
  (cl-comment "The previous hallucination allowed stacks to be built in both ascending")
  (cl-comment "and descending orders, causing the DP to find an invalid, cheaper path.")
  
  (cl-comment "In Card Stacking, dragging stack A onto stack B implies a strict")
  (cl-comment "directional order (e.g., k MUST go onto k+1). Therefore, the physical")
  (cl-comment "location of the merged stack [i, j] is STRICTLY anchored at P[j].")
  
  (forall (i j)
    (implies (<= i j)
             (Equal (DP i j)
                    (Min k (And (>= k i) (< k j))
                         (+ (DP i k) (DP (+ k 1) j) (Abs (- (P k) (P j))))))))
                         
  (cl-comment "This obliterates all false symmetries and perfectly matches G(6) = 8.")
)
||#

(defun power-mod (base exp m)
  (declare (type fixnum base exp m))
  (let ((res 1)
        (b (mod base m)))
    (declare (type fixnum res b))
    (iterate (while (> exp 0))
      (when (oddp exp)
        (setf res (mod (the fixnum (* res b)) m)))
      (setf b (mod (the fixnum (* b b)) m))
      (setf exp (ash exp -1)))
    res))

(defun solve-750 (&optional (n 976))
  "Computes G(N) using the true, strictly asymmetric O(N^3) OBST."
  (let* ((n+1 (1+ n))
         (P (make-array n+1 :element-type 'fixnum :initial-element 0))
         (dp-size (* n+1 n+1))
         ;; DP(i, j) will store the minimum drag distance to merge values i through j.
         ;; The physical location of this merged stack is always P[j].
         (dp (make-array dp-size :element-type 'fixnum :initial-element 0)))
    (declare (type (simple-array fixnum (*)) P dp)
             (type fixnum n n+1 dp-size))
    
    ;; P[v] is the physical original position of the card with value v
    (iterate (for pos from 1 to n)
      (let ((val (power-mod 3 pos n+1)))
        (setf (aref P val) pos)))

    ;; Flatten 2D array access to 1D macro
    (macrolet ((idx (i j) `(the fixnum (+ (the fixnum (* (the fixnum ,i) n+1)) (the fixnum ,j)))))
      
      ;; Iterate over the length of the interval
      (iterate (for len from 1 to (1- n))
        (iterate (for i from 1 to (- n len))
          (let ((j (the fixnum (+ i len)))
                (min-cost 1000000000000000))
            (declare (type fixnum j min-cost))
            
            (let ((pj (aref P j)))
              (declare (type fixnum pj))
              (iterate (for k from i below j)
                (let* ((pk (aref P k))
                       ;; Distance from the root of the left stack (k) to the root of the right stack (j)
                       (dist (if (>= pk pj) (the fixnum (- pk pj)) (the fixnum (- pj pk))))
                       (cost (the fixnum (+ (aref dp (idx i k))
                                            (aref dp (idx (1+ k) j))
                                            dist))))
                  (declare (type fixnum pk dist cost))
                  (when (< cost min-cost)
                    (setf min-cost cost)))))
            
            (setf (aref dp (idx i j)) min-cost))))
      
      ;; The optimal answer is merging the entire sequence 1 to N
      (aref dp (idx 1 n)))))


#+| Do it | (solve-750 )
#|------------------------------------------------------------|
Timing the evaluation of (solve-750)

User time    =       36.787
System time  =        0.287
Elapsed time =       36.860
Allocation   = 9847568 bytes
2848 Page faults
GC time      =        0.004
 |------------------------------------------------------------|#
;;→ 160640
:ok

