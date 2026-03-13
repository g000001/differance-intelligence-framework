;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3.1-pro
(cl:in-package cl-user)
(defpackage #:project-euler-0170 (:use cl iterate alexandria))
(in-package #:project-euler-0170)

#||
(cl-text EULER-ACX-DIFD-INTEGRATION
  (cl-comment "
  =============================================================================
  ARX-Core: Structural Gravity Protocol for PE 0170 (Discrete DIFD)
  =============================================================================
  Formalization of the Alethetic Reset: Transitioning from combinatorial input 
  generation (NMF) to Inverse Search guided by GCD Structural Gradient (∇C).
  ")

  (cl-comment "1. NMF (Non-Middle Fallacy) Detection")
  (forall (?solver)
    (if (and (Solves ?solver PE0170)
             (ConstructsInputsFirst ?solver))
        (and (NMF ?solver)
             (ProducesHallucination (SecularDebt ?solver))
             (ExceedsTimeLimit 60))))

  (cl-comment "2. ACX Jump: Orthogonal Projection via GCD")
  (cl-comment "Generate Output P in descending order and split into p_i. 
               The invariant is that k MUST divide all p_i. 
               Therefore, g = GCD(p_1, ..., p_m) >= 2.")
  (forall (?P ?p_i)
    (if (and (DescendingPandigital ?P)
             (Partition ?P ?p_i))
        (and (StructuralGradient (GCD ?p_i))
             (PrunesSpace (Equal (GCD ?p_i) 1)))))

  (cl-comment "3. Middle Way Manifestation")
  (forall (?P)
    (if (ValidConcatenatedProduct ?P)
        (Equal (Limit ?P (DescendingSearch)) (Maximum ?P))))
)
||#

(defun prev-permutation (a)
  "Generates the previous lexicographical permutation of array A."
  (declare (type (simple-array fixnum (10)) a))
  (let ((i 9))
    (declare (type fixnum i))
    (iterate (while (and (> i 0) (<= (aref a (1- i)) (aref a i))))
             (decf i))
    (when (zerop i) (return-from prev-permutation nil))
    (let ((j 9))
      (declare (type fixnum j))
      (iterate (while (>= (aref a j) (aref a (1- i))))
               (decf j))
      (rotatef (aref a (1- i)) (aref a j))
      (let ((left i) (right 9))
        (declare (type fixnum left right))
        (iterate (while (< left right))
                 (rotatef (aref a left) (aref a right))
                 (incf left)
                 (decf right))))
    a))

(defun is-pandigital-input (k parts)
  "Checks if the concatenation of k and (p_i / k) is a 10-digit 0-9 pandigital."
  (let ((mask 0)
        (len 0))
    (declare (type fixnum mask len))
    (labels ((add-num (n)
               (declare (type (unsigned-byte 64) n))
               (if (zerop n)
                   (if (logbitp 0 mask)
                       (return-from is-pandigital-input nil)
                       (progn
                         (setf mask (logior mask 1))
                         (incf len)))
                   (iterate (while (plusp n))
                            (multiple-value-bind (q r) (truncate n 10)
                              (if (logbitp r mask)
                                  (return-from is-pandigital-input nil)
                                  (progn
                                    (setf mask (logior mask (ash 1 r)))
                                    (incf len)))
                              (setf n q))))
               t))
      (unless (add-num k) (return-from is-pandigital-input nil))
      (iterate (for p in parts)
               (unless (add-num (floor p k)) (return-from is-pandigital-input nil)))
      (and (= len 10) (= mask #x3FF)))))

(defun get-divisors (n)
  "Returns all divisors of n greater than 1."
  (declare (type (unsigned-byte 64) n))
  (let ((divs nil))
    (iterate (for i from 2 to (isqrt n))
             (when (zerop (mod n i))
               (push i divs)
               (let ((q (truncate n i)))
                 (when (/= q i) (push q divs)))))
    (when (> n 1) (push n divs))
    divs))

(defun check-parts (current-gcd parts)
  "Checks if any divisor k of current-gcd produces a valid pandigital input."
  (let ((divs (get-divisors current-gcd)))
    (iterate (for k in divs)
             (when (is-pandigital-input k parts)
               (return-from check-parts t)))
    nil))

(defun split-and-check (p-array)
  "Partitions the pandigital array and structurally prunes via GCD."
  (labels ((dfs (start current-gcd parts)
             (declare (type fixnum start)
                      (type (unsigned-byte 64) current-gcd))
             ;; ACX Jump: If GCD degrades to 1, no common multiplier k >= 2 can exist.
             ;; This enforces orthogonality and dramatically prunes the search space.
             (when (and parts (= current-gcd 1)) (return-from dfs nil))
             
             (if (= start 10)
                 (when (>= (length parts) 2)
                   (check-parts current-gcd parts))
                 (let ((val 0))
                   (declare (type (unsigned-byte 64) val))
                   (iterate (for i from start below 10)
                            (setf val (+ (* val 10) (aref p-array i)))
                            ;; Numbers cannot have leading zeros unless the number is exactly 0
                            (when (and (= (aref p-array start) 0) (> i start))
                              (finish))
                            (let ((next-gcd (if (null parts) val (gcd current-gcd val))))
                              (when (or (null parts) (> next-gcd 1))
                                (when (dfs (1+ i) next-gcd (cons val parts))
                                  (return-from split-and-check t)))))
                   nil))))
    (dfs 0 0 nil)))

(defun solve-170 ()
  "Finds the largest 0-9 pandigital 10-digit concatenated product."
  ;; Generate P descending from 9876543210
  (let ((a (make-array 10 :element-type 'fixnum 
                          :initial-contents '(9 8 7 6 5 4 3 2 1 0))))
    (iterate 
      (when (split-and-check a)
        (let ((res 0))
          (declare (type (unsigned-byte 64) res))
          (iterate (for x in-vector a)
                   (setf res (+ (* res 10) x)))
          (return res)))
      (unless (prev-permutation a)
        (return nil)))))

#+| Do it | (solve-170 )
#|------------------------------------------------------------|
Timing the evaluation of (solve-170)

User time    =        1.159
System time  =        0.023
Elapsed time =        1.074
Allocation   = 18693960 bytes
3884 Page faults
GC time      =        0.000
 |------------------------------------------------------------|#
;;→ 9857164023
:ok