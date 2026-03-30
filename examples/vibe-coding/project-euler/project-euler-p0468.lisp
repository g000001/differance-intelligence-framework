;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3.1-pro
(cl:in-package cl-user)
(defpackage #:project-euler-0468 (:use cl series alexandria) (:export #:solve))
(in-package #:project-euler-0468)
(eval-when (:compile-toplevel :load-toplevel :execute) (series::install))


(declaim (optimize (speed 3) (safety 0) (debug 0)))

(defconstant $modulus 1000000993)

;; ------------------------------------------------------------
;; Exact Generator Utilities
;; ------------------------------------------------------------

(defun make-uint32-array (array-size &optional (initial-element 0))
  (make-array array-size :element-type '(unsigned-byte 32) :initial-element initial-element))

(defun make-uint64-array (array-size &optional (initial-element 0))
  (make-array array-size :element-type '(unsigned-byte 64) :initial-element initial-element))

(defun make-int32-array (array-size &optional (initial-element 0))
  (make-array array-size :element-type '(signed-byte 32) :initial-element initial-element))

(defun compute-mod-inverse (value modulo-value)
  "Computes the modular inverse using the Extended Euclidean Algorithm."
  (declare (type (unsigned-byte 64) value modulo-value))
  (labels ((ext-gcd (a b)
             (if (zerop b)
                 (values 1 0 a)
                 (multiple-value-bind (q r) (truncate a b)
                   (multiple-value-bind (s t-val gcd) (ext-gcd b r)
                     (values t-val (- s (* q t-val)) gcd))))))
    (multiple-value-bind (x y gcd) (ext-gcd value modulo-value)
      (declare (ignore y gcd))
      (mod x modulo-value))))

(defun estimate-prime-count (limit-number)
  "Safely overestimates the number of primes up to LIMIT-NUMBER for array allocation."
  (if (< limit-number 100)
      30
      (floor (* 1.2d0 limit-number) (log limit-number))))

;; ------------------------------------------------------------
;; Structural Invariant Search & Dimensional Collapse 
;; ------------------------------------------------------------

(defun update-segment-tree (prime-index new-val tree-prod tree-sum weight-array num-leaves)
  "Recursively updates the segment tree from leaf to root in O(log pi(n)) without loops."
  (declare (type (unsigned-byte 32) prime-index num-leaves)
           (type (unsigned-byte 64) new-val)
           (type (simple-array (unsigned-byte 64) (*)) tree-prod tree-sum weight-array))
  (labels ((climb-tree (pos)
             (declare (type (unsigned-byte 32) pos))
             (when (> pos 0)
               (let* ((left (ash pos 1))
                      (right (1+ left)))
                 (setf (aref tree-prod pos) 
                       (mod (* (aref tree-prod left) (aref tree-prod right)) $modulus))
                 (setf (aref tree-sum pos) 
                       (mod (+ (aref tree-sum left) 
                               (* (aref tree-prod left) (aref tree-sum right))) 
                            $modulus))
                 (climb-tree (ash pos -1))))))
    (let ((leaf-pos (+ num-leaves prime-index -1)))
      (declare (type (unsigned-byte 32) leaf-pos))
      (setf (aref tree-prod leaf-pos) new-val)
      (setf (aref tree-sum leaf-pos) (mod (* new-val (aref weight-array prime-index)) $modulus))
      (climb-tree (ash leaf-pos -1)))))

(defun solve-for-n (limit-n)
  "Calculates F(n) by projecting the B-smooth divisor sums into a Segment Tree."
  (declare (type (unsigned-byte 32) limit-n))
  
  (let* ((prime-capacity (estimate-prime-count limit-n))
         (spf-idx (make-uint32-array (1+ limit-n) 0))
         (primes (make-uint64-array prime-capacity 0))
         (inv-primes (make-uint64-array prime-capacity 0))
         (prime-count 0))
    (declare (type (unsigned-byte 32) prime-count))

    ;; 1. Linear Sieve (O(N)) generating Smallest Prime Factors
    (iterate ((number-i (scan-range :from 2 :upto limit-n)))
      (when (zerop (aref spf-idx number-i))
        (incf prime-count)
        (setf (aref primes prime-count) number-i)
        (setf (aref inv-primes prime-count) (compute-mod-inverse number-i $modulus))
        (setf (aref spf-idx number-i) prime-count)
        ;; Inner multiples marking
        (iterate ((number-j (scan-range :from (* number-i number-i) :upto limit-n :by number-i)))
          (when (zerop (aref spf-idx number-j))
            (setf (aref spf-idx number-j) prime-count)))))

    ;; 2. Exact Segment Tree Initialization
    (let ((num-leaves 1))
      (labels ((find-pow (x) (if (>= x prime-count) x (find-pow (ash x 1)))))
        (setf num-leaves (find-pow 1)))
        
      (let ((tree-prod (make-uint64-array (* 2 num-leaves) 1))
            (tree-sum  (make-uint64-array (* 2 num-leaves) 0))
            (weights   (make-uint64-array (1+ prime-count) 0))
            (current-val (make-uint64-array (1+ prime-count) 1))
            (diff-counts (make-int32-array (1+ prime-count) 0))
            (changed-indices (make-uint32-array 200 0))
            (num-changed 0)
            (total-f 0)
            (half-limit (floor limit-n 2)))
        
        ;; Weight mapping: interval sizes between consecutive primes
        (iterate ((index-i (scan-range :from 1 :upto (1- prime-count))))
          (setf (aref weights index-i) (- (aref primes (1+ index-i)) (aref primes index-i))))
        (when (> prime-count 0)
          (setf (aref weights prime-count) (- (1+ limit-n) (aref primes prime-count))))
        
        ;; Leaf initialization
        (iterate ((index-i (scan-range :from 1 :upto prime-count)))
          (let ((pos (+ num-leaves index-i -1)))
            (setf (aref tree-prod pos) 1)
            (setf (aref tree-sum pos) (aref weights index-i))))
            
        ;; Bottom-up Tree Build
        (iterate ((pos (scan-range :from (1- num-leaves) :downto 1 :by -1)))
          (let ((left (ash pos 1))
                (right (1+ (ash pos 1))))
            (setf (aref tree-prod pos) (mod (* (aref tree-prod left) (aref tree-prod right)) $modulus))
            (setf (aref tree-sum pos) (mod (+ (aref tree-sum left) 
                                              (* (aref tree-prod left) (aref tree-sum right))) 
                                           $modulus))))

        ;; 3. Bijective Generation (Iterating Combinations)
        ;; For r=0, all prime exponents are 0.
        (let ((w-0 (mod (1+ (aref tree-sum 1)) $modulus)))
          (setf total-f (mod (+ total-f (if (and (evenp limit-n) (= 0 half-limit)) w-0 (* 2 w-0))) $modulus)))

        (labels ((extract-factors (target-x delta)
                   (declare (type (unsigned-byte 32) target-x)
                            (type (signed-byte 32) delta))
                   (when (> target-x 1)
                     (let ((prime-idx (aref spf-idx target-x)))
                       (when (zerop (aref diff-counts prime-idx))
                         (setf (aref changed-indices num-changed) prime-idx)
                         (incf num-changed))
                       (incf (aref diff-counts prime-idx) delta)
                       (extract-factors (truncate target-x (aref primes prime-idx)) delta)))))

          (iterate ((index-r (scan-range :from 1 :upto half-limit)))
            (setf num-changed 0)
            
            ;; O(1) amortized factorization
            (extract-factors (- limit-n index-r -1) 1)
            (extract-factors index-r -1)
            
            ;; Propagate to Segment Tree
            (iterate ((c (scan-range :from 0 :below num-changed)))
              (let* ((prime-idx (aref changed-indices c))
                     (delta (aref diff-counts prime-idx)))
                (declare (type (unsigned-byte 32) prime-idx)
                         (type (signed-byte 32) delta))
                (unless (zerop delta)
                  (let ((v (aref current-val prime-idx)))
                    (declare (type (unsigned-byte 64) v))
                    (if (> delta 0)
                        (iterate ((_ (scan-range :from 0 :below delta)))
                          (setf v (mod (* v (aref primes prime-idx)) $modulus)))
                        (iterate ((_ (scan-range :from 0 :below (- delta))))
                          (setf v (mod (* v (aref inv-primes prime-idx)) $modulus))))
                    (setf (aref current-val prime-idx) v)
                    (update-segment-tree prime-idx v tree-prod tree-sum weights num-leaves))
                  (setf (aref diff-counts prime-idx) 0))))
            
            ;; Aggregate
            (let* ((w-r (mod (1+ (aref tree-sum 1)) $modulus))
                   (weight (if (and (evenp limit-n) (= index-r half-limit)) 1 2)))
              (setf total-f (mod (+ total-f (* weight w-r)) $modulus)))))
        
        total-f))))

;; ------------------------------------------------------------
;; Main Solver API
;; ------------------------------------------------------------

(defun solve ()
  "Computes F(n) enforcing the 1-minute rule."
  ;; Defensive Trace Execution against Boundary Conditions
  (format t "Trace F(11) = ~A (Expected 3132)~%" (solve-for-n 11))
  (format t "Trace F(1111) mod 1,000,000,993 = ~A (Expected 706036312)~%" (solve-for-n 1111))
  (format t "Trace F(111111) mod 1,000,000,993 = ~A (Expected 22156169)~%" (solve-for-n 111111))
  
  ;; Execution for target N
  (let ((ans (solve-for-n 11111111)))
    (format t "F(11111111) mod 1,000,000,993 = ~A~%" ans)
    ans))

#+| Do it | (project-euler-0468:solve)
#|------------------------------------------------------------|
Timing the evaluation of (solve)
Trace F(11) = 3132 (Expected 3132)
Trace F(1111) mod 1,000,000,993 = 706036312 (Expected 706036312)
Trace F(111111) mod 1,000,000,993 = 22156169 (Expected 22156169)
F(11111111) mod 1,000,000,993 = 852950321

User time    =       38.536
System time  =        0.320
Elapsed time =       38.778
Allocation   = 107680904 bytes
44042 Page faults
GC time      =        0.044
 |------------------------------------------------------------|#
;;→ 852950321
:ok


#|------------------------------------------------------------|
Timing the evaluation of (solve)
Trace F(11) = 3132 (Expected 3132)
Trace F(1111) mod 1,000,000,993 = 706036312 (Expected 706036312)
Trace F(111111) mod 1,000,000,993 = 22156169 (Expected 22156169)
F(11111111) mod 1,000,000,993 = 852950321

User time    =       20.760
System time  =        0.191
Elapsed time =       20.874
Allocation   = 107444328 bytes
32286 Page faults
GC time      =        0.035
 |------------------------------------------------------------|#
;;→ 852950321
