
;;; -*- mode: Lisp; coding: utf-8  -*-
(cl:in-package cl-user)
(defpackage #:project-euler-0143 (:use cl alexandria))
(in-package #:project-euler-0143)

;; ==============================================================================
;; Project Euler 0143: Investigating the Torricelli point of a triangle
;;
;; Fermat's challenge was to find a point X inside triangle ABC such that
;; p + q + r is minimized, where p=XA, q=XC, r=XB.
;; This minimum occurs at the Torricelli point T, where the angles 
;; ∠AXB, ∠BXC, ∠CXA are all 120 degrees.
;;
;; From the Law of Cosines at point T:
;; c^2 = p^2 + r^2 - 2pr cos(120°) = p^2 + r^2 + pr
;; a^2 = q^2 + r^2 - 2qr cos(120°) = q^2 + r^2 + qr
;; b^2 = p^2 + q^2 - 2pq cos(120°) = p^2 + q^2 + pq
;;
;; We need to find all distinct values of p + q + r <= 120,000 such that
;; a, b, c, p, q, r are all positive integers.
;; ==============================================================================

(defun solve-project-euler-0143 (&optional (limit 120000))
  "Finds the sum of all distinct values of p + q + r <= limit for Torricelli triangles."
  (let ((adj (make-array (1+ limit) :initial-element nil))
        (bit-vec (make-array (1+ limit) :element-type 'bit :initial-element 0))
        (sums-bit-vec (make-array (1+ limit) :element-type 'bit :initial-element 0))
        (m-limit (truncate (sqrt limit))))
    
    ;; 1. Generate all pairs (u, v) such that u^2 + uv + v^2 is a perfect square.
    ;; Using the parameterization for Eisenstein triples:
    ;; u = d(m^2 - n^2), v = d(2mn + n^2)
    ;; where m > n > 0, gcd(m, n) = 1, and (m - n) is not a multiple of 3.
    ;; This generates all primitive triples (gcd=1) exactly once.
    (loop for m from 2 to (+ m-limit 100) ; generous buffer
          do (loop for n from 1 to (1- m)
                   do (when (and (= 1 (gcd m n))
                                 (not (zerop (mod (- m n) 3))))
                        (let ((u0 (- (* m m) (* n n)))
                              (v0 (+ (* 2 m n) (* n n))))
                          ;; For each primitive pair (u0, v0), generate all multiples d.
                          (loop for d from 1
                                for u-raw = (* d u0)
                                for v-raw = (* d v0)
                                while (< (+ u-raw v-raw) limit)
                                do (let ((u (min u-raw v-raw))
                                         (v (max u-raw v-raw)))
                                     ;; Store in adjacency list to form a graph of potential sides.
                                     ;; Since u < v, we only store v in adj[u].
                                     (push v (aref adj u))))))))

    ;; 2. Find triples (p, q, r) such that all pairs (p,q), (q,r), (p,r) are in adj.
    ;; This corresponds to finding triangles in the graph.
    ;; We assume p < q < r to avoid permutations.
    (loop for p from 1 to limit
          do (let ((list-p (aref adj p)))
               (when list-p
                 ;; Mark all neighbors of p in a bit-vector for O(1) lookup.
                 (dolist (v list-p)
                   (setf (sbit bit-vec v) 1))
                 
                 ;; Iterate through pairs of neighbors (q, r) of p.
                 (dolist (q list-p)
                   (dolist (r (aref adj q))
                     ;; Check if (p, r) is also an edge and sum is within limit.
                     (let ((sum (+ p q r)))
                       (when (and (<= sum limit)
                                  (= 1 (sbit bit-vec r)))
                         ;; Store distinct sums using a bit-vector.
                         (setf (sbit sums-bit-vec sum) 1)))))
                 
                 ;; Reset bit-vector for the next p.
                 (dolist (v list-p)
                   (setf (sbit bit-vec v) 0)))))

    ;; 3. Sum all distinct values found.
    (let ((total-sum 0))
      (loop for s from 1 to limit
            do (when (= 1 (sbit sums-bit-vec s))
                 (incf total-sum s)))
      total-sum)))

;; Execute the solver and print the result.
;(format t "Result: ~A~%" (solve-project-euler-0143))

#+| Do it | (solve-project-euler-0143 )
;→ 30758397

:ok
