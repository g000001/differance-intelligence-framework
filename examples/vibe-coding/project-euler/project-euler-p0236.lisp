
;;; -*- mode: Lisp; coding: utf-8  -*-
(cl:in-package cl-user)
(defpackage #:project-euler-0236 (:use cl ))
(in-package #:project-euler-0236)

;; =============================================================================
;; Project Euler Problem 236: Luxury Hampers
;; -----------------------------------------------------------------------------
;; This solution uses the Two-Truths Slice Category framework to model the
;; relationship between conventional product quantities and ultimate spoilage rates.
;;
;; 1. Expressibility: We express the condition that per-product spoilage rates
;;    r_B,i = m * r_A,i and the overall spoilage rate R_A = m * R_B.
;; 2. Realizability: We find the largest rational m > 1 such that spoilage counts
;;    x_i and y_i are integers satisfying the constraints 1 <= x_i <= a_i and
;;    1 <= y_i <= b_i.
;; 3. Middle Way: The solution is found by iterating through candidates for m
;;    derived from the product ratios and checking for the existence of an
;;    integer solution using a Diophantine-like search.
;; =============================================================================

(defun solve ()
  (declare (optimize (speed 3) (safety 0)))
  (let* ((a-vals #(5248 1312 2624 5760 3936))
         (b-vals #(640 1888 3776 3776 5664))
         (n-vals #(5 59 59 59 59)) ; Numerators of b_i/a_i ratios
         (d-vals #(41 41 41 90 41)) ; Denominators of b_i/a_i ratios
         (m-table (make-hash-table :test 'eql)))
    
    ;; Step 1: Generate all candidate m values.
    ;; m must satisfy m = (y_i * a_i) / (x_i * b_i) for all i.
    ;; We generate candidates based on the first product's ratio.
    (let ((a1 (aref a-vals 0))
          (b1 (aref b-vals 0)))
      (loop for y1 from 1 to b1
            do (loop for x1 from 1 to a1
                     for m = (/ (* y1 a1) (* x1 b1))
                     when (> m 1)
                     do (setf (gethash m m-table) t))))
    
    ;; Step 2: Sort candidates descending to find the largest m first.
    (let ((m-list '()))
      (maphash (lambda (k v) (declare (ignore v)) (push k m-list)) m-table)
      (setf m-list (sort m-list #'>))
      
      ;; Step 3: Check each m for the existence of valid spoilage counts.
      (dolist (m m-list)
        (let* ((u (numerator m))
               (v (denominator m))
               ;; Calculate smallest integer units for x_i and y_i for this m.
               (gs (map 'vector (lambda (ni di) (gcd (* u ni) (* v di))) n-vals d-vals))
               (ls (map 'vector (lambda (di gi) (/ (* v di) gi)) d-vals gs))
               (ys (map 'vector (lambda (ni gi) (/ (* u ni) gi)) n-vals gs))
               ;; Determine the maximum range for each unit w_i.
               (w-maxs (map 'vector (lambda (ai li bi yi) (min (floor ai li) (floor bi yi))) 
                            a-vals ls b-vals ys)))
          
          ;; Ensure at least one unit exists for every product.
          (when (every (lambda (w) (>= w 1)) w-maxs)
            ;; The overall rate condition R_A = m * R_B simplifies to:
            ;; sum(w_i * E_i) = 0, where E_i = 295*u*y_i - 246*v*L_i.
            (let* ((e (map 'vector (lambda (yi li) (- (* 295 u yi) (* 246 v li))) ys ls))
                   (e1 (aref e 0))
                   (e2 (aref e 1)) ; E_2 = E_3 = E_5 due to identical product ratios.
                   (e4 (aref e 3))
                   (w1-max (aref w-maxs 0))
                   ;; W2 represents the sum of units for products 2, 3, and 5.
                   (w2-max (+ (aref w-maxs 1) (aref w-maxs 2) (aref w-maxs 4)))
                   (w4-max (aref w-maxs 3)))
              
              ;; Search for integer units w1, W2, w4 that satisfy the balance equation.
              (unless (zerop e2)
                (loop for w1 from 1 to w1-max
                      do (loop for w4 from 1 to w4-max
                               for target = (- (+ (* w1 e1) (* w4 e4)))
                               when (and (zerop (mod target e2))
                                         (let ((w2 (/ target e2)))
                                           ;; w2 must be at least 3 (one for each of the three products).
                                           (and (>= w2 3) (<= w2 w2-max))))
                               do (return-from solve m)))))))))))

;; Execute and print the result.
;;; (format t "~a~%" (time (solve)))
;;; ▻ 123/59
;;; → nil

:ok