;;; -*- mode: Lisp; coding: utf-8  -*-
(cl:in-package cl-user)
(defpackage #:project-euler-0163 (:use cl alexandria))
(in-package #:project-euler-0163)

;; ==============================================================
;; Project Euler Problem 163: Cross-hatched Triangles
;; --------------------------------------------------------------
;; This solution uses a barycentric-like coordinate system (u, v, w)
;; such that u + v + w = 2n, where n is the size of the triangle.
;; The boundary of the large triangle is defined by u, v, w >= 0.
;;
;; The lines in the cross-hatched grid belong to 6 direction families:
;; 1. u = 2k (0°)
;; 2. v = 2k (60°)
;; 3. w = 2k (120°)
;; 4. u - v = 2k (90°)
;; 5. v - w = 2k (30°)
;; 6. w - u = 2k (150°)
;;
;; A triangle is formed by choosing 3 lines from different families
;; that are not concurrent and whose intersection points lie within
;; the boundary (u, v, w >= 0).
;; ==============================================================

(defun solve-system (line1 line2 n)
  "Calculates the intersection of two lines in the u+v+w=2n plane.
   Returns coordinates scaled by 6 to ensure they are integers."
  (let* ((a1 (first line1)) (b1 (second line1)) (c1 (third line1)) (d1 (fourth line1))
         (a2 (first line2)) (b2 (second line2)) (c2 (third line2)) (d2 (fourth line2))
         (a3 1) (b3 1) (c3 1) (d3 (* 2 n))
         ;; Determinant of the 3x3 system using Cramer's rule
         (det (+ (* a1 b2 c3) (* b1 c2 a3) (* c1 a2 b3)
                 (- (* a3 b2 c1)) (- (* b3 c2 a1)) (- (* c3 a2 b1)))))
    (if (= det 0)
        nil ; Lines are parallel
        (let ((det-u (+ (* d1 b2 c3) (* b1 c2 d3) (* c1 d2 b3)
                        (- (* d3 b2 c1)) (- (* b3 c2 d1)) (- (* c3 d2 b1))))
              (det-v (+ (* a1 d2 c3) (* d1 c2 a3) (* c1 a2 d3)
                        (- (* a3 d2 c1)) (- (* d3 c2 a1)) (- (* c3 a2 d1))))
              (det-w (+ (* a1 b2 d3) (* b1 d2 a3) (* d1 a2 b3)
                        (- (* a3 b2 d1)) (- (* b3 d2 a1)) (- (* d3 a2 b1)))))
          ;; Since det can only be 1, 2, or 3 (up to sign), scale by 6
          (let ((s (/ 6 det)))
            (list (* s det-u) (* s det-v) (* s det-w)))))))

(defun triangle-exists-p (l1 l2 l3 n)
  "Checks if three lines form a valid triangle within the boundary."
  (let ((p1 (solve-system l1 l2 n))
        (p2 (solve-system l2 l3 n)))
    ;; If intersections exist and are not concurrent
    (if (and p1 p2 (not (equal p1 p2)))
        (let ((p3 (solve-system l3 l1 n)))
          (and p3
               ;; Check if all vertices are within the triangle boundary (u,v,w >= 0)
               (>= (first p1) 0) (>= (second p1) 0) (>= (third p1) 0)
               (>= (first p2) 0) (>= (second p2) 0) (>= (third p2) 0)
               (>= (first p3) 0) (>= (second p3) 0) (>= (third p3) 0)))
        nil)))

(defun solve-t (n)
  "Calculates T(n), the total number of triangles in a size n triangle."
  (let ((families (make-array 6)))
    ;; Define the 6 families of lines in the form (a b c d) for au + bv + cw = d
    (setf (aref families 0) (loop for k from 0 to n collect (list 1 0 0 (* 2 k))))
    (setf (aref families 1) (loop for k from 0 to n collect (list 0 1 0 (* 2 k))))
    (setf (aref families 2) (loop for k from 0 to n collect (list 0 0 1 (* 2 k))))
    (setf (aref families 3) (loop for k from (- n) to n collect (list 1 -1 0 (* 2 k))))
    (setf (aref families 4) (loop for k from (- n) to n collect (list 0 1 -1 (* 2 k))))
    (setf (aref families 5) (loop for k from (- n) to n collect (list -1 0 1 (* 2 k))))
    
    (let ((count 0))
      ;; Iterate through all triplets of different direction families
      (loop for i from 0 to 5 do
        (loop for j from (1+ i) to 5 do
          (loop for k from (1+ j) to 5 do
            (dolist (l1 (aref families i))
              (dolist (l2 (aref families j))
                (dolist (l3 (aref families k))
                  (when (triangle-exists-p l1 l2 l3 n)
                    (incf count))))))))
      count)))

;; Execution
'(let ((result (solve-t 36)))
  (format t "~A~%" result))
#+| Do it | (solve-t 36)
;→ 343047

:ok