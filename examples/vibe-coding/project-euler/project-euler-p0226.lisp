;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3.5-pro
(cl:in-package cl-user)
(defpackage #:project-euler-0226 (:use cl iterate alexandria) (:export #:solve))
(in-package #:project-euler-0226)

#||
(cl:comment "PE 226 Mathematical Constraints and Shortcuts")
(cl:comment "Invariant 1: The Blancmange curve has a self-similar fractal structure. Its definite integral I(x) = ∫_0^x B(t) dt inherently satisfies the recurrence relation I(x) = x^2/2 + I(2x)/4 for 0 <= x <= 1/2.")
(cl:comment "Invariant 2: The area enclosed by the curve and the circle lies exactly between the intersection point x_0 and the circle's center-right edge x = 0.5. Given the domain, exactly one intersection x_0 exists in (0, 0.5).")
(cl:comment "Shortcut 1: We completely avoid numerical integration (Riemann sums) over a fractal curve. By recursively evaluating the exact analytical formula for I(x), we achieve O(-log(eps)) time complexity. The recursion is safely truncated at depth 65, which far exceeds the 53-bit mantissa limit of IEEE 754 double-precision.")
(cl:comment "Shortcut 2: The integral of the circle's lower boundary C_lower(x) = 0.5 - sqrt(0.25^2 - (x - 0.25)^2) is computed purely analytically using the arcsin function, sidestepping any geometric approximation errors.")
||#

(defun distance-to-nearest-integer (x)
  "Calculates the distance from x to the nearest integer: min(x - floor(x), ceil(x) - x)."
  (let ((fractional-part (- x (ffloor x))))
    (cond
      ((< fractional-part 0.5d0) fractional-part)
      (t (- 1.0d0 fractional-part)))))

(defun compute-blancmange-value (x)
  "Computes the value of the Blancmange curve B(x) at point x using a loop to avoid stack overhead."
  (let ((sum 0.0d0)
        (term-scale 1.0d0)
        (current-x x))
    (iterate (for step from 0 to 65)
      (incf sum (* term-scale (distance-to-nearest-integer current-x)))
      (setf term-scale (/ term-scale 2.0d0))
      (setf current-x (* current-x 2.0d0)))
    sum))

(defun compute-circle-lower-boundary (x)
  "Computes the lower y-value of the circle C at point x."
  (let* ((dx (- x 0.25d0))
         (radius-squared 0.0625d0)
         (value-inside-sqrt (- radius-squared (* dx dx))))
    (cond
      ;; Clamp for tiny floating point errors that might push x slightly outside the valid domain
      ((< value-inside-sqrt 0.0d0) 0.5d0) 
      (t (- 0.5d0 (sqrt value-inside-sqrt))))))

(defun find-intersection-x ()
  "Finds the intersection x_0 between the Blancmange curve and the lower circle boundary using binary search."
  (let ((left-bound 0.0d0)
        (right-bound 0.5d0)
        (mid-point 0.0d0))
    (iterate (for step from 0 to 65)
      (setf mid-point (/ (+ left-bound right-bound) 2.0d0))
      (let* ((curve-y (compute-blancmange-value mid-point))
             (circle-y (compute-circle-lower-boundary mid-point))
             (difference (- curve-y circle-y)))
        (cond
          ((< difference 0.0d0) (setf left-bound mid-point))
          (t (setf right-bound mid-point)))))
    mid-point))

(defun integral-blancmange (x depth)
  "Computes the exact integral of the Blancmange curve from 0 to x using its fractal self-similarity."
  (cond
    ((>= depth 65) 0.0d0)
    ((<= x 0.0d0) 0.0d0)
    ((>= x 1.0d0) 0.5d0)
    ((<= x 0.5d0)
     (+ (/ (* x x) 2.0d0)
        (/ (integral-blancmange (* 2.0d0 x) (1+ depth)) 4.0d0)))
    (t
     (- 0.5d0 (integral-blancmange (- 1.0d0 x) (1+ depth))))))

(defun integral-circle-lower (x)
  "Computes the exact analytical integral of the circle's lower boundary from 0 to x."
  (let* ((u (- x 0.25d0))
         (radius 0.25d0)
         (radius-squared 0.0625d0)
         (value-inside-sqrt (- radius-squared (* u u)))
         (sqrt-value (cond ((< value-inside-sqrt 0.0d0) 0.0d0)
                           (t (sqrt value-inside-sqrt)))))
    (- (* 0.5d0 x)
       (* 0.5d0 u sqrt-value)
       (* 0.5d0 radius-squared (asin (/ u radius))))))

(defun solve ()
  (let* ((intersection-x (find-intersection-x))
         ;; The enclosed area is bounded between the intersection x_0 and x = 0.5
         (area-blancmange (- (integral-blancmange 0.5d0 0)
                             (integral-blancmange intersection-x 0)))
         (area-circle (- (integral-circle-lower 0.5d0)
                         (integral-circle-lower intersection-x)))
         (enclosed-area (- area-blancmange area-circle)))
    
    (format t "Intersection X: ~10,8F~%" intersection-x)
    (format t "Enclosed Area: ~10,8F~%" enclosed-area)
    
    ;; The answer must be a string rounded to eight decimal places.
    (format nil "~,8F" enclosed-area)))


#+| Do it | (solve )
#|------------------------------------------------------------|
Timing the evaluation of (solve)
Intersection X: 0.07890779
Enclosed Area: 0.11316017

User time    =        0.000
System time  =        0.000
Elapsed time =        0.000
Allocation   = 450368 bytes
14 Page faults
GC time      =        0.000
 |------------------------------------------------------------|#
;;→ "0.11316017"
:ok