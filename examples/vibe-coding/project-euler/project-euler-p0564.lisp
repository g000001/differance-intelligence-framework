;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3.1-pro
(cl:in-package cl-user)
(defpackage #:project-euler-0564 (:use cl iterate alexandria) (:export #:solve))
(in-package #:project-euler-0564)

(defmacro optimized-code-p (boole)
  (typecase boole
    (null nil)
    (T `(declaim (optimize (speed 3) (safety 0) (debug 0))))))

(optimized-code-p T)


(defvar *factorials* (make-array 100 :initial-element 1))
(iterate (for index from 1 to 99)
  (setf (aref *factorials* index) (* (aref *factorials* (1- index)) index)))

(defun compute-factorial (number)
  (aref *factorials* number))

(defun binomial-coeff (top bottom)
  (if (or (< bottom 0) (> bottom top))
      0
      (let ((result 1))
        (iterate (for index from 1 to bottom)
          (setf result (/ (* result (+ (- top index) 1)) index)))
        result)))

(defun total-permutations (num-sides)
  (binomial-coeff (- (* 2 num-sides) 4) (1- num-sides)))

(defun map-partitions (callback target-val max-val current-part)
  (cond ((= target-val 0)
         (funcall callback current-part))
        (t
         (iterate (for part-val from (min target-val max-val) downto 1)
           (let ((next-part (if (and current-part (= (caar current-part) part-val))
                                (cons (cons part-val (1+ (cdar current-part))) (cdr current-part))
                                (cons (cons part-val 1) current-part))))
             (map-partitions callback (- target-val part-val) part-val next-part))))))

(defun build-signed-edges (edge-list max-edge is-internal-p)
  (if is-internal-p
      (mapcar (lambda (edge-cell) (list (car edge-cell) (cdr edge-cell) 1.0d0)) edge-list)
      (let ((result-edges nil)
            (found-max-p nil))
        (dolist (edge-cell edge-list)
          (let ((edge-len (car edge-cell))
                (edge-count (cdr edge-cell)))
            (if (and (= edge-len max-edge) (not found-max-p))
                (progn
                  (setf found-max-p t)
                  (push (list edge-len 1 -1.0d0) result-edges)
                  (if (> edge-count 1)
                      (push (list edge-len (1- edge-count) 1.0d0) result-edges)))
                (push (list edge-len edge-count 1.0d0) result-edges))))
        result-edges)))

(defun solve-curvature (signed-edges max-edge is-internal-p)
  (let* ((target-angle (if is-internal-p pi 0.0d0))
         (low-bound 0.0d0)
         (high-bound (/ 1.0d0 max-edge))
         (mid-val 0.0d0))
    (iterate (for iter-count from 1 to 60)
      (setf mid-val (/ (+ low-bound high-bound) 2.0d0))
      (let ((sum-angles 0.0d0))
        (dolist (edge-info signed-edges)
          (let* ((edge-len (first edge-info))
                 (edge-count (second edge-info))
                 (sign-val (third edge-info))
                 (scaled-val (* edge-len mid-val)))
            (incf sum-angles (* sign-val edge-count (asin (min 1.0d0 scaled-val))))))
        (let ((diff-angle (- sum-angles target-angle)))
          (if is-internal-p
              (if (> diff-angle 0.0d0) (setf high-bound mid-val) (setf low-bound mid-val))
              (if (< diff-angle 0.0d0) (setf high-bound mid-val) (setf low-bound mid-val))))))
    mid-val))

(defun calculate-area (signed-edges curvature-x)
  (let ((total-area 0.0d0))
    (dolist (edge-info signed-edges)
      (let* ((edge-len (first edge-info))
             (edge-count (second edge-info))
             (sign-val (third edge-info))
             (scaled-val (* edge-len curvature-x)))
        (let ((root-val (if (>= scaled-val 1.0d0) 0.0d0 (sqrt (- 1.0d0 (* scaled-val scaled-val))))))
          (incf total-area (* sign-val edge-count (/ edge-len (* 4.0d0 curvature-x)) root-val)))))
    total-area))

(defun compute-expected-area (num-sides)
  (let ((tot-perm-count (total-permutations num-sides))
        (expected-area 0.0d0))
    (map-partitions 
     (lambda (part-list)
       (let ((edge-list nil)
             (total-edge-count 0))
         (dolist (part-cell part-list)
           (push (cons (1+ (car part-cell)) (cdr part-cell)) edge-list)
           (incf total-edge-count (cdr part-cell)))
         (let ((count-ones (- num-sides total-edge-count)))
           (when (> count-ones 0)
             (push (cons 1 count-ones) edge-list)))
         
         (let ((max-edge 0))
           (dolist (edge-cell edge-list)
             (when (> (car edge-cell) max-edge)
               (setf max-edge (car edge-cell))))
           
           (let ((perm-count (compute-factorial num-sides)))
             (dolist (edge-cell edge-list)
               (setf perm-count (/ perm-count (compute-factorial (cdr edge-cell)))))
             
             (let ((sum-angles 0.0d0))
               (dolist (edge-cell edge-list)
                 (let ((edge-len (car edge-cell))
                       (edge-count (cdr edge-cell)))
                   (if (= edge-len max-edge)
                       (incf sum-angles (* (1- edge-count) (asin (min 1.0d0 (/ (coerce edge-len 'double-float) max-edge)))))
                       (incf sum-angles (* edge-count (asin (min 1.0d0 (/ (coerce edge-len 'double-float) max-edge))))))))
               
               (let* ((is-internal-p (> sum-angles (/ pi 2.0d0)))
                      (signed-edges (build-signed-edges edge-list max-edge is-internal-p))
                      (curvature-x (solve-curvature signed-edges max-edge is-internal-p))
                      (poly-area (calculate-area signed-edges curvature-x)))
                 (incf expected-area (* (coerce (/ perm-count tot-perm-count) 'double-float) poly-area))))))))
     (- num-sides 3) (- num-sides 3) nil)
    expected-area))

(defun solve ()
  (let ((sum-expected 0.0d0))
    (iterate (for num-sides from 3 to 50)
      (let ((expected-n (compute-expected-area num-sides)))
        (incf sum-expected expected-n)
        (when (member num-sides '(3 4 5 10 50))
          (format t "E(~A) = ~,6F, S(~A) = ~,6F~%" num-sides expected-n num-sides sum-expected))))
    (format nil "~,6F" sum-expected)))


#+| Do it | (solve )
#|------------------------------------------------------------|
Timing the evaluation of (solve)
E(3) = 0.433013, S(3) = 0.433013
E(4) = 1.299038, S(4) = 1.732051
E(5) = 2.872716, S(5) = 4.604767
E(10) = 21.106070, S(10) = 66.955511
E(50) = 745.741579, S(50) = 12363.698850

User time    =       26.859
System time  =        0.280
Elapsed time =       27.059
Allocation   = 25933842992 bytes
9617 Page faults
GC time      =        0.262
 |------------------------------------------------------------|#
;;→ "12363.698850"
:ok