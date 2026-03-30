;;; -*- mode: Lisp; coding: utf-8 -*-
;;; llm-model: gemini-3.1-pro
(cl:in-package cl-user)
(defpackage #:project-euler-0930 (:use cl series alexandria) (:export #:solve))
(in-package #:project-euler-0930)
(eval-when (:compile-toplevel :load-toplevel :execute) (series::install))

(declaim (optimize (speed 3) (safety 0) (debug 0)))

;; ------------------------------------------------------------
;; Exact Arithmetic & Scientific Formatting
;; ------------------------------------------------------------

(defun format-scientific-custom (val)
  "Formats double-float to 12 decimal places scientific notation and standardizes the exponent."
  (let* ((str (format nil "~,12E" val))
         (str (string-downcase str)))
    ;; Normalize exponent characters
    (setf str (substitute #\e #\d str))
    (setf str (substitute #\e #\f str))
    (setf str (substitute #\e #\l str))
    (setf str (substitute #\e #\s str))
    ;; Remove explicit positive sign in exponent
    (let ((pos (search "e+" str)))
      (when pos
        (setf str (concatenate 'string (subseq str 0 pos) "e" (subseq str (+ pos 2))))))
    ;; Strip leading zeros from the numeric part of the exponent
    (let ((pos (search "e" str)))
      (when pos
        (let* ((prefix (subseq str 0 (1+ pos)))
               (suffix (subseq str (1+ pos)))
               (sign "")
               (num-part suffix))
          (when (char= (char suffix 0) #\-)
            (setf sign "-")
            (setf num-part (subseq suffix 1)))
          (iterate ((_ (scan-range)))
            (if (and (> (length num-part) 1) (char= (char num-part 0) #\0))
                (setf num-part (subseq num-part 1))
                (terminate-producing)))
          (setf str (concatenate 'string prefix sign num-part)))))
    str))

;; ------------------------------------------------------------
;; Phase Space Projection (Quotient Group Spectra)
;; ------------------------------------------------------------

(defun compute-f (n m fact)
  "Computes F(n,m) utilizing the spectral decomposition over the quotient group."
  (declare (type fixnum n m)
           (type (simple-array (unsigned-byte 64) (*)) fact))
  (let ((sum 0.0d0)
        (c-err 0.0d0)
        (cosines (make-array n :element-type 'double-float)))
    
    ;; Precompute transcendental eigenvalues
    (iterate ((i (scan-range :from 0 :below n)))
      (setf (aref cosines i) (cos (/ (* 2.0d0 (coerce pi 'double-float) i) n))))
      
    (labels ((kahan-add (val)
               (declare (type double-float val))
               (let* ((y (- val c-err))
                      (t-val (+ sum y)))
                 (setf c-err (- (- t-val sum) y))
                 (setf sum t-val)))
             
             ;; DFS enumerating multisets of size m from n elements
             (dfs (k rem-m sum-mod sum-cos current-mult)
               (declare (type fixnum k rem-m sum-mod)
                        (type double-float sum-cos)
                        (type (unsigned-byte 64) current-mult))
               (if (= k (1- n))
                   (let ((c-k rem-m))
                     ;; Condition: \sum q_j \equiv 0 (mod n)
                     (when (zerop (mod (+ sum-mod (* k c-k)) n))
                       (let* ((mult (floor current-mult (aref fact c-k)))
                              (final-sum-cos (+ sum-cos (* c-k (aref cosines k)))))
                         ;; Exclude the origin corresponding to \lambda_0 = 1 (c_0 = m)
                         (unless (and (= mult 1)
                                      (< (abs (- final-sum-cos (coerce m 'double-float))) 1d-6))
                           (let ((val (/ (coerce mult 'double-float) 
                                         (- 1.0d0 (/ final-sum-cos m)))))
                             (kahan-add val))))))
                   (iterate ((c-k (scan-range :from 0 :upto rem-m)))
                     (dfs (1+ k)
                          (- rem-m c-k)
                          (mod (+ sum-mod (* k c-k)) n)
                          (+ sum-cos (* c-k (aref cosines k)))
                          (floor current-mult (aref fact c-k)))))))
      
      (dfs 0 m 0 0.0d0 (aref fact m))
      sum)))

(defun solve-g (limit-n limit-m)
  "Calculates G(N, M) accumulating all expectations."
  (let ((fact (make-array 20 :element-type '(unsigned-byte 64)))
        (total-sum 0.0d0)
        (total-c-err 0.0d0))
    ;; Initialize Factorials
    (setf (aref fact 0) 1)
    (iterate ((i (scan-range :from 1 :upto 19)))
      (setf (aref fact i) (* i (aref fact (1- i)))))
    
    (labels ((global-add (val)
               (declare (type double-float val))
               (let* ((y (- val total-c-err))
                      (t-val (+ total-sum y)))
                 (setf total-c-err (- (- t-val total-sum) y))
                 (setf total-sum t-val))))
      
      (iterate ((n (scan-range :from 2 :upto limit-n)))
        (iterate ((m (scan-range :from 2 :upto limit-m)))
          (global-add (compute-f n m fact)))))
    total-sum))

;; ------------------------------------------------------------
;; Main Solver API
;; ------------------------------------------------------------

(defun solve ()
  "Entry point for Project Euler 930."
  ;; Defensive Trace Execution against Boundary Conditions
  (format t "Trace G(3, 3) = ~A (Expected 11.416666666667 / 137/12)~%" 
          (format-scientific-custom (solve-g 3 3)))
  (format t "Trace G(4, 5) = ~A (Expected 523.08333333333 / 6277/12)~%" 
          (format-scientific-custom (solve-g 4 5)))
  (format t "Trace G(6, 6) = ~A (Expected 1.681521567954e4)~%" 
          (format-scientific-custom (solve-g 6 6)))
  
  (let* ((ans (solve-g 12 12))
         (formatted-ans (format-scientific-custom ans)))
    (format t "G(12, 12) = ~A~%" formatted-ans)
    formatted-ans))

#+| Do it | (project-euler-0930:solve)
#|------------------------------------------------------------|
Timing the evaluation of (solve)
Trace G(3, 3) = 1.141666666667e1 (Expected 11.416666666667 / 137/12)
Trace G(4, 5) = 5.230833333333e2 (Expected 523.08333333333 / 6277/12)
Trace G(6, 6) = 1.681521567954e4 (Expected 1.681521567954e4)
G(12, 12) = 1.345679959251e12

User time    =        0.651
System time  =        0.018
Elapsed time =        0.607
Allocation   = 554953152 bytes
325 Page faults
GC time      =        0.005
 |------------------------------------------------------------|#
;;→ "1.345679959251e12"
:ok