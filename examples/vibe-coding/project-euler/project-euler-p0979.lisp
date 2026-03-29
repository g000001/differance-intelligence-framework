;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3.1-pro
(cl:in-package cl-user)
(defpackage #:project-euler-0979 (:use cl series alexandria) (:export #:solve))
(in-package #:project-euler-0979)
(eval-when (:compile-toplevel :load-toplevel :execute) (series::install))

(defmacro make-fixnum-array (size &rest keys)
  `(make-array ,size :element-type 'fixnum ,@keys))

(defmacro make-integer-array (size &rest keys)
  `(make-array ,size :element-type 'integer ,@keys))

(defconstant $max-nodes 500000)

(defparameter *adj* (make-array (list $max-nodes 7) :element-type 'fixnum))
(defparameter *deg* (make-fixnum-array $max-nodes :initial-element 0))
(defparameter *dp-current* (make-integer-array $max-nodes :initial-element 0))
(defparameter *dp-next* (make-integer-array $max-nodes :initial-element 0))

(defun add-edge (u v)
  (let ((d-u (aref *deg* u))
        (d-v (aref *deg* v)))
    (setf (aref *adj* u d-u) v)
    (setf (aref *deg* u) (1+ d-u))
    (setf (aref *adj* v d-v) u)
    (setf (aref *deg* v) (1+ d-v))))

(defun build-graph (max-d)
  (fill *deg* 0)
  (let ((node-count 2)
        (v-curr (make-array 1 :initial-element 1 :adjustable t :fill-pointer t))
        (t-curr (make-array 1 :initial-element #\A :adjustable t :fill-pointer t)))
    
    (add-edge 0 1)
    (add-edge 1 1)
    (add-edge 1 1)
    
    (iterate ((d (scan-range :from 0 :below max-d)))
      (let ((v-next (make-array 0 :adjustable t :fill-pointer t))
            (t-next (make-array 0 :adjustable t :fill-pointer t))
            (n-curr (length v-curr)))
        
        (iterate ((i (scan-range :from 0 :below n-curr)))
          (let ((v (aref v-curr i))
                (type-v (aref t-curr i))
                (prev-v (aref v-curr (mod (+ i n-curr -1) n-curr))))
            
            (let ((b-node node-count))
              (incf node-count)
              (vector-push-extend b-node v-next)
              (vector-push-extend #\B t-next)
              (add-edge b-node prev-v)
              (add-edge b-node v))
            
            (let ((num-a (if (char= type-v #\A) 2 1)))
              (iterate ((k (scan-range :from 0 :below num-a)))
                (let ((a-node node-count))
                  (incf node-count)
                  (vector-push-extend a-node v-next)
                  (vector-push-extend #\A t-next)
                  (add-edge a-node v))))))
        
        (let ((n-next (length v-next)))
          (iterate ((j (scan-range :from 0 :below n-next)))
            (let ((u (aref v-next j))
                  (w (aref v-next (mod (1+ j) n-next))))
              (add-edge u w))))
        
        (setf v-curr v-next)
        (setf t-curr t-next)))
    node-count))

(defun solve-frog (steps)
  ;; 修正: 往復距離の半分 + 1 のマージンのみを生成する
  (let ((total-nodes (build-graph (+ (floor steps 2) 1))))
    (format t "Graph built with ~A nodes.~%" total-nodes)
    (fill *dp-current* 0)
    (setf (aref *dp-current* 0) 1)
    
    (iterate ((step (scan-range :from 1 :upto steps)))
      (fill *dp-next* 0)
      
      (setf (aref *dp-next* 0) (* 7 (aref *dp-current* 1)))
      
      (iterate ((i (scan-range :from 1 :below total-nodes)))
        (let ((sum 0)
              (deg (aref *deg* i)))
          (iterate ((j (scan-range :from 0 :below deg)))
            (incf sum (aref *dp-current* (aref *adj* i j))))
          (setf (aref *dp-next* i) sum)))
      
      (rotatef *dp-current* *dp-next*)
      (format t "Step ~A: F = ~A~%" step (aref *dp-current* 0)))
    
    (aref *dp-current* 0)))

(defun solve ()
  (solve-frog 20))

#+| Do it | (project-euler-0979:solve)
#|------------------------------------------------------------|
Timing the evaluation of (solve) ;
Graph built with 75025 nodes.
Step 1: F = 0
Step 2: F = 7
Step 3: F = 14
Step 4: F = 119
Step 5: F = 490
Step 6: F = 3031
Step 7: F = 15792
Step 8: F = 92351
Step 9: F = 521654
Step 10: F = 3054317
Step 11: F = 17838128
Step 12: F = 105645183
Step 13: F = 628115670
Step 14: F = 3761925153
Step 15: F = 22630891164
Step 16: F = 136805326063
Step 17: F = 830174905490
Step 18: F = 5056211445901
Step 19: F = 30892996771462
Step 20: F = 189306828278449

User time    =        0.394
System time  =        0.013
Elapsed time =        0.354
Allocation   = 3836136 bytes
3935 Page faults
GC time      =        0.000
 |------------------------------------------------------------|#
;;→ 189306828278449
:ok