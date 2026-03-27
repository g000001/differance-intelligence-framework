;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3-flash-preview
(cl:in-package cl-user)
(defpackage #:project-euler-0948 (:use cl iterate alexandria) (:export #:solve))
(in-package #:project-euler-0948)

(defun make-fixnum-array (size &key (initialize-element 0))
  (make-array size :element-type 'fixnum :initial-element initialize-element))

;; BDD (Binary Decision Diagram) Engine
(defparameter *bdd-count* 2)
(defparameter *bdd-var* (make-fixnum-array #.(* 5 (expt 10 6))))
(defparameter *bdd-low* (make-fixnum-array #.(* 5 (expt 10 6))))
(defparameter *bdd-high* (make-fixnum-array #.(* 5 (expt 10 6))))
(defparameter *bdd-map* (make-hash-table :test 'eql :size #.(expt 10 6)))
(defparameter *apply-cache* (make-hash-table :test 'eql :size #.(expt 10 6)))
(defparameter *count-cache* (make-hash-table :test 'eql :size #.(expt 10 6)))

(declaim (inline mk-key))
(defun mk-key (v l h)
  (declare (type fixnum v l h))
  (logior (ash v 48) (ash l 24) h))

(defun init-bdd ()
  (setf *bdd-count* 2)
  (clrhash *bdd-map*)
  (clrhash *apply-cache*)
  (clrhash *count-cache*))

(defun mk (var low high)
  (declare (type fixnum var low high))
  (if (= low high)
      low
      (let* ((key (mk-key var low high))
             (id (gethash key *bdd-map*)))
        (or id
            (let ((new-id *bdd-count*))
              (incf *bdd-count*)
              (setf (aref *bdd-var* new-id) var
                    (aref *bdd-low* new-id) low
                    (aref *bdd-high* new-id) high)
              (setf (gethash key *bdd-map*) new-id)
              new-id)))))

(defun bdd-apply (op u v)
  (declare (type fixnum op u v))
  ;; op: 0 = AND, 1 = XOR
  (when (= op 0)
    (when (or (= u 0) (= v 0)) (return-from bdd-apply 0))
    (when (= u 1) (return-from bdd-apply v))
    (when (= v 1) (return-from bdd-apply u))
    (when (= u v) (return-from bdd-apply u)))
  (when (= op 1)
    (when (= u 0) (return-from bdd-apply v))
    (when (= v 0) (return-from bdd-apply u))
    (when (= u v) (return-from bdd-apply 0)))

  (let* ((key (mk-key op u v))
         (cached (gethash key *apply-cache*)))
    (if cached
        cached
        (let* ((var-u (if (<= u 1) 1000 (aref *bdd-var* u)))
               (var-v (if (<= v 1) 1000 (aref *bdd-var* v)))
               (var (min var-u var-v))
               (low-u (if (= var-u var) (aref *bdd-low* u) u))
               (high-u (if (= var-u var) (aref *bdd-high* u) u))
               (low-v (if (= var-v var) (aref *bdd-low* v) v))
               (high-v (if (= var-v var) (aref *bdd-high* v) v))
               (low-res (bdd-apply op low-u low-v))
               (high-res (bdd-apply op high-u high-v))
               (res (mk var low-res high-res)))
          (setf (gethash key *apply-cache*) res)
          res))))

(declaim (inline bdd-not bdd-and))
(defun bdd-not (u) (bdd-apply 1 u 1))
(defun bdd-and (u v) (bdd-apply 0 u v))

(defun count-paths (u var max-var)
  (declare (type fixnum u var max-var))
  (cond
    ((= u 0) 0)
    ((= u 1) (ash 1 (1+ (- max-var var))))
    (t
     (let* ((key (mk-key u var 0))
            (cached (gethash key *count-cache*)))
       (if cached
           cached
           (let* ((u-var (aref *bdd-var* u))
                  (res
                   (if (> u-var var)
                       (* (ash 1 (- u-var var))
                          (count-paths u u-var max-var))
                       (+ (count-paths (aref *bdd-low* u) (1+ var) max-var)
                          (count-paths (aref *bdd-high* u) (1+ var) max-var)))))
             (setf (gethash key *count-cache*) res)
             res))))))

(defun solve (&optional (n 60))
  (init-bdd)
  (format t "Initializing BDD for length ~A...~%" n)
  
  (let ((A-arr (make-fixnum-array n))
        (B-arr (make-fixnum-array n)))
    
    ;; Base case: k=1
    ;; L=0, R=1. A_1 = x_i, B_1 = NOT x_i
    (iterate (for i from 0 below n)
      (let ((var (1+ i)))
        (setf (aref A-arr i) (mk var 0 1))
        (setf (aref B-arr i) (mk var 1 0))))
    
    ;; DP step: build boolean equations
    (iterate (for k from 2 to n)
      (when (= (mod k 10) 0)
        (format t "Building boolean formulas, Depth: ~A / ~A (BDD Nodes: ~A)~%" k n *bdd-count*))
      (let ((next-A (make-fixnum-array n))
            (next-B (make-fixnum-array n)))
        (iterate (for i from 0 to (- n k))
          ;; A_k(i) = A_{k-1}(i+1) AND NOT B_{k-1}(i+1)
          (setf (aref next-A i) (bdd-and (aref A-arr (1+ i)) (bdd-not (aref B-arr (1+ i)))))
          ;; B_k(i) = B_{k-1}(i) AND NOT A_{k-1}(i)
          (setf (aref next-B i) (bdd-and (aref B-arr i) (bdd-not (aref A-arr i)))))
        (setf A-arr next-A)
        (setf B-arr next-B)))
    
    (format t "Counting valid boolean assignments...~%")
    ;; We want First player to win: A_n = 0 AND B_n = 0
    (let* ((final-cond (bdd-and (bdd-not (aref A-arr 0))
                                (bdd-not (aref B-arr 0))))
           (ans (count-paths final-cond 1 n)))
      (format t "Finished. Answer: ~A~%" ans)
      ans)))


#+| Do it | (solve )
#|------------------------------------------------------------|
Timing the evaluation of (solve)
Initializing BDD for length 60...
Building boolean formulas, Depth: 10 / 60 (BDD Nodes: 1854)
Building boolean formulas, Depth: 20 / 60 (BDD Nodes: 7814)
Building boolean formulas, Depth: 30 / 60 (BDD Nodes: 16124)
Building boolean formulas, Depth: 40 / 60 (BDD Nodes: 24784)
Building boolean formulas, Depth: 50 / 60 (BDD Nodes: 31794)
Building boolean formulas, Depth: 60 / 60 (BDD Nodes: 35154)
Counting valid boolean assignments...
Finished. Answer: 1033654680825334184

User time    =        0.078
System time  =        0.008
Elapsed time =        0.055
Allocation   = 3167216 bytes
3449 Page faults
GC time      =        0.000
 |------------------------------------------------------------|#
;;→ 1033654680825334184
:ok