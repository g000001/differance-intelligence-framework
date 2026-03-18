;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3.1-flash
(cl:in-package cl-user)
(defpackage #:project-euler-0680 (:use cl iterate alexandria) (:export #:solve))
(in-package #:project-euler-0680)


#||
(clif:property (PisanoPeriod Fibonacci 10^18))
(clif:algorithm (Treap (IntervalMerging)))
(clif:complexity (O K (log K)))
(clif:invariant (LinearFibonacciEvolution) (MemoryEfficientNodeReuse))
||#

;; 極限まで軽量化した構造体
(defstruct (node (:constructor %make-node (start end priority)))
  (start 0 :type (signed-byte 64))
  (end 0 :type (signed-byte 64))
  (size 0 :type (signed-byte 64))
  (priority 0 :type (signed-byte 32))
  (rev nil :type boolean)
  (left nil)
  (right nil))

(defun node-count (n) (if n (1+ (abs (- (node-end n) (node-start n)))) 0))
(defun get-size (n) (if n (node-size n) 0))

(defun update (n)
  (when n
    (setf (node-size n) (+ (node-count n) (get-size (node-left n)) (get-size (node-right n)))))
  n)

(defun push-down (n)
  (when (and n (node-rev n))
    (setf (node-rev n) nil)
    (rotatef (node-left n) (node-right n))
    (rotatef (node-start n) (node-end n))
    (let ((l (node-left n)) (r (node-right n)))
      (when l (setf (node-rev l) (not (node-rev l))))
      (when r (setf (node-rev r) (not (node-rev r))))))
  n)

(defun split (n k)
  (unless n (return-from split (values nil nil)))
  (push-down n)
  (let ((l-size (get-size (node-left n)))
        (c-count (node-count n)))
    (cond ((<= k l-size)
           (multiple-value-bind (l r) (split (node-left n) k)
             (setf (node-left n) r)
             (values l (update n))))
          ((< k (+ l-size c-count))
           (let* ((off (- k l-size))
                  (s (node-start n))
                  (e (node-end n))
                  (mid (if (<= s e) (+ s off -1) (- s off -1)))
                  (next-s (if (<= s e) (1+ mid) (1- mid)))
                  (new-r (%make-node next-s e (random (ash 1 30)))))
             (setf (node-end n) mid
                   (node-right new-r) (node-right n)
                   (node-right n) nil)
             (values (update n) (update new-r))))
          (t (multiple-value-bind (l r) (split (node-right n) (- k l-size c-count))
               (setf (node-right n) l)
               (values (update n) r))))))

(defun merge-treap (l r)
  (cond ((null l) r)
        ((null r) l)
        (t (push-down l) (push-down r)
           (if (> (node-priority l) (node-priority r))
               (progn (setf (node-right l) (merge-treap (node-right l) r)) (update l))
               (progn (setf (node-left r) (merge-treap l (node-left r))) (update r))))))

(defun solve ()
  (let* ((n (expt 10 18))
         (k (expt 10 6))
         (root (update (%make-node 0 (1- n) (random (ash 1 30)))))
         ;; Fibonacci Matrix Evolution: M = [[1,1],[1,0]]
         ;; F_{k+1}, F_k, F_{k-1}
         (f-curr 1) (f-prev 0)) ; F_1=1, F_0=0
    
    (format t "Processing ~A steps...~%" k)
    (iterate (for j from 1 to k)
      ;; F_{2j-1}
      (let ((s_j (mod f-curr n)))
        ;; F_{2j}
        (let* ((tmp (mod (+ f-curr f-prev) n))
               (t_j tmp))
          ;; Advance to F_{2j+1} and F_{2j} for next iteration
          (setf f-prev t_j
                f-curr (mod (+ t_j f-curr) n))
          
          (let* ((l (min s_j t_j))
                 (r (max s_j t_j))
                 (len (1+ (- r l))))
            (multiple-value-bind (t1 t23) (split root l)
              (multiple-value-bind (t2 t3) (split t23 len)
                (when t2 (setf (node-rev t2) (not (node-rev t2))))
                (setf root (merge-treap t1 (merge-treap t2 t3))))))))
      (when (zerop (mod j 200000)) (format t "Step ~A...~%" j)))

    (format t "Final summation...~%")
    (labels ((get-sum (node pos)
               (unless node (return-from get-sum 0))
               (push-down node)
               (let* ((ls (get-size (node-left node)))
                      (l-sum (get-sum (node-left node) pos))
                      (m-pos (+ pos ls))
                      (m-count (node-count node))
                      (s (node-start node)) (e (node-end node))
                      (sgn (if (<= s e) 1 -1))
                      ;; sum_{k=0}^{m-1} (m-pos + k)(s + sgn*k)
                      ;; = m*m-pos*s + sum(k*(m-pos*sgn + s)) + sgn*sum(k^2)
                      (term1 (* m-count m-pos s))
                      (sum-k (/ (* m-count (1- m-count)) 2))
                      (term2 (* sum-k (+ (* m-pos sgn) s)))
                      (sum-k2 (/ (* m-count (1- m-count) (1- (* 2 m-count))) 6))
                      (term3 (* sgn sum-k2))
                      (m-sum (+ term1 term2 term3))
                      (r-sum (get-sum (node-right node) (+ m-pos m-count))))
                 (+ l-sum m-sum r-sum))))
      (mod (get-sum root 0) 1000000000))))


#+| Do it | (solve )
#|------------------------------------------------------------|
Timing the evaluation of (solve)
Processing 1000000 steps...
Step 200000...
Step 400000...
Step 600000...
Step 800000...
Step 1000000...
Final summation...

User time    =  0:02:05.209
System time  =        1.345
Elapsed time =  0:02:15.992
Allocation   = 792643032 bytes
43300 Page faults
GC time      =        0.575
 |------------------------------------------------------------|#
;;→ 563917241
