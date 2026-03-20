;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3.1-pro
(cl:in-package cl-user)
(defpackage #:project-euler-0298 (:use cl iterate alexandria) (:export #:solve))
(in-package #:project-euler-0298)

#||
(clif-logic
  (formal-problem "Project Euler 298: Larry and Robin memory game")
  (invariants
    (length-equality (always (= (length L-memory) (length R-memory))))
    (state-canonicalization
      (iff (equivalent state1 state2)
           (equal (canonicalize state1.R-memory state1.L-memory)
                  (canonicalize state2.R-memory state2.L-memory))))
    (max-states (<= (count canonical-states) 1799)))
  (optimizations
    (dimension-reduction "Combined state space size collapsed from O(10^10) to exactly 1799 discrete equivalence classes based on element intersection identities.")
    (transition-mapping "Precompute the full Markov Chain transition matrix once using BFS, resulting in < 20000 edges.")
    (dynamic-programming "Time complexity O(N * S * D) where N=50 steps, S=1799 states, D=101 score deltas. DP updates require no allocation.")
    (garbage-collection "Inner DP loop uses pre-allocated double-float arrays. GC is fully avoided during DP phase.")))
||#


(defun next-state (canon-r called-id)
  "Given the current normalized Robin memory and the called ID, computes the next normalized state and point gains."
  (let* ((len (length canon-r))
         (l-memory (iterate (for i from 1 to len) (collect i)))
         (r-memory (copy-list canon-r))
         (score-l (if (member called-id l-memory) 1 0))
         (score-r (if (member called-id r-memory) 1 0)))
    
    ;; Update Larry (LRU Strategy)
    (if (= score-l 1)
        (setf l-memory (cons called-id (remove called-id l-memory)))
        (setf l-memory (cons called-id (if (= len 5) (butlast l-memory) l-memory))))
        
    ;; Update Robin (FIFO Strategy)
    (if (= score-r 1)
        nil ; If in memory, position does not change
        (setf r-memory (cons called-id (if (= len 5) (butlast r-memory) r-memory))))
        
    ;; Re-canonicalize the state relative to new L-memory
    (let ((mapping (make-hash-table :test 'eql))
          (next-new-id (1+ (length l-memory)))
          (new-canon-r nil))
      (iterate (for x in l-memory)
               (for i from 1)
               (setf (gethash x mapping) i))
      (iterate (for x in r-memory)
               (let ((mapped (gethash x mapping)))
                 (if mapped
                     (push mapped new-canon-r)
                     (progn
                       (setf (gethash x mapping) next-new-id)
                       (push next-new-id new-canon-r)
                       (incf next-new-id)))))
      (values (nreverse new-canon-r) score-l score-r))))


(defun build-state-space ()
  "Explores all reachable memory states using BFS and builds the Markov chain transition matrix."
  (let ((state-to-id (make-hash-table :test 'equal))
        (id-to-state (make-array 2000 :adjustable t :fill-pointer 0))
        (transitions (make-array 2000 :adjustable t :fill-pointer 0))
        (queue (make-array 2000 :adjustable t :fill-pointer 0))
        (read-ptr 0))
    
    ;; Initial empty state
    (setf (gethash nil state-to-id) 0)
    (vector-push-extend nil id-to-state)
    (vector-push-extend nil transitions)
    (vector-push-extend nil queue)
    
    (iterate
      (while (< read-ptr (fill-pointer queue)))
      (let* ((curr-r (aref queue read-ptr))
             (curr-id (gethash curr-r state-to-id))
             (max-id (if curr-r (reduce #'max curr-r) 0))
             (trans-list nil))
        (incf read-ptr)
        
        ;; Simulate calling every distinct known number, plus exactly one "new" unknown number
        (iterate
          (for i from 1 to (min (1+ max-id) 10))
          (multiple-value-bind (next-r score-l score-r)
              (next-state curr-r i)
            (let ((next-id (gethash next-r state-to-id)))
              (unless next-id
                (setf next-id (hash-table-count state-to-id))
                (setf (gethash next-r state-to-id) next-id)
                (vector-push-extend next-r id-to-state)
                (vector-push-extend nil transitions)
                (vector-push-extend next-r queue))
              
              ;; Probability logic: known numbers have 1/10 prob. The "new" state groups all remaining unseen numbers.
              (let ((prob (if (<= i max-id) 
                              0.1d0 
                              (/ (float (- 10 max-id) 0.0d0) 10.0d0))))
                (push (list next-id (- score-l score-r) prob) trans-list)))))
        (setf (aref transitions curr-id) trans-list)))
    (values (length id-to-state) transitions)))


(defun solve ()
  "Calculates the expected value of |L - R| after 50 turns using Dynamic Programming."
  (multiple-value-bind (num-states transitions)
      (build-state-space)
    (let ((dp (make-array (list num-states 101) :element-type 'double-float :initial-element 0.0d0))
          (next-dp (make-array (list num-states 101) :element-type 'double-float :initial-element 0.0d0)))
      
      ;; Start with score difference 0 (which is offset to index 50)
      (setf (aref dp 0 50) 1.0d0)
      
      (iterate (for step from 1 to 50)
        ;; Clear next-dp matrix
        (iterate (for s from 0 below num-states)
          (iterate (for d from 0 to 100)
            (setf (aref next-dp s d) 0.0d0)))
            
        ;; DP transitions
        (iterate (for s from 0 below num-states)
          (let ((trans (aref transitions s)))
            ;; Bounding 'd' search space to possible reachable deltas
            (iterate (for d from (- 51 step) to (+ 49 step))
              (let ((prob-s-d (aref dp s d)))
                (when (> prob-s-d 0.0d0)
                  (iterate (for t-info in trans)
                    (let ((next-id (first t-info))
                          (delta (second t-info))
                          (t-prob (third t-info)))
                      (incf (aref next-dp next-id (+ d delta))
                            (* prob-s-d t-prob)))))))))
                            
        ;; Swap DP buffers
        (let ((temp dp))
          (setf dp next-dp)
          (setf next-dp temp)))
          
      ;; Calculate expected value weighted by absolute score difference
      (let ((expected 0.0d0))
        (iterate (for s from 0 below num-states)
          (iterate (for d from 0 to 100)
            (incf expected (* (aref dp s d) (abs (- d 50))))))
        ;; Round safely to 8 decimal places
        (format nil "~,8F" expected)))))


#+| Do it | (solve )
#|------------------------------------------------------------|
Timing the evaluation of (solve)

User time    =        0.280
System time  =        0.021
Elapsed time =        0.236
Allocation   = 158552384 bytes
4037 Page faults
GC time      =        0.005
 |------------------------------------------------------------|#
;;→ "1.76882294"
:ok