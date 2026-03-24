;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3.5-pro
(cl:in-package cl-user)
(defpackage #:project-euler-0150 (:use cl iterate alexandria) (:export #:solve))
(in-package #:project-euler-0150)

#||
(cl:comment "PE 150 Mathematical Constraints and Shortcuts")
(cl:comment "Invariant 1: A sub-triangle of size S starting at (r,c) contains all elements of the sub-triangle of size S-1 starting at (r,c) plus a row segment at row r+S-1 from column c to c+S-1.")
(forall (r c S)
  (implies (> S 1)
           (= (triangle-sum r c S)
              (+ (triangle-sum r c (- S 1))
                 (row-segment-sum (+ r S 1) c (+ c S 1))))))

(cl:comment "Constraint 1: The row segment sum can be computed in strictly O(1) time using precalculated prefix sums for each row.")
(forall (row start end)
  (= (row-segment-sum row start end)
     (- (prefix-sum row end) (prefix-sum row (- start 1)))))

(cl:comment "Shortcut: We eliminate the need to calculate the triangular indices via reverse math (sqrt). By generating the Linear Congruential values exactly within nested row-column loops, index mapping becomes implicit and computationally free.")
||#


(defun solve ()
  (let* ((total-rows 1000)
         ;; Pre-allocate a 2D array for the row prefix sums to achieve O(1) row segment lookups.
         (row-prefix-sums-array (make-array (list total-rows total-rows) :element-type 'fixnum :initial-element 0))
         (lcg-state 0)
         (lcg-multiplier 615949)
         (lcg-addend 797807)
         (lcg-modulus 1048576) ; 2^20
         (lcg-subtrahend 524288)) ; 2^19
         
    (format t "Generating LCG values and precomputing row prefix sums in O(N^2)...~%")
    ;; Generate the pseudo-random numbers and simultaneously build the prefix sum array.
    ;; This strictly avoids any expensive math like sqrt or floor.
    (iterate (for r from 0 below total-rows)
      (let ((current-row-sum 0))
        (iterate (for c from 0 to r)
          ;; Advance LCG state
          (setf lcg-state (mod (+ (* lcg-multiplier lcg-state) lcg-addend) lcg-modulus))
          (let ((s-value (- lcg-state lcg-subtrahend)))
            (incf current-row-sum s-value)
            (setf (aref row-prefix-sums-array r c) current-row-sum)))))

    (format t "Searching for the minimal sub-triangle sum (O(N^3) complexity)...~%")
    (let ((minimum-triangle-sum most-positive-fixnum))
      
      ;; Iterate over every possible starting vertex of a sub-triangle
      (iterate (for r from 0 below total-rows)
        (when (zerop (mod r 100))
          (format t "Processing row ~A / ~A...~%" r total-rows))
          
        (iterate (for c from 0 to r)
          (let ((current-subtriangle-sum 0))
            
            ;; Expand the sub-triangle size downwards incrementally
            (iterate (for size from 1 to (- total-rows r))
              (let* ((target-row (+ r size -1))
                     (target-col-end (+ c size -1))
                     (target-col-start-minus-1 (1- c))
                     ;; O(1) retrieval of the new row segment
                     (row-segment-sum (if (< target-col-start-minus-1 0)
                                          (aref row-prefix-sums-array target-row target-col-end)
                                          (- (aref row-prefix-sums-array target-row target-col-end)
                                             (aref row-prefix-sums-array target-row target-col-start-minus-1)))))
                
                ;; Add only the new row segment to the running total
                (incf current-subtriangle-sum row-segment-sum)
                
                (when (< current-subtriangle-sum minimum-triangle-sum)
                  (setf minimum-triangle-sum current-subtriangle-sum)))))))
                  
      (format t "Minimal sub-triangle sum: ~A~%" minimum-triangle-sum)
      minimum-triangle-sum)))


#+| Do it | (solve )
#|------------------------------------------------------------|
Timing the evaluation of (solve)
Generating LCG values and precomputing row prefix sums in O(N^2)...
Searching for the minimal sub-triangle sum (O(N^3) complexity)...
Processing row 0 / 1000...
Processing row 100 / 1000...
Processing row 200 / 1000...
Processing row 300 / 1000...
Processing row 400 / 1000...
Processing row 500 / 1000...
Processing row 600 / 1000...
Processing row 700 / 1000...
Processing row 800 / 1000...
Processing row 900 / 1000...
Minimal sub-triangle sum: -271248680

User time    =        4.229
System time  =        0.055
Elapsed time =        4.209
Allocation   = 8486288 bytes
6331 Page faults
GC time      =        0.018
 |------------------------------------------------------------|#
;;→ -271248680
:ok