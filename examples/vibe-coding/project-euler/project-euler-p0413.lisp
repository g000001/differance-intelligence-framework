;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3.5-pro
(cl:in-package cl-user)
(defpackage #:project-euler-0413 (:use cl iterate alexandria) (:export #:solve))
(in-package #:project-euler-0413)

#||
(cl:comment "PE 413 Mathematical Constraints and Shortcuts")
(cl:comment "Invariant 1: A d-digit number is a 'one-child number' if exactly ONE of its substrings is divisible by d. The substrings can be generated incrementally by viewing them as suffixes of the growing prefixes.")
(cl:comment "Invariant 2: When appending a new digit c to a prefix, the new valid substrings are exactly the single digit c, plus (10*r + c) for each previous suffix value r. We only care about these values modulo d.")
(cl:comment "Constraint 1: The target exactly counts divisibility by d. If any remainder r modulo d appears more than 2 times in the current active suffixes, we can safely clamp its count to 2. Reason: If it transitions to 0 later, it will immediately add >= 2 to our global 'divisible count', rendering the state invalid since exactly ONE is allowed.")
(cl:comment "Shortcut: The DP state collapses to (k, c_0, ..., c_{d-1}) where k in {0, 1} is the global count of 0-remainders seen so far, and c_r in {0, 1, 2} is the clamped count of current suffixes with remainder r modulo d.")
(cl:comment "Optimization: By packing c_r into a base-3 integer, the state space compresses drastically. Since d <= 19, there are theoretically 3^19 states, but the dynamically reachable states per d are extremely sparse (a few thousands). Expanding only reachable states with memoized transitions reduces the complexity to instantaneous limits.")
||#

(declaim (inline unpack-state pack-state get-next-state))

(defun unpack-state (state d counts)
  "Unpacks the base-3 compressed state into an array of suffix remainder counts."
  (let ((state state))
    (iterate (for i from 0 below d)
      (setf (values state (aref counts i))
            (truncate state 3)))))

(defun pack-state (d counts)
  "Packs the array of suffix remainder counts into a base-3 integer."
  (let ((state 0)
        (mult 1))
    (iterate (for i from 0 below d)
      (incf state (* (aref counts i) mult))
      (setf mult (* mult 3)))
    state))

(defun get-next-state (state c d counts next-counts)
  "Calculates the next state after appending digit c, returning the packed state and the increment in 0-remainders."
  (unpack-state state d counts)
  (fill next-counts 0)
  
  ;; Transition existing suffix remainders
  (iterate (for r from 0 below d)
    (let ((cnt (aref counts r)))
      (when (> cnt 0)
        (let ((nr (mod (+ (* 10 r) c) d)))
          (incf (aref next-counts nr) cnt)))))
          
  ;; Add the new single-digit suffix
  (let ((nr (mod c d)))
    (incf (aref next-counts nr) 1))
  
  ;; The number of new 0-remainders created
  (let ((inc-k (aref next-counts 0)))
    ;; Clamp counts to 2 to massively reduce the state space
    (iterate (for i from 0 below d)
      (when (> (aref next-counts i) 2)
        (setf (aref next-counts i) 2)))
        
    (values (pack-state d next-counts) inc-k)))

(defun solve ()
  (let ((ans 0)
        ;; Pre-allocate workspace arrays to eliminate inner-loop allocations
        (counts (make-array 19 :element-type 'fixnum :initial-element 0))
        (next-counts (make-array 19 :element-type 'fixnum :initial-element 0)))
        
    ;; Process each length d from 1 to 19 independently
    (iterate (for d from 1 to 19)
      (let ((trans-cache (make-hash-table :test 'eql))
            (dp (make-array 2 :initial-element nil)))
        
        ;; Dynamic Transition Memoization
        (labels ((get-transitions (state)
                   (let ((cached (gethash state trans-cache)))
                     (if cached
                         cached
                         (let ((arr (make-array 10 :element-type 'fixnum)))
                           (iterate (for c from 0 to 9)
                             (multiple-value-bind (nstate inc-k) 
                                                  (get-next-state state c d counts next-counts)
                               ;; Pack transition info into a single integer for cache efficiency
                               (setf (aref arr c) (logior (ash nstate 8) inc-k))))
                           (setf (gethash state trans-cache) arr)
                           arr)))))
          
          (setf (aref dp 0) (make-hash-table))
          (setf (aref dp 1) (make-hash-table))
          
          ;; Base case: length 1 (First digit 1-9, no leading zero)
          (iterate (for c from 1 to 9)
            (multiple-value-bind (nstate inc-k)
                                 (get-next-state 0 c d counts next-counts)
              (when (<= inc-k 1)
                (incf (gethash nstate (aref dp inc-k) 0) 1))))
          
          ;; Step forward digit by digit
          (iterate (for len from 2 to d)
            (let ((next-dp (make-array 2 :initial-element nil)))
              (setf (aref next-dp 0) (make-hash-table))
              (setf (aref next-dp 1) (make-hash-table))
              
              (iterate (for k from 0 to 1)
                (let ((table (aref dp k)))
                  (iterate (for (state ways) in-hashtable table)
                    (let ((arr (get-transitions state)))
                      (iterate (for c from 0 to 9)
                        (let* ((val (aref arr c))
                               (nstate (ash val -8))
                               (inc-k (logand val 255))
                               (new-k (+ k inc-k)))
                          (when (<= new-k 1)
                            (incf (gethash nstate (aref next-dp new-k) 0) ways))))))))
              (setf dp next-dp)))
          
          ;; Aggregate valid configurations
          (let ((d-ans 0))
            (iterate (for (state ways) in-hashtable (aref dp 1))
              (declare (ignore state))
              (incf d-ans ways))
            (incf ans d-ans)))))
            
    (format t "Final Answer F(10^19): ~A~%" ans)
    ans))


#+| Do it | (solve )
#|------------------------------------------------------------|
Timing the evaluation of (solve)
Final Answer F(10^19): 3079418648040719

User time    =       56.745
System time  =        0.765
Elapsed time =       57.252
Allocation   = 776569016 bytes
213304 Page faults
GC time      =        0.1245
 |------------------------------------------------------------|#
;;→ 3079418648040719
:ok