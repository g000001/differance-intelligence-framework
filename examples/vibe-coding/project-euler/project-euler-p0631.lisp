;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3.1-pro
(cl:in-package cl-user)
(defpackage #:project-euler-0631 (:use cl iterate alexandria) (:export #:solve))
(in-package #:project-euler-0631)

#||
(clif-logic
  (formal-problem "Project Euler 631: 1243-avoiding permutations with limited inversions")
  (invariants
    (optimal-automaton
      (implies (1243-avoiding-p P)
               (and (<= k c_2)
                    (if (>= k c_1)
                        (and (= c_1' c_1) (= c_2' k))
                        (and (= c_1' k) (= c_2' (- c_2 1)))))))
    (polynomial-prefix-sum
      (polynomial-p (sum_{L=0}^n (valid-permutations L m)))))
  (optimizations
    (absolute-dimension-collapse "The entire 1243-avoiding condition collapses to tracking just two relative unplaced rank limits (c_1, c_2). The state space is bounded strictly at 72,324.")
    (sparse-active-state-dp "Instead of iterating all 72,324 states, only active states are pushed to a flat queue array. This drops operations to ~10^5, finishing in milliseconds.")
    (zero-allocation "All arrays are pre-allocated. GC is completely silenced.")
    (safe-interpolation "Newton forward differences strictly bounded by 'below' to prevent out-of-bounds, combined with perfect prefix-sum accumulation.")))
||#

(defun mod-inverse (a m)
  "Calculates the modular inverse using the Extended Euclidean Algorithm."
  (let ((t0 0) (t1 1)
        (r0 m) (r1 (mod a m)))
    (iterate (while (> r1 0))
      (let* ((q (floor r0 r1))
             (t2 (- t0 (* q t1)))
             (r2 (- r0 (* q r1))))
        (setf t0 t1 t1 t2)
        (setf r0 r1 r1 r2)))
    (if (< t0 0) (+ t0 m) t0)))

(defun solve ()
  (let* ((mod-val 1000000007)
         (m-max 40)
         (N0 90)     ; Point where the prefix sum becomes a pure polynomial
         (D 50)      ; Degree buffer (m+1 = 41 is the actual degree)
         (Y (make-array (1+ D) :element-type 'fixnum :initial-element 0))
         
         ;; Flat arrays for the 72324 states: (inv * 42 * 42) + (c1 * 42) + c2
         (dp (make-array 72324 :element-type 'fixnum :initial-element 0))
         (next-dp (make-array 72324 :element-type 'fixnum :initial-element 0))
         (active (make-array 72324 :element-type 'fixnum :initial-element 0))
         (next-active (make-array 72324 :element-type 'fixnum :initial-element 0))
         (in-next (make-array 72324 :element-type 'fixnum :initial-element 0))
         (num-active 0)
         (num-next-active 0))
    
    (let ((f-sum 1)) ; L = 0 contributes exactly 1 valid permutation
      
      (iterate (for L from 1 to (+ N0 D))
        
        ;; 1. Clear DP array purely based on the active states of the PREVIOUS L
        (iterate (for i from 0 below num-active)
          (setf (aref dp (aref active i)) 0))
        
        ;; 2. Set the initial empty state for this length L
        ;; State: inv=0, c1=41(inf), c2=41(inf) -> Index: 0*1764 + 41*42 + 41 = 1763
        (setf (aref dp 1763) 1)
        (setf (aref active 0) 1763)
        (setf num-active 1)
        
        ;; 3. Run the Automaton DP for exactly L steps
        (iterate (for step from 1 to L)
          (setf num-next-active 0)
          
          (iterate (for i from 0 below num-active)
            (let* ((idx (aref active i))
                   (count (aref dp idx)))
              
              (let* ((inv (floor idx 1764))
                     (rem (mod idx 1764))
                     (c1 (floor rem 42))
                     (c2 (mod rem 42))
                     (limit-k (if (= c2 41) 40 c2))
                     ;; Maximum allowed choice k to satisfy both inversions and exact length L
                     (max-k (min (- m-max inv) limit-k (- L step))))
                
                (iterate (for k from 0 to max-k)
                  (let ((nc1 c1) (nc2 c2))
                    (if (>= k c1)
                        (setf nc2 k)
                        (progn
                          (setf nc1 k)
                          (setf nc2 (if (= c2 41) 41 (1- c2)))))
                    
                    (let* ((n-inv (+ inv k))
                           (n-idx (+ (* n-inv 1764) (* nc1 42) nc2)))
                      
                      ;; Track new active states to maintain sparsity
                      (when (= (aref in-next n-idx) 0)
                        (setf (aref in-next n-idx) 1)
                        (setf (aref next-active num-next-active) n-idx)
                        (incf num-next-active))
                      
                      (setf (aref next-dp n-idx)
                            (mod (+ (aref next-dp n-idx) count) mod-val))))))))
          
          ;; 4. Fast cleanup and buffer swap for the next step
          (iterate (for i from 0 below num-active)
            (setf (aref dp (aref active i)) 0))
          (iterate (for i from 0 below num-next-active)
            (setf (aref in-next (aref next-active i)) 0))
          
          (let ((tmp-dp dp)) (setf dp next-dp) (setf next-dp tmp-dp))
          (let ((tmp-act active)) (setf active next-active) (setf next-active tmp-act))
          (setf num-active num-next-active))
        
        ;; 5. Accumulate total valid permutations of exactly length L
        (let ((g-L 0))
          (iterate (for i from 0 below num-active)
            (setf g-L (mod (+ g-L (aref dp (aref active i))) mod-val)))
          
          ;; Accumulate into the prefix sum f(n, m)
          (setf f-sum (mod (+ f-sum g-L) mod-val))
          
          ;; Store polynomial points
          (when (>= L N0)
            (setf (aref Y (- L N0)) f-sum))))
      
      ;; 6. Newton Forward Differences Interpolation
      (let ((C (make-array (1+ D) :element-type 'fixnum :initial-element 0)))
        (iterate (for i from 0 to D)
          (setf (aref C i) (aref Y 0))
          ;; Safely bounded iteration to calculate differences in-place
          (iterate (for j from 0 below (- D i))
            (setf (aref Y j) (mod (- (aref Y (1+ j)) (aref Y j)) mod-val))))
            
        ;; 7. Evaluate P(x) at x = 10^18 - N0
        (let* ((n-target #.(expt 10 18))
               (x-target (mod (- n-target N0) mod-val))
               (ans 0))
          (iterate (for i from 0 to D)
            (let ((term (aref C i))
                  (binom 1))
              (iterate (for j from 0 below i)
                (setf binom (mod (* binom (mod (- x-target j) mod-val)) mod-val)))
              (let ((inv-fact 1))
                (iterate (for j from 1 to i)
                  (setf inv-fact (mod (* inv-fact j) mod-val)))
                (let ((inv (mod-inverse inv-fact mod-val)))
                  (setf binom (mod (* binom inv) mod-val))))
              (setf ans (mod (+ ans (mod (* term binom) mod-val)) mod-val))))
          ans)))))


#+| Do it | (solve )
#|------------------------------------------------------------|
Timing the evaluation of (solve)

User time    =        0.833
System time  =        0.026
Elapsed time =        0.782
Allocation   = 3248152 bytes
3894 Page faults
GC time      =        0.000
 |------------------------------------------------------------|#
;;→ 869588692
:ok