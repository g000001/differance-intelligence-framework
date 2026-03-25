;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3.5-pro
(cl:in-package cl-user)
(defpackage #:project-euler-0765 (:use cl iterate alexandria) (:export #:solve))
(in-package #:project-euler-0765)

#||
(cl:comment "PE 765 Mathematical Constraints and Shortcuts")
(cl:comment "Invariant 1: The betting game forms a fair martingale under a risk-neutral measure with coin probability 1/2. Expected value of final wealth under this measure is conserved: E_Q[X_N] = X_0.")
(cl:comment "Invariant 2: Any non-negative final wealth distribution X_N(s) satisfying E_Q[X_N] <= X_0 and V_win >= V_loss can be achieved with a valid non-negative betting strategy (b_t >= 0, b_t <= X_t).")
(cl:comment "Constraint 1: The 'cost' to assign a target payout of 10^12 to a single final state (a specific 1000-toss sequence) is exactly 10^12 * (1/2)^N.")
(cl:comment "Constraint 2: With initial wealth X_0 = 1, the maximum number of such winning states we can afford is M = floor(2^N / 10^12).")
(cl:comment "Shortcut: We apply the Neyman-Pearson lemma. To maximize the real probability (where win rate p=0.6), we greedily 'buy' states with the highest cost-efficiency. Efficiency is strictly increasing with the number of wins k, so we select states starting from k=N downwards until we hit the capacity M.")
(cl:comment "Self-Correction: To ensure the non-negative bet constraint (V_win >= V_loss) is maintained even when we only partially buy states in the boundary layer k_min, we don't even need to be selective. Because we buy ALL states for k > k_min, any 'win' branch from any node inherently leads to a subtree with more (or equal) target states than its 'loss' counterpart. This mathematically guarantees V_win >= V_loss without any additional logic.")
||#

(defun solve ()
  (let* ((number-of-rounds 1000)
         (target-wealth #.(expt 10 12))
         ;; Total possible outcomes for 1000 coin tosses (2^1000)
         (total-states (ash 1 number-of-rounds)) 
         ;; Maximum number of outcomes we can afford to make "Trillionaire" states
         (max-states-to-buy (floor total-states target-wealth))
         (bought-states 0)
         (total-probability-rational 0)
         
         ;; Iteration variables for Binomial coefficient C(n, k) and Probabilities
         (combinations-k 1) ; Starting at C(1000, 1000)
         (power-of-3 (expt 3 number-of-rounds)) ; 3^1000 (representing 0.6^1000 scaled)
         (power-of-2 1)                         ; 2^0 (representing 0.4^0 scaled)
         (probability-denominator (expt 5 number-of-rounds))) ; 5^1000
         
    (format t "Total states to buy: ~A~%" max-states-to-buy)
    (format t "Starting greedy state acquisition from k=1000 downwards...~%")
    
    (iterate (for k from number-of-rounds downto 0)
      (let ((probability-numerator (* power-of-3 power-of-2)))
        ;; Check if we can afford all combinations at the current win count 'k'
        (cond ((<= (+ bought-states combinations-k) max-states-to-buy)
               (incf bought-states combinations-k)
               (incf total-probability-rational (* combinations-k (/ probability-numerator probability-denominator))))
              (T (let ((remaining-states-needed (- max-states-to-buy bought-states)))
                   ;; Buy only the remaining affordable states and break
                   (incf total-probability-rational (* remaining-states-needed (/ probability-numerator probability-denominator)))
                   (finish)))))
      
      ;; Efficiently update values for the next iteration (k-1)
      (when (> k 0)
        (setf combinations-k (/ (* combinations-k k) (- number-of-rounds k -1)))
        (setf power-of-3 (/ power-of-3 3))
        (setf power-of-2 (* power-of-2 2))))
        
    ;; Format the exact rational to a string with exactly 10 decimal digits
    ;; Multiplying by 10^10 and rounding allows precise extraction of the integer and fractional parts
    (let* ((scaling-factor (expt 10 10))
           (scaled-probability (round (* total-probability-rational scaling-factor)))
           (integer-part (truncate scaled-probability scaling-factor))
           (fractional-part (mod scaled-probability scaling-factor))
           (answer-string (format nil "~D.~10,'0D" integer-part fractional-part)))
           
      (format t "Probability of becoming a trillionaire: ~A~%" answer-string)
      answer-string)))

#+| Do it | (solve )
#|------------------------------------------------------------|
Timing the evaluation of (solve)
Total states to buy: 10715086071862673209484250490600018105614048117055336074437503883703510511249361224931983788156958581275946729175531468251871452856923140435984577574698574803934567774824230985421074605062371141877954182153046474983581941267398767559165543946077062914571196477686542167660429831652624386837
Starting greedy state acquisition from k=1000 downwards...
Probability of becoming a trillionaire: 0.2429251641

User time    =        0.145
System time  =        0.011
Elapsed time =        0.091
Allocation   = 32362576 bytes
381 Page faults
GC time      =        0.000
 |------------------------------------------------------------|#
;;→ "0.2429251641"
:ok