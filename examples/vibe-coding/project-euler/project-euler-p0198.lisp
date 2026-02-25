;;; ARX-Core Manifestation: Project Euler 198 Solver
;;; Structural Necessity: Ambiguous numbers are midpoints of Farey neighbors.
;;; Leap: 1D Search (q=1..10^8) -> 2D Farey Tree Traversal (bd <= 5*10^7)

(declaim (optimize (speed 3) (safety 0) (debug 0)))

(defun solve-ambiguous-numbers ()
  (let* ((q-limit 100000000)
         (bd-limit (floor q-limit 2))
         (threshold-num 1)
         (threshold-den 100)
         (count 0))
    (declare (type (unsigned-byte 64) q-limit bd-limit count threshold-den))

    (labels ((dfs (a b c d)
               "Farey Tree traversal using structural constraints."
               (declare (type (unsigned-byte 64) a b c d))
               (let ((bd (* b d)))
                 (declare (type (unsigned-byte 64) bd))
                 (when (<= bd bd-limit)
                   (let* ((num (+ (* 2 a d) 1))
                          (den (* 2 bd)))
                     ;; Condition: x = num/den < 1/100  =>  100 * num < den
                     (cond ((< (* threshold-den num) den)
                            (incf count)
                            ;; Proceed both directions in the structural branch
                            (dfs a b (+ a c) (+ b d))
                            (dfs (+ a c) (+ b d) c d))
                           ;; Pruning: Only explore left-ward if the left bound is still within threshold
                           ((< (* threshold-den a) b)
                            (dfs a b (+ a c) (+ b d)))))))))

      ;; 1. a=0 branch (Structural Singularity)
      ;; x = 1/(2d). Condition: 1/(2d) < 1/100 and 2d <= 10^8
      ;; This leads to: 50 < d <= 50,000,000
      (setf count (- (floor q-limit 2) 50))

      ;; 2. a > 0 branches (Alethetic Leap)
      ;; We iterate through the top-level segments of the Stern-Brocot tree (1/(d+1), 1/d)
      ;; that are relevant to the denominator constraint.
      (loop for d from 1 while (<= (* d (+ d 1)) bd-limit) do
            (dfs 1 (1+ d) 1 d))

      count)))

;; Execution within the Ffix0 convergence state.
(format t "Total Ambiguous Numbers (AC-optimized): ~A~%" (solve-ambiguous-numbers))
;▻ Total Ambiguous Numbers (AC-optimized): 52374425
;→ nil
