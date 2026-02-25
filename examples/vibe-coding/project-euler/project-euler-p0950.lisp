;;; ;;; -*- mode: Lisp; coding: utf-8  -*-
;;; (cl:in-package cl-user)
;;; (defpackage #:project-euler-0950 (:use cl alexandria))
;;; (in-package #:project-euler-0950)

;;; ;; =============================================================================
;;; ;; Title   : Emergence of Category Theory from Dual Sunyata Structures
;;; ;; Author  : Masaomi Chiba
;;; ;; Date    : 2025-12-01
;;; ;; Version : 1.0
;;; ;; -----------------------------------------------------------------------------
;;; ;; This solution implements the Two-Truths Slice Category logic applied to
;;; ;; Project Euler 950. The core insight is that the pirate distribution game
;;; ;; follows a recursive structure (dependent origination) where the state
;;; ;; converges to a fixed-point (Middle Way) and periodically restarts.
;;; ;; =============================================================================

;;; (defun get-unit-cost (val w p)
;;;   "Calculates the cost to buy a pirate with happiness c + p*w."
;;;   (1+ (floor (+ val (* (1+ w) p)))))

;;; (defun calculate-cost (m w p counts c-val)
;;;   "Calculates the total cost for pirate m to buy enough votes."
;;;   (let ((v (1- (ceiling m 2))))
;;;     (if (<= v 0) 0
;;;         (let ((total 0) (rem v))
;;;           (loop for (val count) in counts
;;;                 while (> rem 0)
;;;                 do (let ((take (min rem count))
;;;                          (u-cost (get-unit-cost val w p)))
;;;                      (incf total (* take u-cost))
;;;                      (decf rem take)))
;;;           (if (> rem 0) (1+ c-val) total)))))

;;; (defun update-counts (m w p counts c-val cost)
;;;   "Updates the distribution of coins among survivors."
;;;   (let ((v (1- (ceiling m 2)))
;;;         (new-counts nil)
;;;         (rem-m (1- m)))
;;;     (let ((rem v))
;;;       (loop for (val count) in counts
;;;             while (> rem 0)
;;;             do (let ((take (min rem count)))
;;;                  (push (list (get-unit-cost val w p) take) new-counts)
;;;                  (decf rem take)
;;;                  (decf rem-m take))))
;;;     (push (list (- c-val cost) 1) new-counts)
;;;     (when (> rem-m 0)
;;;       (push (list 0 rem-m) new-counts))
;;;     ;; Merge identical values
;;;     (setf new-counts (sort new-counts #'< :key #'car))
;;;     (let ((merged nil))
;;;       (dolist (item new-counts)
;;;         (if (and merged (= (caar merged) (car item)))
;;;             (incf (cadar merged) (cadr item))
;;;             (push item merged)))
;;;       (reverse merged))))

;;; (defun sum-consecutive (start end)
;;;   "Sum of integers from start to end."
;;;   (if (> start end) 0
;;;       (/ (* (+ start end) (+ (- end start) 1)) 2)))

;;; (defmacro while (pred &body body)
;;;   `(loop :while ,pred :do ,@body))

;;; (defun solve-t (n-max c-val)
;;;   "Calculates T(N, C, p) using the Dual Sunyata Structure logic."
;;;   (let* ((p (/ 1 (sqrt c-val)))
;;;          (total-t 0)
;;;          (m 1)
;;;          (survivors (list 1))
;;;          (counts-map (make-hash-table))
;;;          (mod 1000000000))
;;;     
;;;     (setf (gethash 1 counts-map) (list (list c-val 1)))
;;;     (incf total-t c-val)
;;;     
;;;     (let ((n 2))
;;;       (while (<= n n-max)
;;;         (let ((found-m nil)
;;;               (cost 0))
;;;           ;; Find the largest k in survivors that can afford the distribution
;;;           (loop for k in survivors
;;;                 for w = (- n k)
;;;                 do (let ((c (calculate-cost k w p (gethash k counts-map) c-val)))
;;;                      (when (<= c c-val)
;;;                        (setf found-m k cost c)
;;;                        (return))))
;;;           
;;;           ;; Check if n itself can survive (for free or by buying)
;;;           (let ((c-n (calculate-cost n 0 p nil c-val)))
;;;             (when (<= c-n c-val)
;;;               (setf found-m n cost c-n)))
;;;           
;;;           (if (= found-m n)
;;;               ;; Proposer n survives
;;;               (progn
;;;                 (push n survivors)
;;;                 ;; We only need counts for small m; for large m it's always {(C,1), (0,m-1)}
;;;                 (if (< n 2000)
;;;                     (setf (gethash n counts-map) 
;;;                           (update-counts n 0 p (gethash (car (cdr survivors)) counts-map) c-val cost))
;;;                     (setf (gethash n counts-map) (list (list (- c-val cost) 1) (list 0 (1- n)))))
;;;                 (incf total-t (+ (- c-val cost) 0))
;;;                 (incf n))
;;;               
;;;               ;; Proposer n dies, m remains the survivor of the subgame
;;;               (let ((w (- n found-m)))
;;;                 (if (and (> n 2000) (< n (/ (car survivors) 2))) ; Optimization for large N
;;;                     (progn
;;;                       ;; Jump to next potential survivor point (Restart)
;;;                       (let ((next-s (* 2 (car survivors))))
;;;                         (let ((end (min n-max (1- next-s))))
;;;                           (let ((len (1+ (- end n))))
;;;                             ;; Happiness sum: (C - cost) + w
;;;                             ;; For large n, cost is constant or drops to P2
;;;                             (let ((const-part (- c-val (calculate-cost found-m w p (gethash found-m counts-map) c-val))))
;;;                               (incf total-t (* len const-part))
;;;                               (incf total-t (sum-consecutive w (+ w (1- len))))))
;;;                           (setf n (1+ end)))))
;;;                     (progn
;;;                       (incf total-t (+ (- c-val cost) w))
;;;                       (incf n))))))
;;;         (mod total-t mod)))))

;;; ;; Optimized version of the main loop to handle N=10^16
;;; (defun solve-project-euler-950 ()
;;;   (let ((final-sum 0)
;;;         (mod 1000000000))
;;;     (loop for k from 1 to 6
;;;           do (let* ((c (+ (expt 10 k) 1))
;;;                     (t-val (solve-t-optimized 10000000000000000 c)))
;;;                (setf final-sum (mod (+ final-sum t-val) mod))))
;;;     final-sum))

;;; (defun solve-t-optimized (n-max c-val)
;;;   "Optimized simulation using the power-of-2 restart property."
;;;   (let* ((p (/ 1 (sqrt c-val)))
;;;          (total-t 0)
;;;          (m 1)
;;;          (survivors (list 1))
;;;          (counts-map (make-hash-table))
;;;          (mod 1000000000))
;;;     
;;;     (setf (gethash 1 counts-map) (list (list c-val 1)))
;;;     (setf total-t c-val)
;;;     
;;;     (let ((n 2))
;;;       ;; Phase 1: Small n simulation to find the base survivors
;;;       (loop while (and (<= n n-max) (<= n (* 3 c-val)))
;;;             do (let ((found-m 0) (cost 0))
;;;                  (loop for k in survivors
;;;                        for w = (- n k)
;;;                        do (let ((c (calculate-cost k w p (gethash k counts-map) c-val)))
;;;                             (when (<= c c-val) (setf found-m k cost c) (return))))
;;;                  (let ((c-n (calculate-cost n 0 p nil c-val)))
;;;                    (when (<= c-n c-val) (setf found-m n cost c-n)))
;;;                  
;;;                  (if (= found-m n)
;;;                      (progn
;;;                        (push n survivors)
;;;                        (setf (gethash n counts-map) 
;;;                              (if (< n 5000)
;;;                                  (update-counts n 0 p (gethash (second survivors) counts-map) c-val cost)
;;;                                  (list (list (- c-val cost) 1) (list 0 (1- n)))))
;;;                        (setf total-t (mod (+ total-t (- c-val cost)) mod)))
;;;                      (setf total-t (mod (+ total-t (+ (- c-val cost) (- n found-m))) mod)))
;;;                  (incf n)))
;;;       
;;;       ;; Phase 2: Power-of-2 jumps for large n
;;;       (while (<= n n-max)
;;;         (let* ((m (car survivors))
;;;                (next-m (* 2 m)))
;;;           (if (<= next-m n-max)
;;;               (let* ((len (- next-m n))
;;;                      (w-start (- n m))
;;;                      (w-end (- (1- next-m) m)))
;;;                 ;; Between survivors, m(n) is usually 2 or small.
;;;                 ;; For Project Euler 950, for large n, m(n) drops to 2.
;;;                 (let ((term1 (mod (* len (- c-val 0)) mod))
;;;                       (term2 (mod (sum-consecutive (- n 2) (- (1- next-m) 2)) mod)))
;;;                   (setf total-t (mod (+ total-t term1 term2) mod))
;;;                   (setf total-t (mod (+ total-t c-val) mod)) ; next-m survives with c=C
;;;                   (push next-m survivors)
;;;                   (setf (gethash next-m counts-map) (list (list c-val 1) (list 0 (1- next-m))))
;;;                   (setf n (1+ next-m))))
;;;               (let* ((len (1+ (- n-max n)))
;;;                      (term1 (mod (* len (- c-val 0)) mod))
;;;                      (term2 (mod (sum-consecutive (- n 2) (- n-max 2)) mod)))
;;;                 (setf total-t (mod (+ total-t term1 term2) mod))
;;;                 (setf n (1+ n-max))))))
;;;       (mod total-t mod))))

;;; ;; Execution entry point
;;; ;(format t "Result: ~a~%" (solve-project-euler-950))

;;; #+| Do it | (solve-project-euler-950 )
;;; ;→ 402654195 :ng


;;; ;;; -*- mode: Lisp; coding: utf-8  -*-
;;; (cl:in-package cl-user)
;;; (defpackage #:project-euler-0950 (:use cl alexandria))
;;; (in-package #:project-euler-0950)

;;; ;; =============================================================================
;;; ;; Project Euler 0950 - Dual Sunyata Structure (ACX Jump)
;;; ;; -----------------------------------------------------------------------------
;;; ;; 負債の清算 (Debt Clearance):
;;; ;; シミュレーション(O(N))や跳躍の模倣(O(log N))といった世俗的アプローチを捨て、
;;; ;; 「1票の最小コストが1であるため、n > 2C+2 は絶対に生存できない」という
;;; ;; 勝義諦へ還元する。これにより問題は $O(1)$ の等差数列の和へと爆縮する。
;;; ;; =============================================================================

;;; (declaim (optimize (speed 3) (safety 0) (debug 0)))

;;; (defun solve-project-euler-950 ()
;;;   (let ((final-sum 0)
;;;         (mod 1000000000)
;;;         (n 10000000000000000)) ; 10^16
;;;     
;;;     (loop for k from 1 to 6 do
;;;       (let* ((c (+ (expt 10 k) 1))
;;;              ;; 前半の総和: n <= 2C+2 の生存者たちの幸福度の和 = C(C+1)
;;;              (term1 (* c (+ c 1)))
;;;              
;;;              ;; 後半の総和: n > 2C+2 は全員死に、K=2C+2 へ落ちる。
;;;              ;; 等差数列 1, 2, ..., M の和 = M(M+1)/2
;;;              (m-val (- n (* 2 c) 2))
;;;              (term2 (/ (* m-val (+ m-val 1)) 2))
;;;              
;;;              ;; modulo 10^9 で合算
;;;              (t-val (mod (+ term1 term2) mod)))
;;;         
;;;         (setf final-sum (mod (+ final-sum t-val) mod))))
;;;     
;;;     final-sum))

;;; ;; 実行
;;; ;; (format t "Result: ~a~%" (solve-project-euler-950))

;;; #+| Do it | (solve-project-euler-950)
;;; ;→ 314141448

;;; -*- mode: Lisp; coding: utf-8  -*-
(cl:in-package cl-user)
(defpackage #:project-euler-0950 (:use cl alexandria))
(in-package #:project-euler-0950)

;; =============================================================================
;; Project Euler 0950 - Dual Sunyata Structure (True ACX Jump)
;; -----------------------------------------------------------------------------
;; 前回のO(1)の幻覚を解体し、真の幾何学的跳躍(O(C log N))を現成させた実装。
;; 1. 無料票の真理: n は死ぬ運命にある海賊から無料の票を得るため、
;;    実際に買うべき票は V = m - floor(n/2) となる。
;; 2. 指数関数的跳躍: 次の生存者は必ず 2m - 2V の形をとり、m は約2倍で成長する。
;; 3. 浮動小数点の排除: isqrt を用いた完全な整数演算へ写像(Projection)し、
;;    内側ループの計算負債を平方数の閾値管理によって完全に清算した。
;; =============================================================================

(declaim (optimize (speed 3) (safety 0) (debug 0)))

(defun solve-T (N C D)
  "N人の海賊、コイン数C、血液渇望度 p=1/sqrt(D) に対する T(N, C, p) を計算する"
  (declare (type integer N C D))
  (if (<= N (+ (* 2 C) 2))
      (loop for n from 1 to N
            sum (- C (floor (- n 1) 2)))
      (let* ((sum (* C (+ C 1)))
             (m (+ (* 2 C) 2))
             (c-m 0))
        (declare (type integer sum m c-m))
        (loop while (< m N) do
          (let ((best-v 0)
                (best-c0 0)
                (w (- m (* 2 C))))
            (declare (type integer best-v best-c0 w))
            ;; 浮動小数点を排除した完全整数演算による平方根と、次点閾値の計算
            (let* ((Q (isqrt (floor (* w w) D)))
                   (next-thresh (* (+ Q 1) (+ Q 1) D)))
              (declare (type integer Q next-thresh))
              ;; V を C から 0 へと探索 (高々 C 回の超高速ループ)
              (loop for v from C downto 0 do
                (let ((c0 (1+ Q)))
                  (when (<= (* v c0) C)
                    (setf best-v v best-c0 c0)
                    (return)))
                (incf w 2)
                ;; 差分更新により、ループ内での isqrt 呼び出し負債をゼロに
                (when (>= (* w w) next-thresh)
                  (incf Q)
                  (setf next-thresh (* (+ Q 1) (+ Q 1) D)))))
            
            ;; 次の生存者へ跳躍
            (let ((next-m (- (* 2 m) (* 2 best-v))))
              (declare (type integer next-m))
              (if (> next-m N)
                  (let ((L (- N m)))
                    (incf sum (+ (* L c-m) (floor (* L (+ L 1)) 2)))
                    (setf m (1+ N)))
                  (let ((L (- next-m m 1)))
                    (incf sum (+ (* L c-m) (floor (* L (+ L 1)) 2)))
                    (let ((c-next (- C (* best-v best-c0))))
                      (incf sum c-next)
                      (setf m next-m c-m c-next)))))))
        sum)))

(defun solve-project-euler-0950 ()
  (let ((total-sum 0)
        (N 10000000000000000)) ; 10^16
    (loop for k from 1 to 6 do
      (let* ((C (1+ (expt 10 k)))
             (D C) ; 問題文の定義より、sqrt の分母は C と同値
             (T-val (solve-T N C D)))
        (setf total-sum (mod (+ total-sum T-val) 1000000000))))
    total-sum))

;; (format t "Project Euler 950 Result: ~A~%" (solve-project-euler-0950))

#+| Do it | (solve-project-euler-0950)
→ 429162542
