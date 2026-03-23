;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3-flash-preview
(cl:in-package cl-user)
(defpackage #:project-euler-0220 (:use cl iterate alexandria) (:export #:solve))
(in-package #:project-euler-0220)

#||
(def-logic-principle correctness-principle
  :domain :computational-geometry
  :operator :recursive-decomposition
  :justification "Previous implementation suffered from premature termination. It failed to differentiate between consuming a full recursive block (time-axis progression) and applying a zero-step rotation (state-axis update). By strictly isolating the 'remaining-steps' countdown from zero-cost operations, the true positional displacement over exactly 10^12 steps can be restored.")
||#


(defun rotate-state (current-state turns)
  "current-stateを指定された回数（90度単位、右回り）回転させた変位を返す"
  (let ((pos-x (first current-state))
        (pos-y (second current-state))
        (direction (third current-state))
        (rotation-count (mod turns 4)))
    (iterate (repeat rotation-count)
      (let ((old-x pos-x))
        (setf pos-x pos-y
              pos-y (- old-x))))
    (list pos-x pos-y (mod (+ direction rotation-count) 4))))

(defun combine-states (state-1 state-2)
  "state-1の後に、state-1の最終方位に合わせて回転させたstate-2を合成する"
  (let* ((rotated-2 (rotate-state state-2 (third state-1))))
    (list (+ (first state-1) (first rotated-2))
          (+ (second state-1) (second rotated-2))
          (third rotated-2))))

(defparameter *memo-a* (make-hash-table))
(defparameter *memo-b* (make-hash-table))
(defparameter *memo-steps* (make-hash-table))

(defun precompute (max-level)
  "各レベルにおけるa, bの変位と総歩数Fを事前計算する"
  (setf (gethash 0 *memo-a*) '(0 0 0)
        (gethash 0 *memo-b*) '(0 0 0)
        (gethash 0 *memo-steps*) 0)
  (iterate (for current-level from 1 to max-level)
    (let ((prev-steps (gethash (1- current-level) *memo-steps*))
          (f-step '(0 1 0)))
      ;; a -> a R b F R
      (let* ((s (gethash (1- current-level) *memo-a*))          ; a
             (s (combine-states s '(0 0 1)))                    ; R
             (s (combine-states s (gethash (1- current-level) *memo-b*))) ; b
             (s (combine-states s f-step))                      ; F
             (s (combine-states s '(0 0 1))))                   ; R
        (setf (gethash current-level *memo-a*) s))
      ;; b -> L F a L b
      (let* ((s '(0 0 3))                                       ; L
             (s (combine-states s f-step))                      ; F
             (s (combine-states s (gethash (1- current-level) *memo-a*))) ; a
             (s (combine-states s '(0 0 3)))                    ; L
             (s (combine-states s (gethash (1- current-level) *memo-b*)))) ; b
        (setf (gethash current-level *memo-b*) s))
      (setf (gethash current-level *memo-steps*) (1+ (* 2 prev-steps))))))

(defun solve-recursive (target-sym current-level remaining-steps current-state)
  "再帰的にステップを消費しながら正確な座標を特定する"
  (let ((rules (if (eq target-sym 'a)
                   '((a . -1) (r . 0) (b . -1) (f . 0) (r . 0))
                   '((l . 0) (f . 0) (a . -1) (l . 0) (b . -1)))))
    (iterate (for (sub-sym . level-diff) in rules)
      (let* ((actual-level (+ current-level level-diff))
             (steps (cond ((eq sub-sym 'f) 1)
                          ((member sub-sym '(a b)) (gethash actual-level *memo-steps*))
                          (t 0))))
        (cond
          ;; ケース1: 歩数を消費しない記号 (R, L, または レベル0の a/b)
          ((zerop steps)
           (setf current-state
                 (cond ((eq sub-sym 'r) (combine-states current-state '(0 0 1)))
                       ((eq sub-sym 'l) (combine-states current-state '(0 0 3)))
                       (t current-state))))
          
          ;; ケース2: シンボルの全歩数を完全に消費できる場合 (ジャンプ)
          ((>= remaining-steps steps)
           (setf current-state
                 (combine-states current-state 
                                 (cond ((eq sub-sym 'a) (gethash actual-level *memo-a*))
                                       ((eq sub-sym 'b) (gethash actual-level *memo-b*))
                                       ((eq sub-sym 'f) '(0 1 0)))))
           (decf remaining-steps steps)
           (when (zerop remaining-steps)
             (return-from solve-recursive current-state)))
          
          ;; ケース3: シンボルの途中で目標歩数に到達する場合 (内部へ潜る)
          (t
           (return-from solve-recursive 
             (solve-recursive sub-sym actual-level remaining-steps current-state))))))
    current-state))

(defun solve ()
  (let* ((max-level 50)
         (target-steps #.(expt 10 12)))
    (format t "Precomputing up to level ~A...~%" max-level)
    (precompute max-level)
    (format t "Tracing ~A steps...~%" target-steps)
    (let* ((start-state '(0 0 0))
           (after-f (combine-states start-state '(0 1 0)))
           ;; D_n は "Fa" から始まるため、最初のFを処理した状態から 'a' の展開を辿る
           (final-state (if (= target-steps 1)
                            after-f
                            (solve-recursive 'a max-level (1- target-steps) after-f))))
      (format t "Output Coordinate: ~D,~D~%" (first final-state) (second final-state))
      (format nil "~D,~D" (first final-state) (second final-state)))))


#+| Do it | (solve )
#|------------------------------------------------------------|
Timing the evaluation of (solve)
Precomputing up to level 50...
Tracing 1000000000000 steps...
Output Coordinate: 139776,963904

User time    =        0.000
System time  =        0.000
Elapsed time =        0.000
Allocation   = 43752 bytes
0 Page faults
GC time      =        0.000
 |------------------------------------------------------------|#
;;→ "139776,963904"
:ok