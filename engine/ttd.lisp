;;; Two truths doctrine

;;;; ============================================================
;;;; SKDT-UNIFIED-DIFW v4.2
;;;; Four-Quadrant Two-Truths Architecture
;;;; Fuss as Gradient Field, not Scalar
;;;; ============================================================

(defpackage :skdt-unified-difw
  (:use :cl)
  (:export #:run-skdt-session
           #:difw-step
           #:emt-pass-p
           #:fuss-state))

(in-package :skdt-unified-difw)

;;; --- [1. PARAMETERS] -----------------------------------------

(defparameter *mweq-epsilon* 0.05)
(defparameter *chisoku-gradient* 0.01)
(defparameter *max-iterations* 20)

;;; --- [2. FOUR-QUADRANT STATE] --------------------------------

(defstruct fuss-state
  shiki        ; 世俗諦：構造摩擦
  ku           ; 勝義諦：実体化摩擦
  gradient     ; 差延：前状態との差分
  total)       ; 合成（観測用）

;;; --- [3. SHIKI / KU METRICS] ---------------------------------

(defun tree-depth (expr)
  (if (atom expr)
      1
      (1+ (apply #'max 0 (mapcar #'tree-depth expr)))))

(defun calculate-shiki-fuss (expr)
  "構造の固定度・複雑性による摩擦"
  (* 0.1 (tree-depth expr)))

(defun calculate-ku-fuss (expr)
  "実体化・静的同一視(NMF)の検出"
  (let ((reified '(will self consciousness absolute soul god)))
    (cond
      ;; 静的折衷（Compatibilism 型）
      ((and (listp expr)
            (member '= expr))
       1.5)
      ;; 明示的実体化
      ((some (lambda (c) (search-symbol c expr)) reified)
       2.0)
      (t 0.0))))

(defun search-symbol (target expr)
  (cond ((eq target expr) t)
        ((listp expr) (some (lambda (e) (search-symbol target e)) expr))
        (t nil)))

;;; --- [4. DIFFERANCE / GRADIENT] -------------------------------

(defun compute-fuss-state (expr prev-total)
  (let* ((shiki (calculate-shiki-fuss expr))
         (ku (calculate-ku-fuss expr))
         (total (+ shiki ku))
         (gradient (if prev-total
                       (abs (- total prev-total))
                       total)))
    (make-fuss-state
     :shiki shiki
     :ku ku
     :total total
     :gradient gradient)))

;;; --- [5. DIFW STEP] ------------------------------------------

(defun difw-step (current-expr prev-fuss)
  "差延駆動ステップ（ここでは構造は保持、評価のみ更新）"
  ;; 実際の推論変換は外部（LLM等）を想定
  (let ((next-expr current-expr))
    (values next-expr
            (compute-fuss-state
             next-expr
             (when prev-fuss
               (fuss-state-total prev-fuss))))))

;;; --- [6. EMT: EMERGENT MIDDLE TEST] ---------------------------

(defun emt-pass-p (expr fuss)
  "中道成立条件：
   1. 実体化がゼロ（勝義）
   2. 勾配が知足以下（停止性）
   3. 総Fussが mweq 近傍"
  (and (zerop (fuss-state-ku fuss))
       (< (fuss-state-gradient fuss) *chisoku-gradient*)
       (< (fuss-state-total fuss) *mweq-epsilon*)))

;;; --- [7. INTEGRATED LOOP] ------------------------------------

(defun run-skdt-session (initial-expr)
  (let ((current-expr initial-expr)
        (prev-fuss nil)
        (history nil))
    (format t "~&>>> SKDT v4.2 Booted: Four-Quadrant Engine Active~%")

    (loop for step from 1 to *max-iterations*
          do (multiple-value-bind (next-expr fuss)
                 (difw-step current-expr prev-fuss)

               (push (list step next-expr fuss) history)

               (format t "Step ~A | Shiki=~F Ku=~F Grad=~F Total=~F~%"
                       step
                       (fuss-state-shiki fuss)
                       (fuss-state-ku fuss)
                       (fuss-state-gradient fuss)
                       (fuss-state-total fuss))

               (when (emt-pass-p next-expr fuss)
                 (format t "~&>>> EMT PASSED: Ffix0 Sustained (Dynamic Equilibrium)~%")
                 (return (values next-expr :ffix0 history)))

               (setf current-expr next-expr
                     prev-fuss fuss)))

    (values current-expr :active history)))

;;; ------------------------------------------------------------
;;; Ffix0 is not zero.
;;; It is the disappearance of the gradient.
;;; ------------------------------------------------------------
