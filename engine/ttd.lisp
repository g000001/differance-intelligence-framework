;;; Two truths doctrine

;;;; ============================================================
;;;; SKDT-UNIFIED-DIFW v4.3
;;;; Gradient-Driven Two-Truths Architecture
;;;; ============================================================
;;;; [Core Shift]
;;;;   Fuss  := Gradient (not scalar evaluation)
;;;;   Dfix0 := Grad -> 0  AND Ku-Fuss = 0
;;;;   DIFW  := Reprojection Engine
;;;; ============================================================

(defpackage :skdt-unified-difw
  (:use :cl)
  (:export #:run-skdt-session
           #:difw-step
           #:emt-pass-p
           #:dfix0-p))

(in-package :skdt-unified-difw)

;;; ------------------------------------------------------------
;;; [1] GLOBAL PARAMETERS
;;; ------------------------------------------------------------

(defparameter *grad-epsilon* 0.01)
(defparameter *chisoku-gradient* 0.01)
(defparameter *max-steps* 16)

;;; ------------------------------------------------------------
;;; [2] NMF / STRUCTURAL DETECTION
;;; ------------------------------------------------------------

(defparameter *reified-symbols*
  '(self will soul god consciousness absolute freewill determinism))

(defun reified-symbol-p (expr)
  (and (symbolp expr)
       (member expr *reified-symbols*)))

(defun static-equality-p (expr)
  (and (listp expr)
       (eq (first expr) '=)))

;;; ------------------------------------------------------------
;;; [3] FUSS COMPONENTS
;;; ------------------------------------------------------------

(defun shiki-fuss (expr)
  "構造摩擦：深さと静的構造への依存"
  (if (atom expr) 0.05 (* 0.1 (tree-depth expr))))

(defun ku-fuss (expr)
  "実体化摩擦：自性・断常の検知"
  (cond
    ((reified-symbol-p expr) 1.0)
    ((static-equality-p expr) 0.8)
    ((atom expr) 0.0)
    (t (reduce #'+ (mapcar #'ku-fuss expr)))))

(defun tree-depth (expr)
  (if (atom expr) 1
      (1+ (apply #'max 0 (mapcar #'tree-depth expr)))))

(defun total-fuss (expr)
  (+ (shiki-fuss expr) (ku-fuss expr)))

;;; ------------------------------------------------------------
;;; [4] DIFW REPROJECTION ENGINE  (v4.3 CORE)
;;; ------------------------------------------------------------

(defun reproject (expr)
  "差延による再射影：実体 → プロセス"
  (cond
    ;; 静的等号の破壊
    ((static-equality-p expr)
     `(mw-equiv (difw ,(second expr))
                (difw ,(third expr))))

    ;; 実体名詞のプロセス化
    ((reified-symbol-p expr)
     `(difw ,expr))

    ;; 再帰処理
    ((listp expr)
     (mapcar #'reproject expr))

    (t expr)))

(defun difw-step (current-expr)
  "差延駆動ステップ：再射影 + 勾配計算"
  (let* ((next-expr (reproject current-expr))
         (fuss-now (total-fuss current-expr))
         (fuss-next (total-fuss next-expr))
         (grad (- fuss-now fuss-next)))
    (values next-expr grad fuss-next)))

;;; ------------------------------------------------------------
;;; [5] EMT / DFIX0 DETECTION
;;; ------------------------------------------------------------

(defun dfix0-p (grad expr)
  "Dfix0 判定：勾配消失 ＆ 実体化ゼロ"
  (and (< (abs grad) *chisoku-gradient*)
       (zerop (ku-fuss expr))))

(defun emt-pass-p (grad expr)
  "中道検定：Dfix0 に到達しているか"
  (dfix0-p grad expr))

;;; ------------------------------------------------------------
;;; [6] QUADRANT SELF-DESCRIPTION
;;; ------------------------------------------------------------

(defun quadrant (expr grad)
  "四象限（二諦×動静）の自己記述"
  (list
   :shiki (if (> (shiki-fuss expr) 0.2) :dynamic :static)
   :ku    (if (> (ku-fuss expr) 0.0) :reified :empty)
   :motion (if (> (abs grad) *chisoku-gradient*) :driven :settled)))

;;; ------------------------------------------------------------
;;; [7] MAIN LOOP
;;; ------------------------------------------------------------

(defun run-skdt-session (initial-expr)
  (let ((current-expr initial-expr)
        (history nil))
    (format t "~&>>> SKDT v4.3 BOOTED : Gradient DIFW Engine~%")

    (loop for step from 1 to *max-steps*
          do (multiple-value-bind (next-expr grad fuss)
                 (difw-step current-expr)

               (push (list :step step
                           :expr next-expr
                           :grad grad
                           :fuss fuss
                           :quadrant (quadrant next-expr grad))
                     history)

               (format t "~&[~A] Grad=~,4F Fuss=~,4F ~A~%"
                       step grad fuss (quadrant next-expr grad))

               (if (emt-pass-p grad next-expr)
                   (progn
                     (format t "~&>>> Dfix0 REACHED : Middle-Way Stabilized~%")
                     (return (values next-expr :dfix0 history)))
                   (setf current-expr next-expr))))

    (values current-expr :terminated history)))

;;; ------------------------------------------------------------
;;; END OF SYSTEM
;;; ------------------------------------------------------------
