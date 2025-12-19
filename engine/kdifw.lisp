;;;; ============================================================
;;;; ku-lisp: Two-Truths DIFW / EMT Reference Interpreter
;;;; Ffix0 (mweq) Edition
;;;; ============================================================

(defpackage :ku-lisp
  (:use :cl)
  (:export
   :make-history
   :ku-eval
   :emt-test
   :calculate-fuss))

(in-package :ku-lisp)

;;; --- 中道等価(mweq)のための定数 ---
(defparameter *mweq-epsilon* 0.05 "中道等価とみなす構造的差異の許容範囲")

;;;; ------------------------------------------------------------
;;;; 1. History Structures (Inductive Truth + Middle-Way)
;;;; ------------------------------------------------------------

(defstruct history-entry
  result
  timestamp
  confidence
  context
  structural-signature) ; Ffix0クラスを識別するためのシグネチャ

(defstruct history
  (truth-heap (make-hash-table :test 'equal)) ; 世俗的真理の蓄積
  (nmf-heap   (make-hash-table :test 'equal)) ; 非中道の誤謬の記録
  (decay-rate 0.01))

;;;; ------------------------------------------------------------
;;;; 2. Middle-Way Equivalence (mweq) Logic
;;;; ------------------------------------------------------------

(defun mweq-p (a b history)
  "二つの表現が中道等価(mweq)であるか判定する。
   単なる equal ではなく、Ffix0の近傍(Fussの差が小さい)にあるかをみる。"
  (let ((dist (expression-distance a b)))
    (<= dist *mweq-epsilon*)))

;;;; ------------------------------------------------------------
;;;; 3. Advanced Fuss Calculation (Two Truths Version)
;;;; ------------------------------------------------------------

(defun calculate-fuss (expression history)
  "世俗的整合性と勝義的非実体化の両面からFussを計算する"
  (let ((shiki-fuss (calculate-shiki-distance expression history))
        (ku-fuss (calculate-reification-penalty expression)))
    (+ shiki-fuss ku-fuss)))

(defun calculate-shiki-distance (expression history)
  "過去の真理(Ffix0点)群との最小『中道距離』を求める"
  (let ((min-distance 100.0))
    (maphash
     (lambda (k v)
       (declare (ignore v))
       (setf min-distance (min min-distance (expression-distance expression k))))
     (history-truth-heap history))
    min-distance))

(defun calculate-reification-penalty (expr)
  "勝義的ペナルティ：接地不可能な実体化(意志、絶対的な自己等)が含まれる場合にFussを増大させる"
  ;; 論文5.1節の実装。特定のシンボルや構造をNMF予備軍として検知
  (if (has-reification-risk-p expr) 2.0 0.0))

(defun has-reification-risk-p (expr)
  "再実体化(Reification)の傾向があるかチェック"
  (let ((taboo '(will consciousness absolute-self soul)))
    (some (lambda (x) (search-symbol x expr)) taboo)))

(defun search-symbol (target expr)
  (cond ((eq target expr) t)
        ((listp expr) (some (lambda (e) (search-symbol target e)) expr))
        (t nil)))

;;;; ------------------------------------------------------------
;;;; 4. 改良版 EMT (Emergent Middle Test)
;;;; ------------------------------------------------------------

(defun emt-test (expression result history
                  &key (fuss-threshold 1.0)
                       (confidence-threshold 0.5))
  "二諦のバランスをテストする。
   1. 世俗的収束: Fussが閾値以下か (Conventional)
   2. 勝義的安定: 中道等価なクラスへの帰属が確認できるか (Ultimate)"
  (let* ((fuss (calculate-fuss expression history))
         (is-coherent (<= fuss fuss-threshold))
         (is-non-reified (zerop (calculate-reification-penalty expression))))
    
    ;; 論文の『漸近的中道固定』を反映: 
    ;; 単なる一致ではなく、非実体化された安定状態(mweq)をパス条件とする
    (and is-coherent is-non-reified)))

;;;; ------------------------------------------------------------
;;;; 5. DIFW Evaluator (ku-eval: Two Truths Implementation)
;;;; ------------------------------------------------------------

(defun ku-eval (expression
                &key
                  (history (make-history))
                  (fuss-threshold 1.0)
                  (confidence-threshold 0.5)
                  (context :default))
  (format t "~&--- Two-Truths DIFW (Ffix0) Trace ---~%")
  
  ;; A. NMF遮断 (勝義的ガードレール)
  (when (gethash expression (history-nmf-heap history))
    (format t "[Blocked] Reified NMF region detected.~%")
    (return-from ku-eval nil))

  ;; B. 中道等価による想起 (mweq Recall)
  ;; 厳密な一致でなくても、mweqの範囲内なら『知足』として再利用する
  (maphash
   (lambda (k entry)
     (when (mweq-p expression k history)
       (let ((conf (decay-confidence entry history)))
         (when (> conf confidence-threshold)
           (format t "[mweq Recall] Using stable Ffix0 neighbor (conf=~F).~%" conf)
           (return-from ku-eval (history-entry-result entry))))))
   (history-truth-heap history))

  ;; C. 色化計算 (Deductive Colorization)
  (format t "[Compute] Generating emergent color (Shiki)...~%")
  (let ((result (ignore-errors (eval expression))))
    (unless result
      (format t "[Error] Calculation collapsed into Fuss.~%")
      (return-from ku-eval nil))

    ;; D. 二諦版 EMT 検証
    (if (emt-test expression result history
                  :fuss-threshold fuss-threshold
                  :confidence-threshold confidence-threshold)
        (progn
          (format t "[Ffix0] Asymptotic fixation established (mweq 0).~%")
          (store-truth expression result history context)
          result)
        (progn
          (format t "[NMF] Failed: Excessive reification or incoherence.~%")
          (store-nmf expression history)
          nil))))

;;;; ------------------------------------------------------------
;;;; ヘルパー関数 (維持)
;;;; ------------------------------------------------------------

(defun expression-distance (a b)
  (cond ((equal a b) 0.0)
        ((and (consp a) (consp b))
         (+ 0.5 ; 重みを調整
            (expression-distance (car a) (car b))
            (expression-distance (cdr a) (cdr b))))
        (t 1.0)))

(defun store-truth (expression result history context)
  (setf (gethash expression (history-truth-heap history))
        (make-history-entry
         :result result
         :timestamp (get-universal-time)
         :confidence 1.0
         :context context)))

(defun store-nmf (expression history)
  (setf (gethash expression (history-nmf-heap history)) (get-universal-time)))

(defun decay-confidence (entry history)
  (let* ((dt (- (get-universal-time) (history-entry-timestamp entry)))
         (decay (* dt (history-decay-rate history))))
    (max 0.0 (- (history-entry-confidence entry) decay))))
