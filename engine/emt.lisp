;;; -*- mode: Lisp; coding: utf-8  -*-

;;; チャットに利用するEMTの概念の定義をCommon Lispコードとして与えます。扱うデータはku-lispというlispインタプリタの形式をしています。

;;; ---------------------------------------------------------------------------
;;; 改良版 EMT (Emergent Middle Test) - Two Truths Verification System
;;; ---------------------------------------------------------------------------

(defstruct (emt-process (:conc-name emt-))
  difw-proc            ; 内部で実行されるDifwプロセス (世俗的推論)
  mcc-threshold        ; 世俗的知足閾値 (Conventional Truth Threshold)
  fuss-change-rate     ; Fussの変化率
  ultimate-stability-p ; 勝義的安定フラグ (非実体化ペナルティが0であるか)
  emt-passed-p)

;;; 1. 世俗的収束の検証 (Conventional Truth Verification)
(defun check-conventional-convergence (process current-fuss)
  "Fussの変化率に基づき、世俗的な安定（色の世界の整合性）を判定する"
  (let* ((history (dp-history (emt-difw-proc process)))
         (prev-fuss (if (and history (>= (length history) 1)) 
                        (calculate-fuss (car history) (dp-constraints (emt-difw-proc process)))
                        current-fuss)))
    (if (= prev-fuss 0)
        0.0
        (/ (abs (- current-fuss prev-fuss)) prev-fuss))))

;;; 2. 勝義的非実体化の検証 (Ultimate Truth Verification)
(defun check-ultimate-non-reification (expr)
  "論文セクション5.1に基づき、実体化による歪み(NMF)が含まれていないかを検証する"
  ;; calculate-reification-penalty が 0 である、つまり非実体的な中道が保たれているか
  (zerop (calculate-reification-penalty expr)))

;;; 3. 二諦版 EMT メインステップ
(defun emt-test-step (emt-proc)
  "世俗的整合性と勝義的非実体化の両面から中道を検定する"
  (unless (emt-emt-passed-p emt-proc)
    (let* ((difw-proc (k-step-difw (emt-difw-proc emt-proc)))
           (current-expr (dp-expr difw-proc))
           (current-fuss (dp-current-fuss difw-proc)))
      
      ;; A. 世俗的指標の更新: Fuss変化率
      (let ((change-rate (check-conventional-convergence emt-proc current-fuss)))
        (setf (emt-fuss-change-rate emt-proc) change-rate)
        
        ;; B. 勝義的指標の更新: 非実体化の確認
        (setf (emt-ultimate-stability-p emt-proc) (check-ultimate-non-reification current-expr))
        
        ;; C. 二諦の統合判定 (Middle-Way Test)
        ;; 1. 変化率が *mweq-epsilon* 以下 (漸近的固定) 
        ;; 2. Fussが世俗的閾値(MCC)以下 (構造的整合性) [cite: 14]
        ;; 3. 勝義的安定がパスされている (非実体化の維持) 
        (when (and (< change-rate 0.01)                               ; 漸近的安定
                   (<= current-fuss (emt-mcc-threshold emt-proc))     ; 世俗的合格
                   (emt-ultimate-stability-p emt-proc))               ; 勝義的合格
          
          ;; EMT 成功: Dfix0 における二諦の不二が確認された
          (setf (emt-emt-passed-p emt-proc) t)
          (format t "EMT Passed: Dfix0 established with mweq coherence.~%")))))
  
  emt-proc)

;;; 4. 実行インターフェース
(defun emt-run (initial-expr constraints &key (mcc-threshold 0.1) (max-steps 100))
  (let* ((initial-fuss (calculate-fuss initial-expr constraints))
         (initial-proc (make-difw-process :expr initial-expr 
                                          :current-fuss initial-fuss
                                          :constraints constraints
                                          :status :active
                                          :step-count 0))
         (emt-proc (make-emt-process :difw-proc initial-proc 
                                     :mcc-threshold mcc-threshold
                                     :ultimate-stability-p nil)))
    (loop :for i :from 1 :to max-steps
          :until (emt-emt-passed-p emt-proc)
          :do (setf emt-proc (emt-test-step emt-proc))
          :finally (return emt-proc))))
