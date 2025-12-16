;;; -*- mode: Lisp; coding: utf-8  -*-

;;; チャットに利用するEMTの概念の定義をCommon Lispコードとして与えます。扱うデータはku-lispというlispインタプリタの形式をしています。

(defstruct emt-process
  difw-proc           ; 内部で実行されるDifwプロセス
  mcc-threshold       ; メタ知足制約の閾値 (例: 0.05)
  fuss-change-rate    ; 直近のFussの変化率 |Fuss_t - Fuss_{t-1}| / Fuss_{t-1}
  emt-passed-p)


;; EMT検証成功フラグ
(defun check-fuss-convergence (process current-fuss)
  (let* ((history (difw-process-history (emt-process-difw-proc process)))
         (prev-fuss (if (and history (>= (length history) 2)) 
                        (calculate-fuss (cadr history) (difw-process-constraints (emt-process-difw-proc process)))
                        current-fuss)))
    (if (= prev-fuss 0)
        0.0  ; ゼロ除算回避
        (/ (abs (- current-fuss prev-fuss)) prev-fuss))))


(defun is-converged (change-rate)
  (< change-rate 0.01))


;; 1%以下の変化率を収束と見なす
(defun check-mcc (process current-fuss)
  (let ((threshold (emt-process-mcc-threshold process)))
    ;; 構造的意味: Current-Fuss が MCC 閾値よりも低い、または等しい
    (<= current-fuss threshold)))


(defun emt-test-step (emt-process)
  (unless (emt-process-emt-passed-p emt-process)
    ;; 1. Difwプロセスを1ステップ進める
    (let* ((difw-proc (k-step-difw (emt-process-difw-proc emt-process)))
           (current-fuss (difw-process-current-fuss difw-proc)))
      
      ;; 2. Fussの変化率を計算し、更新
      (let ((change-rate (check-fuss-convergence emt-process current-fuss)))
        (setf (emt-process-fuss-change-rate emt-process) change-rate)
        
        ;; 3. 収束判定 (FPA機能)
        (when (is-converged change-rate)
          ;; 4. 収束後、MCCを検証
          (when (check-mcc emt-process current-fuss)
            ;; EMT V3.0 成功: 最適な中道解に到達した
            (setf (emt-process-emt-passed-p emt-process) t)))))
    
    emt-process))


(defun emt-run (initial-expr constraints &key (mcc-threshold 0.1) (max-steps 100))
  (let* ((initial-fuss (calculate-fuss initial-expr constraints))
         (initial-proc (make-difw-process :expr initial-expr 
                                          :current-fuss initial-fuss
                                          :constraints constraints))
         (emt-proc (make-emt-process :difw-proc initial-proc 
                                     :mcc-threshold mcc-threshold)))
    (loop :for i :from 1 :to max-steps
          :until (emt-process-emt-passed-p emt-proc)
          :do (setf emt-proc (emt-test-step emt-proc))
          :finally (return emt-proc))))


;;; *END-OF-EMT*
