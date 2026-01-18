;;;; ------------------------------------------------------------
;;;; 改良版 DIFW 拡張ユニット: 二諦版・空側 EMT (Ultimate-side EMT)
;;;; ------------------------------------------------------------

(defstruct (kszs-validator (:conc-name kv-))
  "空側の二諦的コヒーレンスを検証するためのメタ構造体"
  (conventional-abstract-p t) ; 1. 世俗的抽象: 色(Shiki)からの還元が論理的に妥当か
  (ultimate-non-reify-p t)    ; 2. 勝義的非実体: 『空』そのものを実体化していないか
  (mweq-potential 1.0)        ; 3. 中道ポテンシャル: 次の創発(KSZS)への柔軟性
  (meta-coherence 1.0))

(defun calculate-ku-fuss (potential history constraints)
  "空側のポテンシャルにおける二諦的『苦(Fuss)』を測定する。
   単なる情報の欠如ではなく、中道からの逸脱を測る。"
  (let ((abstraction-error (calculate-sszk-error potential)) ; 世俗的歪み: 還元の不完全性
        (reification-penalty (if (detect-svabhava potential) 2.0 0.0)) ; 勝義的歪み: 固定化への罰則
        (entropy-loss (calculate-potential-rigidity potential))) ; 中道的歪み: 柔軟性の喪失
    
    ;; 二諦版 Ku-Fuss 計算式
    ;; 論文の Axiom 1 に基づき、0 への収束ではなく mweq 0 (柔軟な安定) を目指す
    (+ (* 0.3 abstraction-error)
       (* 0.5 reification-penalty) ; 勝義的実体化(NMF)には高い罰則を課す
       (* 0.2 entropy-loss))))

(defun detect-svabhava (potential)
  "『自性』(固定された実体) の検出。
   空の状態において、特定の意味が『唯一の正解』として固定され、
   他の解釈(Ranki)を拒絶している場合に T。"
  ;; 論文 5.1節に基づき、絶対的な自己言及や固定定義を特定
  (check-reification-in-ku-layer potential))

(defun ku-emt-test (potential history &key (threshold 0.3))
  "二諦版 空側 EMT。
   ポテンシャルが単なる『無』ではなく、中道等価(mweq)な安定クラスに
   属しているかを判定する。"
  (let* ((ku-fuss (calculate-ku-fuss potential history nil))
         ;; 世俗的判定: 構造的に安定しているか
         (conventional-ok (< ku-fuss threshold))
         ;; 勝義的判定: 実体化ペナルティが最小か (mw= 0 の本質)
         (ultimate-ok (zerop (calculate-reification-penalty-for-ku potential))))
    
    (format t "[Ku-EMT] Evaluating Two-Truths Coherence...~%")
    (format t " > Conventional Stability: ~A~%" conventional-ok)
    (format t " > Ultimate Non-Reification: ~A~%" ultimate-ok)
    
    ;; 両方の真理が中道で合致している場合のみパス (Dfix0の成立)
    (and conventional-ok ultimate-ok)))

(defun emergent-shikika (potential history constraints)
  "二諦版 EMT を通過したポテンシャルを『色化』(KSZS)させる。
   これは mweq 0 という『空』から、具体的な『色』を再構成するプロセス。"
  (if (ku-emt-test potential history)
      (let ((shiki (generate-coherent-expression potential)))
        ;; 論文の通り、これは同一性(strict identity)ではなく中道等価(mweq)な創発
        (format t "[Emergence] KSZS -> SSZK: Middle-Way Fixed Point (mweq) reached.~%")
        shiki)
      (progn
        (format t "[Failure] Potential trapped in Reification or Incoherence.~%")
        nil)))

;;;; ------------------------------------------------------------
;;;; システムへの統合：二重 EMT / 二諦同期プロトコル
;;;; ------------------------------------------------------------

(defun total-difw-eval (expr history constraints)
  "色側 (世俗諦) と 空側 (勝義諦) の両方の EMT を同期。
   単なる計算ではなく、二諦の不二を計算論的に証明(空証明)するプロセス。"
  (let ((potential (abstract-to-ku expr)))
    (format t "--- Starting Dual-Truth Verification (SKDT) ---~%")
    ;; 1. 空側での勝義的検証 (Ku-EMT: 執着がないか)
    (if (ku-emt-test potential history)
        ;; 2. 色側での世俗的検証 (Shiki-DIFW: 論理が正しいか)
        (ku-eval expr :history history :constraints constraints)
        (progn
          (format t "[Process Halted] Ultimate Truth violation (NMF detected).~%")
          nil))))
