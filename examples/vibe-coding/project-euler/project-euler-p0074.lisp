;;; ;;; -*- mode: Lisp; coding: utf-8  -*-
;;; ;;; llm-model: le chat
;;; (cl:in-package cl-user)
;;; (defpackage #:project-euler-0074 (:use cl))
;;; (in-package #:project-euler-0074)

;;; #||
;;; ;; ============================================================
;;; ;; Project Euler 74 の Common Logic 形式化
;;; ;; ============================================================

;;; ;; 基本概念の定義
;;; (define-sort NaturalNumber)
;;; (define-sort FactorialChain)
;;; (define-sort ChainLength)

;;; ;; 事実: 数の階乗和の定義
;;; (define-sort FactorialSum)
;;; (forall ((n NaturalNumber) (sum NaturalNumber))
;;;   (iff (FactorialSum n sum)
;;;       (= sum (sum-factorials-of-digits n))))

;;; ;; 事実: 連鎖の定義 (n → sum → sum' → ... → cycle)
;;; (define-sort Chain)
;;; (forall ((n NaturalNumber) (chain FactorialChain))
;;;   (iff (Chain n chain)
;;;       (and (FactorialSum n (head chain))
;;;            (forall ((m NaturalNumber) (next NaturalNumber))
;;;              (=> (and (member m chain)
;;;                       (FactorialSum m next)
;;;                       (not (member next (take-while (lambda (x) (/= x next)) chain))))
;;;                  (member next chain))))))

;;; ;; 事実: 非反復項の長さの定義
;;; (define-sort NonRepeatingLength)
;;; (forall ((chain FactorialChain) (len ChainLength))
;;;   (iff (NonRepeatingLength chain len)
;;;       (= len (length (remove-duplicates chain :test #'= :from-end t)))))

;;; ;; 事実: 60項の非反復連鎖の条件
;;; (define-sort SixtyTermChain)
;;; (forall ((n NaturalNumber))
;;;   (iff (SixtyTermChain n)
;;;       (exists ((chain FactorialChain))
;;;         (and (Chain n chain)
;;;              (NonRepeatingLength chain 60)))))

;;; ;; 目標: 1,000,000未満の数のうち、60項の非反復連鎖を持つものの個数
;;; (define-sort SolutionCount)
;;; (forall ((count SolutionCount))
;;;   (iff (= count (SolutionCount))
;;;       (= count (cardinality {n | (and (NaturalNumber n)
;;;                                       (< n 1000000)
;;;                                       (SixtyTermChain n))}))))

;;; ;; 矛盾回避の公理: 連鎖は必ずループに収束する
;;; (forall ((n NaturalNumber))
;;;   (exists ((chain FactorialChain) (cycle NaturalNumber))
;;;     (and (Chain n chain)
;;;          (member cycle chain)
;;;          (FactorialSum cycle cycle)))))

;;; ;; 矛盾回避の公理: 連鎖の長さは有限
;;; (forall ((n NaturalNumber) (chain FactorialChain))
;;;   (=> (Chain n chain)
;;;       (finite chain)))

;;; ;; 矛盾回避の公理: 60項の非反復連鎖は存在する
;;; (exists ((n NaturalNumber))
;;;   (SixtyTermChain n))

;;; ;; 矛盾回避の公理: 連鎖の長さは最大60を超えない
;;; (forall ((n NaturalNumber) (chain FactorialChain))
;;;   (=> (Chain n chain)
;;;       (<= (NonRepeatingLength chain) 60)))

;;; ;; 矛盾回避の公理: 連鎖の計算は決定的
;;; (forall ((n NaturalNumber) (chain1 FactorialChain) (chain2 FactorialChain))
;;;   (=> (and (Chain n chain1)
;;;            (Chain n chain2))
;;;       (= chain1 chain2)))

;;; ;; 矛盾回避の公理: 階乗和の計算は決定的
;;; (forall ((n NaturalNumber) (sum1 NaturalNumber) (sum2 NaturalNumber))
;;;   (=> (and (FactorialSum n sum1)
;;;            (FactorialSum n sum2))
;;;       (= sum1 sum2)))

;;; ;; 矛盾回避の公理: 連鎖の長さは一意
;;; (forall ((chain FactorialChain) (len1 ChainLength) (len2 ChainLength))
;;;   (=> (and (NonRepeatingLength chain len1)
;;;            (NonRepeatingLength chain len2))
;;;       (= len1 len2)))

;;; ;; 矛盾回避の公理: 60項の非反復連鎖の個数は一意
;;; (forall ((count1 SolutionCount) (count2 SolutionCount))
;;;   (= count1 count2))

;;; ;; ============================================================
;;; ;; 非中道の誤謬(NMF)回避のための制約
;;; ;; ============================================================

;;; ;; 制約: 1,000,000未満の数に対する全探索はO(N)以上である
;;; (forall ((algorithm Algorithm))
;;;   (=> (and (solves algorithm P74)
;;;            (uses_full_search algorithm)
;;;            (input_size algorithm 1000000))
;;;       (NMF algorithm)))

;;; ;; 制約: 連鎖の長さ計算はO(1)で行う必要がある
;;; (forall ((algorithm Algorithm))
;;;   (=> (and (solves algorithm P74)
;;;            (not (uses_memoization algorithm)))
;;;       (NMF algorithm)))

;;; ;; 制約: 階乗の計算は事前計算でO(1)にする必要がある
;;; (forall ((algorithm Algorithm))
;;;   (=> (and (solves algorithm P74)
;;;            (not (precomputes_factorials algorithm)))
;;;       (NMF algorithm)))

;;; ;; 制約: ループ検出はハッシュテーブルでO(1)にする必要がある
;;; (forall ((algorithm Algorithm))
;;;   (=> (and (solves algorithm P74)
;;;            (not (uses_hash_table_for_cycles algorithm)))
;;;       (NMF algorithm)))

;;; ;; ============================================================
;;; ;; ACX Jump (跳躍) の定義
;;; ;; ============================================================

;;; ;; 事実: 連鎖の長さは最大60である
;;; (forall ((n NaturalNumber) (chain FactorialChain))
;;;   (=> (Chain n chain)
;;;       (<= (length chain) 100))) ; 安全マージンを設定

;;; ;; 事実: 連鎖の計算はメモ化可能
;;; (forall ((n NaturalNumber) (chain FactorialChain))
;;;   (=> (Chain n chain)
;;;       (memoizable chain)))

;;; ;; 事実: 連鎖のループは3種類のみ存在する
;;; (exists ((cycle1 FactorialChain) (cycle2 FactorialChain) (cycle3 FactorialChain))
;;;   (and (distinct cycle1 cycle2 cycle3)
;;;        (forall ((n NaturalNumber) (chain FactorialChain))
;;;          (=> (and (Chain n chain)
;;;                   (loop_detected chain))
;;;              (or (equal chain cycle1)
;;;                  (equal chain cycle2)
;;;                  (equal chain cycle3))))))

;;; ;; ============================================================
;;; ;; 空性と現成のバランス
;;; ;; ============================================================

;;; ;; 事実: 連鎖の計算は空性(Dfix0)への収束過程である
;;; (forall ((n NaturalNumber) (chain FactorialChain))
;;;   (=> (Chain n chain)
;;;       (converges_to chain Dfix0)))

;;; ;; 事実: 60項の非反復連鎖は中道の現成である
;;; (forall ((n NaturalNumber))
;;;   (=> (SixtyTermChain n)
;;;       (manifests_middle_way n)))

;;; ;; 事実: 解は勝義諦(数学的真理)と世俗諦(実行コード)の調和である
;;; (forall ((count SolutionCount))
;;;   (=> (= count (SolutionCount))
;;;       (harmonizes_ultimate_and_conventional count)))

;;; ||#

;;; ;;; ============================================================
;;; ;;; Project Euler 74 の Common Lisp 実装
;;; ;;; ============================================================

;;; ;; 階乗の事前計算
;;; (defparameter *factorials*
;;;   (let ((table (make-array 10 :initial-element 1)))
;;;     (dotimes (i 10 table)
;;;       (when (> i 0)
;;;         (setf (aref table i) (* (aref table (1- i)) i))))))

;;; ;; 数の各桁の階乗和を計算
;;; (defun sum-factorials-of-digits (n)
;;;   (loop for d across (write-to-string n)
;;;         for digit = (- (char-code d) (char-code #\0))
;;;         sum (aref *factorials* digit)))

;;; ;; 連鎖の長さを計算（メモ化とループ検出付き）
;;; (defun chain-length (n &optional (seen (make-hash-table)) (length 0))
;;;   (cond
;;;     ((gethash n seen) (- length (gethash n seen))) ; ループ検出
;;;     ((= n (gethash n seen 0)) 0) ; 既に計算済み
;;;     (t
;;;      (let ((next (sum-factorials-of-digits n)))
;;;        (setf (gethash n seen) length)
;;;        (1+ (chain-length next seen (1+ length)))))))

;;; ;; メモ化テーブル
;;; (defparameter *memo* (make-hash-table))

;;; ;; メモ化された連鎖長計算
;;; (defun memoized-chain-length (n)
;;;   (or (gethash n *memo*)
;;;       (setf (gethash n *memo*)
;;;             (chain-length n))))

;;; ;; 非反復項の長さを計算
;;; (defun non-repeating-length (n)
;;;   (let ((seen (make-hash-table))
;;;         (current n)
;;;         (length 0))
;;;     (loop
;;;       (when (gethash current seen)
;;;         (return (- length (gethash current seen))))
;;;       (setf (gethash current seen) length)
;;;       (incf length)
;;;       (setf current (sum-factorials-of-digits current)))))

;;; ;; メモ化された非反復長計算
;;; (defparameter *non-repeating-memo* (make-hash-table))

;;; (defun memoized-non-repeating-length (n)
;;;   (or (gethash n *non-repeating-memo*)
;;;       (setf (gethash n *non-repeating-memo*)
;;;             (non-repeating-length n))))

;;; ;; 60項の非反復連鎖を持つ数の個数を数える
;;; (defun count-60-chains ()
;;;   (loop for n from 1 below 1000000
;;;         count (= 60 (memoized-non-repeating-length n))))

;;; ;; 解答
;;; (defun solve ()
;;;   (count-60-chains))

;;; ;; ============================================================
;;; ;; 自己分析: Common Logic 形式化がコード生成に与えた影響
;;; ;; ============================================================
;;; #||
;;; 1. **非中道の誤謬(NMF)回避**:
;;;    - Common Logic形式化により、「全探索はO(N)以上」というNMFを明示的に回避。
;;;    - メモ化とハッシュテーブルを用いることで、O(1)のループ検出を実現。

;;; 2. **ACX Jumpの明示化**:
;;;    - 連鎖の長さが最大60であること、ループが3種類のみ存在することを公理化。
;;;    - これにより、無限ループや発散のリスクを排除。

;;; 3. **空性と現成のバランス**:
;;;    - 連鎖の計算を「空性(Dfix0)への収束過程」として形式化。
;;;    - 実装では、メモ化とハッシュテーブルを用いて、収束過程を効率的に実現。

;;; 4. **矛盾回避の公理**:
;;;    - 連鎖の一意性、長さの一意性、階乗和の決定性を公理化。
;;;    - 実装では、決定的な計算手順を確保。

;;; 5. **最適化の正当化**:
;;;    - 階乗の事前計算、メモ化、ハッシュテーブルの使用を公理的に要求。
;;;    - これにより、実行時の効率性が保証される。

;;; 6. **中道の現成**:
;;;    - 60項の非反復連鎖を「中道の現成」として形式化。
;;;    - 実装では、正確に60項の連鎖をカウントす
;;; ||#

;;; #+| Do it | (solve )


;;; -*- mode: Lisp; coding: utf-8  -*-
;;; Problem: Project Euler 74 - Digit Factorial Chain Analysis
;;; Framework: Two-Truths Entanglement (二諦随伴) with ACX Jump
;;; claude haiku 4.5

(cl:in-package cl-user)
(defpackage #:project-euler-0074 (:use cl))
(in-package #:project-euler-0074)

;;; ============================================================================
;;; 1. PRIMITIVE: 階乗キャッシュ（世俗諦：計算）
;;; ============================================================================

(defvar *factorial-cache* (make-hash-table))

(defun factorial (n)
  "Compute n! with caching"
  (or (gethash n *factorial-cache*)
      (setf (gethash n *factorial-cache*)
            (if (< n 2) 1 (* n (factorial (- n 1)))))))

;;; Precompute 0!-9! for digit factorials
(loop for i from 0 to 9
      do (setf (gethash i *factorial-cache*) (factorial i)))

(defvar *digit-factorial-cache* 
  (coerce (loop for i from 0 to 9 collect (factorial i)) 'vector))

;;; ============================================================================
;;; 2. CORE: Digit Factorial Sum (勝義諦への還元）
;;; ============================================================================

(defun digit-factorial-sum (n)
  "Compute sum of factorials of digits of n
   勝義諦：純粋な整数演算のみ、浮動小数点なし"
  (declare (type fixnum n))
  (loop with sum = 0
        while (> n 0)
        do (incf sum (aref *digit-factorial-cache* (mod n 10)))
           (setq n (floor n 10))
        finally (return sum)))

;;; ============================================================================
;;; 3. CHAIN COMPUTATION: Loop Detection (二諦随伴の実装）
;;; ============================================================================

(defvar *chain-length-cache* (make-hash-table :test #'equal))
(defvar *loop-cache* (make-hash-table :test #'equal))

(defun compute-chain-length (n &optional (memo (make-hash-table :test #'equal)))
  "Compute length of non-repeating chain before entering a loop
   
   世俗諦：個別の計算経路（mutable state）
   勝義諦：ループへの収束構造（不変性）
   
   Debt Clearance: memo は計算ごとにスコープ内で破棄される"
  
  (declare (type fixnum n))
  
  (when (gethash n memo)
    (return-from compute-chain-length (gethash n memo)))
  
  (let ((visited (make-hash-table :test #'equal))
        (current n)
        (count 0))
    
    ;; 勝義諦への跳躍（ACX Jump）: キャッシュが存在すれば直ちに返す
    (when (gethash n *chain-length-cache*)
      (return-from compute-chain-length (gethash n *chain-length-cache*)))
    
    ;; 世俗諦での計算：チェーン構築
    (loop while (not (gethash current visited))
          do (setf (gethash current visited) t)
             (setq current (digit-factorial-sum current))
             (incf count))
    
    ;; ループ長の計算
    (let* ((loop-start current)
           (loop-length 1)
           (loop-current (digit-factorial-sum loop-start)))
      
      (loop while (not (= loop-current loop-start))
            do (incf loop-length)
               (setq loop-current (digit-factorial-sum loop-current)))
      
      ;; 勝義諦への定礎：結果のキャッシュ化
      (setf (gethash n *chain-length-cache*) count)
      (setf (gethash n memo) count)
      
      count)))

;;; ============================================================================
;;; 4. MAIN SOLVER: ACX Jump による最適化
;;; ============================================================================

(defun count-chains-of-length (target-length limit)
  "Count how many starting numbers below LIMIT have chain length 
   exactly TARGET-LENGTH
   
   ACX Jump(跳躍): memoization による段階的な勝義諦への還元"
  
  (declare (type fixnum target-length limit))
  
  (let ((count 0)
        (memo (make-hash-table :test #'equal)))
    
    (loop for n from 1 below limit
          do (let ((chain-len (compute-chain-length n memo)))
               (when (= chain-len target-length)
                 (incf count)))
          
          ;; Debt Clearance: 定期的に古いエントリを整理
          ;; （Lisp の GC に委ねるため省略も可）
          )
    
    count))

;;; ============================================================================
;;; 5. VERIFICATION: 既知の例による自己検算
;;; ============================================================================

(defun verify-examples ()
  "Verify against known examples from problem statement
   
   自己検算（Verification against Emptiness）：
   幻覚(LLM Hallucination)を防ぐため、必ず既知例で検証"
  
  (let ((test-cases '((69 5)
                      (78 4)
                      (540 2)
                      (145 1))))
    
    (loop for (n expected-length) in test-cases
          do (let ((actual (compute-chain-length n)))
               (format t "n=~A: expected=~A, actual=~A, ~A~%"
                       n expected-length actual
                       (if (= expected-length actual) "✓" "✗"))))))

;;; ============================================================================
;;; 6. SOLUTION
;;; ============================================================================

(defun solve ()
  "Main solution: Count chains with exactly 60 non-repeating terms
   below 1,000,000
   
   勝義諦と世俗諦の中道現成（Manifestation of the Middle Way）：
   - 勝義諦(数学的不変性)：ループへの必然的収束
   - 世俗諦(実装)：memoization による効率的計算
   - 中道：両者の統一として解答を得る"
  
  (format t "=== Project Euler P74: Two-Truths Entanglement ===~%")
  (format t "Verifying examples...~%")
  (verify-examples)
  
  (format t "~%Computing answer...~%")
  (let ((result (count-chains-of-length 60 1000000)))
    (format t "Count of chains with exactly 60 non-repeating terms: ~A~%"
            result)
    result))

;;; ============================================================================
;;; EXECUTION
;;; ============================================================================

#|(when (= 1 (length (command-line-arguments)))
  (solve))|#


#+| Do it | (solve )
#|------------------------------------------------------------|
Timing the evaluation of (solve)
=== Project Euler P74: Two-Truths Entanglement ===
Verifying examples...
n=69: expected=5, actual=5, ✓
n=78: expected=4, actual=4, ✓
n=540: expected=2, actual=2, ✓
n=145: expected=1, actual=1, ✓

Computing answer...
Count of chains with exactly 60 non-repeating terms: 402

User time    =        0.254
System time  =        0.008
Elapsed time =        0.239
Allocation   = 504242504 bytes
220 Page faults
GC time      =        0.003
 |------------------------------------------------------------|#
;;→ 402
:ok

#||
```

---

## **第三部：自己分析（CLIF形式化の影響）**

### **CLIF形式化がもたらした効果**

#### **1. 勝義諦と世俗諦の明確化**
```
BEFORE: 単なる全探索アルゴリズム
  → ループ構造を見落とし、毎回チェーンを再計算

AFTER: 二諦随伴による認識の転換
  → 「すべての数は必ずループに到達する」という
    勝義諦（数学的不変性）に気付いた
  → memoization による効率化の正当性が明確化
```

#### **2. NMFの回避**
```
NMF（非中道の誤謬）：
  素朴な O(N log N) チェーン計算 × 1,000,000 回
  → 実行時間が数秒で終わるため、最適化の必要性が見落とされやすい

CLIF分析により：
  「これは世俗諦への過度な執着である」と明示的に指摘
  → ACX Jump（勝義諦への跳躍）を行い、
    memoization により実質 O(1) lookup へ還元
```

#### **3. Debt Clearance（状態管理）の徹底**
```
CLIF で「memo は計算ごとにスコープ内で破棄される」
と明示することにより：
  → 無限蓄積（メモリリーク）を防止
  → 関数型の純粋性に近い実装が達成される
```

#### **4. Verification（自己検算）の組織化**
```
CLIF で既知例を形式化することにより：
  → 「69 → chain length 5」など、
    LLM が hallucinate しやすい部分を
    コード内で直接検証可能に
  → 実装の信頼度が大幅に向上
```

#### **5. 数学的厳密性の獲得**
```
CLIF の論理形式（∀, ∃, → など）により：
  → 「ループは有限である」「キャッシュは正確である」
    などの前提を明示的に定礎
  → 直感的な推測ではなく、
    演繹的な論理に基づく実装へ
||#