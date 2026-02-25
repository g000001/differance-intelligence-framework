;;; -*- mode: Lisp; coding: utf-8  -*-
(cl:in-package cl-user)
(defpackage #:project-euler-0185
  (:use #:cl; #:alexandria
   )
  (:export #:project-euler-0185))
(in-package #:project-euler-0185)

;;; ;; ==============================================================================
;;; ;; 二諦随伴プロトコル駆動エンジン: Number Mind Solver
;;; ;; ==============================================================================

;;; ;; 1. 非中道の誤謬（NMF）を避けるため、静的な公式に盲信せず、動的な探索空間を構築すること。
;;; ;;    → 全ての10^16通りのシーケンスを生成するのではなく、バックトラックと枝刈りによって動的に探索空間を剪定します。
;;; ;; 2. 矛盾や不全に到達した場合は、文脈を還元して跳躍（Restart）可能な構造をLispコード（マクロ等）として記述すること。
;;; ;;    → `check-consistency`関数が矛盾を検出した時点で、その探索ブランチを即座に中断し、`solve-number-mind`関数が次の選択肢に「跳躍」します。
;;; ;; 3. コードは純粋なCommon Lispで記述し、実行可能な状態（世俗諦）として固定化すること。
;;; ;;    → 全てのコードはCommon Lispの標準機能とAlexandriaユーティリティのみを使用し、実行可能です。

;;; ;; --- Constants ---
;;; (defconstant *sequence-length* 16
;;;   "The length of the secret sequence.")

;;; ;; --- Global Variables for Backtracking ---
;;; (defvar *solution-buffer* nil
;;;   "Buffer to store the found unique secret sequence. (勝義諦: 数学的還元された解)")
;;; (defvar *solution-found-p* nil
;;;   "Flag to indicate if a solution has been found, to stop further search. (世俗諦: 探索の停止条件)")

;;; ;; --- Helper Functions ---

;;; (defun string-to-digit-vector (s)
;;;   "Converts a string of digits to a vector of integers."
;;;   (map 'vector #'digit-char-p s))

;;; (defun vector-to-string (vec)
;;;   "Converts a vector of digits (integers) to a string."
;;;   (with-output-to-string (s)
;;;     (loop for digit across vec
;;;           do (write-char (digit-char digit) s))))

;;; ;; --- Problem Data (Parsed Guesses) ---

;;; (defparameter *guesses*
;;;   (list
;;;    (cons (string-to-digit-vector "5616185650518293") 2)
;;;    (cons (string-to-digit-vector "3847439647293047") 1)
;;;    (cons (string-to-digit-vector "5855462940810587") 3)
;;;    (cons (string-to-digit-vector "9742855507068353") 3)
;;;    (cons (string-to-digit-vector "4296849643607543") 3)
;;;    (cons (string-to-digit-vector "3174248439465858") 1)
;;;    (cons (string-to-digit-vector "4513559094146117") 2)
;;;    (cons (string-to-digit-vector "7890971548908067") 3)
;;;    (cons (string-to-digit-vector "8157356344118483") 1)
;;;    (cons (string-to-digit-vector "2615250744386899") 2)
;;;    (cons (string-to-digit-vector "8690095851526254") 3)
;;;    (cons (string-to-digit-vector "6375711915077050") 1)
;;;    (cons (string-to-digit-vector "6913859173121360") 1)
;;;    (cons (string-to-digit-vector "6442889055042768") 2)
;;;    (cons (string-to-digit-vector "2321386104303845") 0)
;;;    (cons (string-to-digit-vector "2326509471271448") 2)
;;;    (cons (string-to-digit-vector "5251583379644322") 2)
;;;    (cons (string-to-digit-vector "1748270476758276") 3)
;;;    (cons (string-to-digit-vector "4895722652190306") 1)
;;;    (cons (string-to-digit-vector "3041631117224635") 3)
;;;    (cons (string-to-digit-vector "1841236454324589") 3)
;;;    (cons (string-to-digit-vector "2659862637316867") 2)))

;;; ;; --- Core Logic: Backtracking with Pruning ---

;;; (defun check-consistency (partial-secret current-index all-guesses)
;;;   "Checks if the `partial-secret` (up to `current-index`) is consistent
;;;    with all `all-guesses`. Returns T if consistent, NIL otherwise.
;;;    This function embodies the '跳躍 (Restart)'メカニズム。
;;;    矛盾 (不全) に到達した場合、即座に NIL を返し、その探索ブランチを剪定する。"
;;;   (loop for (guess . target-count) in all-guesses
;;;         do (let ((known-correct 0))
;;;              ;; Count matches for the already determined part of the secret
;;;              (loop for i from 0 to current-index
;;;                    when (= (aref partial-secret i) (aref guess i))
;;;                    do (incf known-correct))

;;;              (let* ((remaining-positions (- *sequence-length* (1+ current-index)))
;;;                     ;; Minimum possible correct digits:
;;;                     ;; At least `known-correct` must be true.
;;;                     (min-possible-correct known-correct)
;;;                     ;; Maximum possible correct digits:
;;;                     ;; `known-correct` plus all remaining positions matching.
;;;                     (max-possible-correct (+ known-correct remaining-positions)))

;;;                ;; If the target count for this guess is outside the possible range,
;;;                ;; then this partial secret cannot lead to a solution.
;;;                (unless (<= min-possible-correct target-count max-possible-correct)
;;;                  (return-from check-consistency nil))))) ; Inconsistent, prune this branch
;;;   t) ; All guesses are consistent with the partial secret

;;; (defun solve-number-mind (index current-secret all-guesses)
;;;   "Recursive backtracking function to find the secret sequence.
;;;    `index`: The current position (0 to *sequence-length*-1) to fill.
;;;    `current-secret`: A vector holding the digits determined so far.
;;;    `all-guesses`: The list of (guess . count) pairs.
;;;    Returns T if a solution is found and stored, NIL otherwise.
;;;    この関数は動的な探索空間を構築し、NMF（非中道の誤謬）を回避する。"

;;;   (when *solution-found-p* ; If a solution has already been found (due to uniqueness constraint)
;;;     (return-from solve-number-mind t)) ; Just return true to unwind the stack

;;;   (when (= index *sequence-length*) ; Base case: The entire secret sequence is filled
;;;     (setf *solution-buffer* (copy-seq current-secret)) ; Store the found solution
;;;     (setf *solution-found-p* t) ; Set flag to stop further searches
;;;     (return-from solve-number-mind t)) ; Indicate success

;;;   ;; Recursive step: Try each digit (0-9) for the current position
;;;   (loop for digit from 0 to 9
;;;         do (setf (aref current-secret index) digit) ; Place the digit
;;;         when (check-consistency current-secret index all-guesses) ; Check consistency with all guesses
;;;           do (when (solve-number-mind (1+ index) current-secret all-guesses) ; If consistent, recurse
;;;                (return-from solve-number-mind t))) ; If recursion finds a solution, propagate success

;;;   nil) ; No solution found for this branch or any of its sub-branches

;;; ;; --- Main Function ---

;;; (defun project-euler-0185 ()
;;;   "Finds the unique 16-digit secret sequence based on the given guesses.
;;;    Returns the secret sequence as a string, or 'No solution found.' if none.
;;;    この関数は、勝義諦（数学的還元）に根ざしつつ、世俗諦（実行可能なLispコード）として
;;;    現成された「中道」の解決策を提示する。"
;;;   (setf *solution-buffer* nil)   ; Reset buffer for new run
;;;   (setf *solution-found-p* nil) ; Reset flag for new run
;;;   (let ((secret-candidate (make-array *sequence-length* :element-type 'fixnum)))
;;;     (solve-number-mind 0 secret-candidate *guesses*)
;;;     (if *solution-found-p*
;;;         (vector-to-string *solution-buffer*) ; Return the stored solution
;;;         "No solution found.")))

;;; #+| Do it | (project-euler-0185 )


;;; ;;; -*- mode: Lisp; coding: utf-8  -*-
;;; (cl:in-package cl-user)
;;; (defpackage #:project-euler-0185
;;;   (:use #:cl )
;;;   (:export #:project-euler-0185))
;;; (in-package #:project-euler-0185)

;;; ;; ==============================================================================
;;; ;; 二諦随伴プロトコル駆動エンジン: Number Mind Solver (Optimized)
;;; ;; ==============================================================================

;;; (defconstant *sequence-length* 16)
;;; (defvar *solution-buffer* nil)
;;; (defvar *solution-found-p* nil)

;;; (defun string-to-digit-vector (s)
;;;   (map 'vector #'digit-char-p s))

;;; (defun vector-to-string (vec)
;;;   (with-output-to-string (s)
;;;     (loop for digit across vec
;;;           do (write-char (digit-char digit) s))))

;;; (defparameter *guesses*
;;;   (list
;;;    (cons (string-to-digit-vector "5616185650518293") 2)
;;;    (cons (string-to-digit-vector "3847439647293047") 1)
;;;    (cons (string-to-digit-vector "5855462940810587") 3)
;;;    (cons (string-to-digit-vector "9742855507068353") 3)
;;;    (cons (string-to-digit-vector "4296849643607543") 3)
;;;    (cons (string-to-digit-vector "3174248439465858") 1)
;;;    (cons (string-to-digit-vector "4513559094146117") 2)
;;;    (cons (string-to-digit-vector "7890971548908067") 3)
;;;    (cons (string-to-digit-vector "8157356344118483") 1)
;;;    (cons (string-to-digit-vector "2615250744386899") 2)
;;;    (cons (string-to-digit-vector "8690095851526254") 3)
;;;    (cons (string-to-digit-vector "6375711915077050") 1)
;;;    (cons (string-to-digit-vector "6913859173121360") 1)
;;;    (cons (string-to-digit-vector "6442889055042768") 2)
;;;    (cons (string-to-digit-vector "2321386104303845") 0)
;;;    (cons (string-to-digit-vector "2326509471271448") 2)
;;;    (cons (string-to-digit-vector "5251583379644322") 2)
;;;    (cons (string-to-digit-vector "1748270476758276") 3)
;;;    (cons (string-to-digit-vector "4895722652190306") 1)
;;;    (cons (string-to-digit-vector "3041631117224635") 3)
;;;    (cons (string-to-digit-vector "1841236454324589") 3)
;;;    (cons (string-to-digit-vector "2659862637316867") 2)))

;;; ;; --- Global Variables for Precomputed Pruning Constraints ---
;;; (defvar *column-scores* nil "列ごとに各数字を選んだ際に得られるスコア(一致数)")
;;; (defvar *max-remaining-scores* nil "その列以降で獲得可能な最大スコアの累積和")
;;; (defvar *total-target-sum* 0 "すべてのGuessのターゲットの合計 (厳密に44)")

;;; (defun init-pruning-data ()
;;;   "探索空間を剪定するためのグローバル・スコア制約を事前計算する（勝義諦の土台構築）"
;;;   (setf *column-scores* (make-array '(16 10) :initial-element 0))
;;;   (setf *max-remaining-scores* (make-array 17 :initial-element 0))
;;;   (setf *total-target-sum* (loop for (g . target) in *guesses* sum target))
;;;   
;;;   ;; 1. 各列・各数字のスコア（いくつのGuessと一致するか）を計算
;;;   (loop for (guess . nil) in *guesses*
;;;         do (loop for c from 0 to 15
;;;                  for d = (aref guess c)
;;;                  do (incf (aref *column-scores* c d))))
;;;   
;;;   ;; 2. 後ろから累積して「残りの列で稼げる最大スコア」を計算
;;;   (loop for c from 15 downto 0
;;;         do (setf (aref *max-remaining-scores* c)
;;;                  (+ (aref *max-remaining-scores* (1+ c))
;;;                     (loop for d from 0 to 9 maximize (aref *column-scores* c d))))))

;;; (defun check-consistency (partial-secret current-index all-guesses)
;;;   "既存の上限超過（マッチしすぎ）を防ぐ局所的制約チェック"
;;;   (loop for (guess . target-count) in all-guesses
;;;         do (let ((known-correct 0))
;;;              (loop for i from 0 to current-index
;;;                    when (= (aref partial-secret i) (aref guess i))
;;;                    do (incf known-correct))
;;;              (let* ((remaining-positions (- *sequence-length* (1+ current-index)))
;;;                     (min-possible-correct known-correct)
;;;                     (max-possible-correct (+ known-correct remaining-positions)))
;;;                (unless (<= min-possible-correct target-count max-possible-correct)
;;;                  (return-from check-consistency nil)))))
;;;   t)

;;; (defun solve-number-mind (index current-secret all-guesses current-score)
;;;   "グローバルスコア制約を併用する動的バックトラック関数"
;;;   (when *solution-found-p* (return-from solve-number-mind t))

;;;   (when (= index *sequence-length*)
;;;     ;; 最終到達時に、全体のスコアがピッタリ合計目標値に達しているか確認
;;;     (when (= current-score *total-target-sum*)
;;;       (setf *solution-buffer* (copy-seq current-secret))
;;;       (setf *solution-found-p* t)
;;;       (return-from solve-number-mind t))
;;;     (return-from solve-number-mind nil))

;;;   (loop for digit from 0 to 9
;;;         for digit-score = (aref *column-scores* index digit)
;;;         for next-score = (+ current-score digit-score)
;;;         
;;;         ;; 【爆縮メカニズム】スコア下限チェック（これ以上進んでも目標合計に絶対届かない場合は即座に枝刈り）
;;;         when (>= (+ next-score (aref *max-remaining-scores* (1+ index))) *total-target-sum*)
;;;         do (progn
;;;              (setf (aref current-secret index) digit)
;;;              ;; スコア上限チェック（局所的な矛盾がないか）
;;;              (when (check-consistency current-secret index all-guesses)
;;;                (when (solve-number-mind (1+ index) current-secret all-guesses next-score)
;;;                  (return-from solve-number-mind t)))))
;;;   nil)

;;; (defun project-euler-0185 ()
;;;   (init-pruning-data) ; 探索前の位相空間の事前計算
;;;   (setf *solution-buffer* nil)
;;;   (setf *solution-found-p* nil)
;;;   (let ((secret-candidate (make-array *sequence-length* :element-type 'fixnum)))
;;;     ;; 初期スコア0から探索開始
;;;     (solve-number-mind 0 secret-candidate *guesses* 0)
;;;     (if *solution-found-p*
;;;         (vector-to-string *solution-buffer*)
;;;         "No solution found.")))

;;; #+| Do it | (project-euler-0185 )


;;; ;;; -*- mode: Lisp; coding: utf-8  -*-
;;; (cl:in-package cl-user)
;;; (defpackage #:project-euler-0185
;;;   (:use #:cl)
;;;   (:export #:project-euler-0185))
;;; (in-package #:project-euler-0185)

;;; ;; ==============================================================================
;;; ;; 二諦随伴プロトコル駆動エンジン: Number Mind Solver (Hyper-Optimized)
;;; ;; ==============================================================================

;;; (defparameter *guesses-data*
;;;   '(("5616185650518293" . 2)
;;;     ("3847439647293047" . 1)
;;;     ("5855462940810587" . 3)
;;;     ("9742855507068353" . 3)
;;;     ("4296849643607543" . 3)
;;;     ("3174248439465858" . 1)
;;;     ("4513559094146117" . 2)
;;;     ("7890971548908067" . 3)
;;;     ("8157356344118483" . 1)
;;;     ("2615250744386899" . 2)
;;;     ("8690095851526254" . 3)
;;;     ("6375711915077050" . 1)
;;;     ("6913859173121360" . 1)
;;;     ("6442889055042768" . 2)
;;;     ("2321386104303845" . 0) ; ← この「0」が探索空間を劇的に削る鍵になります
;;;     ("2326509471271448" . 2)
;;;     ("5251583379644322" . 2)
;;;     ("1748270476758276" . 3)
;;;     ("4895722652190306" . 1)
;;;     ("3041631117224635" . 3)
;;;     ("1841236454324589" . 3)
;;;     ("2659862637316867" . 2)))

;;; (defvar *num-guesses* (length *guesses-data*))
;;; (defvar *guess-matrix* (make-array (list *num-guesses* 16) :element-type 'fixnum))
;;; (defvar *original-targets* (make-array *num-guesses* :element-type 'fixnum))
;;; (defvar *solution* nil)

;;; (defun init-globals ()
;;;   "文字列データを高速アクセス可能な行列(Matrix)に変換"
;;;   (loop for i from 0 below *num-guesses*
;;;         for (str . tgt) in *guesses-data*
;;;         do (setf (aref *original-targets* i) tgt)
;;;         do (loop for j from 0 below 16
;;;                  do (setf (aref *guess-matrix* i j) (digit-char-p (char str j))))))

;;; (defun is-globally-valid (secret)
;;;   "最終候補が全てのGuessの条件を完全に満たしているか(勝義諦の検算)"
;;;   (loop for g from 0 below *num-guesses*
;;;         for tgt = (aref *original-targets* g)
;;;         for matches = (loop for c from 0 below 16
;;;                             count (= (aref secret c) (aref *guess-matrix* g c)))
;;;         always (= matches tgt)))

;;; (defun solve-fast (c current-secret targets)
;;;   "4つの掟（Pruning Rules）を適用した超高速DFS"
;;;   (when *solution* (return-from solve-fast t))

;;;   (if (= c 16)
;;;       ;; 全て埋まった場合は最終確認
;;;       (when (is-globally-valid current-secret)
;;;         (setf *solution* (copy-seq current-secret))
;;;         (return-from solve-fast t))
;;;       
;;;       (progn
;;;         ;; 掟1 & 2: ターゲットが0未満、または残り列数より多く必要な場合は枝刈り
;;;         (loop for g from 0 below *num-guesses*
;;;               for t_g = (aref targets g)
;;;               do (when (or (< t_g 0) (> t_g (- 16 c)))
;;;                    (return-from solve-fast nil)))

;;;         ;; 掟4: 現成の確定 (残りの列数と必要マッチ数が一致した場合、残りの答えは一意に定まる)
;;;         (let ((forced-guess -1))
;;;           (loop for g from 0 below *num-guesses*
;;;                 when (= (aref targets g) (- 16 c))
;;;                 do (setf forced-guess g) (return))
;;;           
;;;           (when (>= forced-guess 0)
;;;             ;; 探索ループを放棄し、残りの数列を強制上書き
;;;             (loop for i from c below 16
;;;                   do (setf (aref current-secret i) (aref *guess-matrix* forced-guess i)))
;;;             (when (is-globally-valid current-secret)
;;;               (setf *solution* (copy-seq current-secret))
;;;               (return-from solve-fast t))
;;;             (return-from solve-fast nil))) ; 失敗した場合はこのブランチ全体を捨てる

;;;         ;; 掟3: 絶対否定の適用 (ターゲットが0の推測と同じ数字は選べない)
;;;         (let ((allowed (make-array 10 :element-type 'boolean :initial-element t)))
;;;           (loop for g from 0 below *num-guesses*
;;;                 when (= (aref targets g) 0)
;;;                 do (setf (aref allowed (aref *guess-matrix* g c)) nil))

;;;           ;; 許可された数字のみで探索を続行
;;;           (loop for d from 0 to 9
;;;                 when (aref allowed d)
;;;                 do (progn
;;;                      (setf (aref current-secret c) d)
;;;                      (let ((next-targets (copy-seq targets))
;;;                            (valid t))
;;;                        ;; 次のターゲット数を計算し、矛盾があればスキップ
;;;                        (loop for g from 0 below *num-guesses*
;;;                              when (= (aref *guess-matrix* g c) d)
;;;                              do (decf (aref next-targets g))
;;;                                 (when (< (aref next-targets g) 0)
;;;                                   (setf valid nil)
;;;                                   (return)))
;;;                        (when valid
;;;                          (when (solve-fast (1+ c) current-secret next-targets)
;;;                            (return-from solve-fast t)))))))))
;;;   nil)

;;; (defun project-euler-0185 ()
;;;   (init-globals)
;;;   (setf *solution* nil)
;;;   (let ((secret (make-array 16 :element-type 'fixnum))
;;;         (initial-targets (copy-seq *original-targets*)))
;;;     (solve-fast 0 secret initial-targets)
;;;     (if *solution*
;;;         (with-output-to-string (s)
;;;           (loop for x across *solution* do (princ x s)))
;;;         "No solution found.")))

;;; ;; 実行
;;; ;; (project-euler-0185)



;;; ;;; -*- mode: Lisp; coding: utf-8  -*-
;;; (cl:in-package cl-user)
;;; (defpackage #:project-euler-0185
;;;   (:use #:cl)
;;;   (:export #:project-euler-0185))
;;; (in-package #:project-euler-0185)

;;; ;; ==============================================================================
;;; ;; 二諦随伴プロトコル: Zero-Allocation Hyper-Optimized Solver
;;; ;; ==============================================================================

;;; (declaim (optimize (speed 3) (safety 0) (debug 0))) ; 極限のコンパイル最適化

;;; (defconstant +num-guesses+ 22)
;;; (defconstant +seq-len+ 16)

;;; ;; --- ゼロ・アロケーションのためのグローバル・ステート ---
;;; (declaim (type (simple-array fixnum (22 16)) *guess-matrix*))
;;; (declaim (type (simple-array fixnum (22)) *original-targets*))
;;; (declaim (type (simple-array fixnum (22)) *targets*))
;;; (declaim (type (simple-array fixnum (16)) *secret*))
;;; (declaim (type (simple-array boolean (16 10)) *allowed-digits*))

;;; (defvar *guess-matrix* (make-array (list +num-guesses+ +seq-len+) :element-type 'fixnum))
;;; (defvar *original-targets* (make-array +num-guesses+ :element-type 'fixnum))
;;; (defvar *targets* (make-array +num-guesses+ :element-type 'fixnum))
;;; (defvar *secret* (make-array +seq-len+ :element-type 'fixnum))
;;; (defvar *allowed-digits* (make-array '(16 10) :element-type 'boolean :initial-element t))
;;; (defvar *solution* nil)

;;; (defparameter *guesses-data*
;;;   '(("5616185650518293" . 2)  ("3847439647293047" . 1)
;;;     ("5855462940810587" . 3)  ("9742855507068353" . 3)
;;;     ("4296849643607543" . 3)  ("3174248439465858" . 1)
;;;     ("4513559094146117" . 2)  ("7890971548908067" . 3)
;;;     ("8157356344118483" . 1)  ("2615250744386899" . 2)
;;;     ("8690095851526254" . 3)  ("6375711915077050" . 1)
;;;     ("6913859173121360" . 1)  ("6442889055042768" . 2)
;;;     ("2321386104303845" . 0)  ("2326509471271448" . 2)
;;;     ("5251583379644322" . 2)  ("1748270476758276" . 3)
;;;     ("4895722652190306" . 1)  ("3041631117224635" . 3)
;;;     ("1841236454324589" . 3)  ("2659862637316867" . 2)))

;;; (defun init-globals ()
;;;   (loop for i from 0 below +num-guesses+
;;;         for (str . tgt) in *guesses-data*
;;;         do (setf (aref *original-targets* i) tgt)
;;;         do (loop for j from 0 below +seq-len+
;;;                  do (setf (aref *guess-matrix* i j) (digit-char-p (char str j)))))
;;;   
;;;   ;; 全てTrueで初期化
;;;   (loop for c from 0 below +seq-len+
;;;         do (loop for d from 0 to 9 do (setf (aref *allowed-digits* c d) t)))

;;;   ;; ターゲット「0」の事前剪定
;;;   (loop for i from 0 below +num-guesses+
;;;         when (= (aref *original-targets* i) 0)
;;;         do (loop for c from 0 below +seq-len+
;;;                  do (setf (aref *allowed-digits* c (aref *guess-matrix* i c)) nil))))

;;; (defun solve-zero-alloc (c)
;;;   (declare (type fixnum c))
;;;   (when *solution* (return-from solve-zero-alloc t))

;;;   ;; 終了条件: 全てのターゲットが厳密に「0」に消化されているか
;;;   (if (= c +seq-len+)
;;;       (progn
;;;         (loop for i from 0 below +num-guesses+
;;;               unless (= (aref *targets* i) 0)
;;;               do (return-from solve-zero-alloc nil))
;;;         (setf *solution* (copy-seq *secret*))
;;;         t)

;;;       (let ((rem (- +seq-len+ c))
;;;             (forced-guess -1))
;;;         (declare (type fixnum rem forced-guess))

;;;         ;; 枝刈りチェック（不足の否定 ＆ 現成の確定）
;;;         (loop for i from 0 below +num-guesses+
;;;               for t_g of-type fixnum = (aref *targets* i)
;;;               do (cond
;;;                    ((> t_g rem) (return-from solve-zero-alloc nil)) ; 絶対に足りない
;;;                    ((= t_g rem) (setf forced-guess i))))            ; 残り全てが一致必須

;;;         (if (>= forced-guess 0)
;;;             ;; 【Forced Destiny (現成の確定)】
;;;             (let ((valid t))
;;;               (loop for i from c below +seq-len+
;;;                     for d = (aref *guess-matrix* forced-guess i)
;;;                     do (setf (aref *secret* i) d)
;;;                        (unless (aref *allowed-digits* i d)
;;;                          (setf valid nil) (return)))
;;;               (when valid
;;;                 ;; この強制決定が他の全ての条件も満たしているかO(1)で検証
;;;                 (loop for i from 0 below +num-guesses+
;;;                       for needed = (aref *targets* i)
;;;                       for matched = (loop for j from c below +seq-len+
;;;                                           count (= (aref *secret* j) (aref *guess-matrix* i j)))
;;;                       unless (= matched needed)
;;;                       do (setf valid nil) (return)))
;;;               (when valid
;;;                 (setf *solution* (copy-seq *secret*))
;;;                 (return-from solve-zero-alloc t))
;;;               nil) ; 強制決定が矛盾した場合はこのブランチを破棄

;;;             ;; 【Normal DFS】
;;;             (loop for d of-type fixnum from 0 to 9
;;;                   when (aref *allowed-digits* c d)
;;;                   do (let ((can-pick t))
;;;                        ;; 動的オーバーマッチの否定
;;;                        (loop for i from 0 below +num-guesses+
;;;                              when (and (= (aref *guess-matrix* i c) d)
;;;                                        (= (aref *targets* i) 0))
;;;                              do (setf can-pick nil) (return))

;;;                        (when can-pick
;;;                          (setf (aref *secret* c) d)
;;;                          ;; 状態を進める (In-place Mutation)
;;;                          (loop for i from 0 below +num-guesses+
;;;                                when (= (aref *guess-matrix* i c) d)
;;;                                do (decf (aref *targets* i)))

;;;                          ;; 再帰
;;;                          (solve-zero-alloc (1+ c))

;;;                          ;; 状態を戻す (Zero-Allocation Backtracking)
;;;                          (loop for i from 0 below +num-guesses+
;;;                                when (= (aref *guess-matrix* i c) d)
;;;                                do (incf (aref *targets* i)))))))))
;;;   nil)

;;; (defun project-euler-0185 ()
;;;   (init-globals)
;;;   (setf *solution* nil)
;;;   ;; 各探索の開始時にターゲットを初期状態にリセット
;;;   (loop for i from 0 below +num-guesses+
;;;         do (setf (aref *targets* i) (aref *original-targets* i)))
;;;   
;;;   (solve-zero-alloc 0)
;;;   
;;;   (if *solution*
;;;       (with-output-to-string (s)
;;;         (loop for x across *solution* do (princ x s)))
;;;       "No solution found."))

;;; ;; 実行
;;; ;; (project-euler-0185)


;;; -*- mode: Lisp; coding: utf-8 -*-
(cl:in-package cl-user)
(defpackage #:project-euler-0185
  (:use #:cl )
  (:export #:project-euler-0185))
(in-package #:project-euler-0185)

;; ==============================================================================
;; 二諦随伴プロトコル: Ultimate Zero-Allocation Solver (All Prunings Integrated)
;; ==============================================================================

(declaim (optimize (speed 3) (safety 0) (debug 0)))

(defconstant +num-guesses+ 22)
(defconstant +seq-len+ 16)

;; --- ゼロ・アロケーションのためのグローバル型宣言 ---
(declaim (type (simple-array fixnum (22 16)) *guess-matrix*))
(declaim (type (simple-array fixnum (22)) *original-targets*))
(declaim (type (simple-array fixnum (22)) *targets*))
(declaim (type (simple-array fixnum (16)) *secret*))
(declaim (type (simple-array boolean (16 10)) *allowed-digits*))
(declaim (type (simple-array fixnum (16 10)) *col-scores*))
(declaim (type (simple-array fixnum (17)) *max-possible-scores*))

(defvar *guess-matrix* (make-array (list +num-guesses+ +seq-len+) :element-type 'fixnum))
(defvar *original-targets* (make-array +num-guesses+ :element-type 'fixnum))
(defvar *targets* (make-array +num-guesses+ :element-type 'fixnum))
(defvar *secret* (make-array +seq-len+ :element-type 'fixnum))
(defvar *allowed-digits* (make-array '(16 10) :element-type 'boolean :initial-element t))
(defvar *col-scores* (make-array '(16 10) :element-type 'fixnum :initial-element 0))
(defvar *max-possible-scores* (make-array 17 :element-type 'fixnum :initial-element 0))
(defvar *solution* nil)
(defvar *total-target-sum* 0)

(defparameter *guesses-data*
  '(("5616185650518293" . 2)  ("3847439647293047" . 1)
    ("5855462940810587" . 3)  ("9742855507068353" . 3)
    ("4296849643607543" . 3)  ("3174248439465858" . 1)
    ("4513559094146117" . 2)  ("7890971548908067" . 3)
    ("8157356344118483" . 1)  ("2615250744386899" . 2)
    ("8690095851526254" . 3)  ("6375711915077050" . 1)
    ("6913859173121360" . 1)  ("6442889055042768" . 2)
    ("2321386104303845" . 0)  ("2326509471271448" . 2)
    ("5251583379644322" . 2)  ("1748270476758276" . 3)
    ("4895722652190306" . 1)  ("3041631117224635" . 3)
    ("1841236454324589" . 3)  ("2659862637316867" . 2)))

(defun init-globals ()
  (setf *total-target-sum* 0)
  (loop for i from 0 below +num-guesses+
        for (str . tgt) in *guesses-data*
        do (setf (aref *original-targets* i) tgt)
           (incf *total-target-sum* tgt)
        do (loop for j from 0 below +seq-len+
                 do (setf (aref *guess-matrix* i j) (digit-char-p (char str j)))))
  
  (loop for c from 0 below +seq-len+
        do (loop for d from 0 to 9 do (setf (aref *allowed-digits* c d) t)))

  ;; ターゲット「0」の数字を禁止リストへ
  (loop for i from 0 below +num-guesses+
        when (= (aref *original-targets* i) 0)
        do (loop for c from 0 below +seq-len+
                 do (setf (aref *allowed-digits* c (aref *guess-matrix* i c)) nil)))
                 
  ;; グローバルスコア上限の事前計算
  (loop for c from 0 below +seq-len+
        do (loop for d from 0 to 9 do (setf (aref *col-scores* c d) 0)))
  (loop for i from 0 below +num-guesses+
        unless (= (aref *original-targets* i) 0)
        do (loop for c from 0 below +seq-len+
                 for d = (aref *guess-matrix* i c)
                 when (aref *allowed-digits* c d)
                 do (incf (aref *col-scores* c d))))
  (setf (aref *max-possible-scores* +seq-len+) 0)
  (loop for c from (1- +seq-len+) downto 0
        do (setf (aref *max-possible-scores* c)
                 (+ (aref *max-possible-scores* (1+ c))
                    (loop for d from 0 to 9 maximize (aref *col-scores* c d))))))

(defun solve-zero-alloc (c total-targets-left)
  (declare (type fixnum c total-targets-left))
  (when *solution* (return-from solve-zero-alloc t))
  
  ;; 【最強の枝刈り】 グローバルスコア制約
  ;; 残りの列で最大稼げるスコアを足しても目標に届かない場合は即座に枝刈り
  (when (> total-targets-left (aref *max-possible-scores* c))
    (return-from solve-zero-alloc nil))

  (if (= c +seq-len+)
      ;; 終了条件チェック
      (progn
        (loop for i from 0 below +num-guesses+
              unless (= (aref *targets* i) 0)
              do (return-from solve-zero-alloc nil))
        (setf *solution* (copy-seq *secret*))
        t)

      (let ((rem (- +seq-len+ c))
            (forced-guess -1))
        (declare (type fixnum rem forced-guess))

        ;; 枝刈りチェック（不足の否定 ＆ 現成の確定）
        (loop for i from 0 below +num-guesses+
              for t_g of-type fixnum = (aref *targets* i)
              do (cond
                   ((> t_g rem) (return-from solve-zero-alloc nil)) ; 絶対に足りない
                   ((= t_g rem) (setf forced-guess i))))            ; 残り全てが一致必須

        (if (>= forced-guess 0)
            ;; 【Forced Destiny (現成の確定)】
            (let ((valid t))
              (loop for i from c below +seq-len+
                    for d = (aref *guess-matrix* forced-guess i)
                    do (setf (aref *secret* i) d)
                       (unless (aref *allowed-digits* i d)
                         (setf valid nil) (return)))
              (when valid
                (loop for i from 0 below +num-guesses+
                      for needed = (aref *targets* i)
                      for matched = (loop for j from c below +seq-len+
                                          count (= (aref *secret* j) (aref *guess-matrix* i j)))
                      unless (= matched needed)
                      do (setf valid nil) (return)))
              (when valid
                (setf *solution* (copy-seq *secret*))
                (return-from solve-zero-alloc t))
              nil)

            ;; 【Normal DFS】
            (loop for d of-type fixnum from 0 to 9
                  when (aref *allowed-digits* c d)
                  do (let ((can-pick t)
                           (matched-count 0))
                       (declare (type fixnum matched-count))
                       
                       ;; 動的オーバーマッチの否定
                       (loop for i from 0 below +num-guesses+
                             when (= (aref *guess-matrix* i c) d)
                             do (if (= (aref *targets* i) 0)
                                    (progn (setf can-pick nil) (return))
                                    (incf matched-count)))

                       (when can-pick
                         (setf (aref *secret* c) d)
                         ;; 状態を進める (In-place Mutation)
                         (loop for i from 0 below +num-guesses+
                               when (= (aref *guess-matrix* i c) d)
                               do (decf (aref *targets* i)))

                         ;; 再帰 (獲得したスコアを引いて渡す)
                         (solve-zero-alloc (1+ c) (- total-targets-left matched-count))

                         ;; 状態を戻す (Zero-Allocation Backtracking)
                         (loop for i from 0 below +num-guesses+
                               when (= (aref *guess-matrix* i c) d)
                               do (incf (aref *targets* i)))))))))
  nil)

(defun project-euler-0185 ()
  (init-globals)
  (setf *solution* nil)
  (loop for i from 0 below +num-guesses+
        do (setf (aref *targets* i) (aref *original-targets* i)))
  
  (solve-zero-alloc 0 *total-target-sum*)
  
  (if *solution*
      (with-output-to-string (s)
        (loop for x across *solution* do (princ x s)))
      "No solution found."))

;; 実行
;; (project-euler-0185)


(time (print (project-euler-0185 )))
