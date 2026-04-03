;;; -*- mode: Lisp; coding: utf-8 -*-
;;; llm-model: gemini-3.1-pro
(cl:in-package cl-user)
(defpackage #:project-euler-0161 (:use cl iterate alexandria) (:export #:solve))
(in-package #:project-euler-0161)

(defmacro optimized-code-p (boole)
  (typecase boole
    (null nil)
    (T `(declaim (optimize (speed 3) (safety 0) (debug 0))))))

(optimized-code-p T)

#||
【数学的考察と次元崩壊の構築】
1. グリッドの特性と Profile DP（輪郭DP）への還元:
   9×12という「幅が非常に狭い」グリッドをタイルで埋める問題は、左上から右下へセル単位で走査し、
   「直近の境界（フロンティア）の凹凸状態」のみをビットマスクとして保持する Profile DP への次元崩壊が定石である。
   トリオミノは最大で縦に3マス（現在地から +2行）、横に3マス（+2列）の広がりを持つ。
   したがって、現在走査しているセルから最大 2W マス先までの空き状態を記憶しておけば、全ての干渉を判定できる。

2. 状態マスクの設計とビットシフト:
   W = 9 のとき、状態マスクは 2W = 18 ビットの整数に収まる（2^18 = 262,144 状態）。
   現在のセルを idx としたとき、各トリオミノの配置は idx からの相対オフセットで表現できる。
   - 横 I型: 0, 1, 2
   - 縦 I型: 0, W, 2W
   - L型1: 0, 1, W
   - L型2: 0, 1, W+1
   - L型3: 0, W, W+1
   - L型4: 0, W-1, W （※現在セルより左下に伸びるL字。W-1 オフセット）
   
3. アロケーションの完全排除 (O(0) Bytes Consed):
   DFSメモ化再帰やハッシュテーブルを用いた実装は、数千万回の関数呼び出しと Cons セルの生成により GC を誘発して破綻する。
   これを回避するため、サイズ 262,144 の 1次元配列（約 2MB）を `dp` と `next-dp` の 2本だけ静的確保する。
   各セルについて配列をスワップしながら 18bit マスクの遷移を in-place で加算していくことで、
   ループ内の動的アロケーションを「完全にゼロ」に抑え込み、純粋なキャッシュ・レジスタ演算へと昇華させる。
||#

(defun solve-for (W H)
  (let* ((num-cells (* W H))
         ;; マスクの最大値。W=9 なら 2^18 = 262144
         (max-mask (ash 1 (* 2 W)))
         ;; 状態配列は 64-bit 符号なし整数（最大 1.8 * 10^19 なのでオーバーフローしない）
         (dp (make-array max-mask :element-type '(unsigned-byte 64) :initial-element 0))
         (next-dp (make-array max-mask :element-type '(unsigned-byte 64) :initial-element 0))
         ;; 6種類のトリオミノのビットマスク事前計算
         (m-hi 7)                                     ; 1 | 2 | 4
         (m-vi (logior 1 (ash 1 W) (ash 1 (* 2 W))))  ; 1 | (1<<W) | (1<<2W)
         (m-l1 (logior 1 2 (ash 1 W)))                ; 1 | 2 | (1<<W)
         (m-l2 (logior 1 2 (ash 1 (1+ W))))           ; 1 | 2 | (1<<(W+1))
         (m-l3 (logior 1 (ash 1 W) (ash 1 (1+ W))))   ; 1 | (1<<W) | (1<<(W+1))
         (m-l4 (logior 1 (ash 1 (1- W)) (ash 1 W))))  ; 1 | (1<<(W-1)) | (1<<W)
    (declare (type fixnum num-cells max-mask m-hi m-vi m-l1 m-l2 m-l3 m-l4))
    
    ;; 初期状態: 何も配置していない状態の数は 1
    (setf (aref dp 0) 1)
    
    (iterate (for idx from 0 below num-cells)
      (let ((c (mod idx W))
            (r (truncate idx W)))
        (declare (type fixnum c r))
        
        ;; 次のセル用の DP 配列をゼロクリア (高速なメモリセット)
        (fill next-dp 0)
        
        (iterate (for mask from 0 below max-mask)
          (let ((ways (aref dp mask)))
            (declare (type (unsigned-byte 64) ways))
            (when (> ways 0)
              (if (not (zerop (logand mask 1)))
                  ;; 現在のセルが既に埋まっている場合は、そのまま右シフトして次へ
                  (incf (aref next-dp (ash mask -1)) ways)
                  ;; 現在のセルが空いている場合は、6種類のトリオミノを全て試す
                  (progn
                    ;; H-I (横棒)
                    (when (and (< (+ c 2) W) (zerop (logand mask m-hi)))
                      (incf (aref next-dp (ash (logior mask m-hi) -1)) ways))
                    ;; V-I (縦棒)
                    (when (and (< (+ r 2) H) (zerop (logand mask m-vi)))
                      (incf (aref next-dp (ash (logior mask m-vi) -1)) ways))
                    ;; L1 (「Г」の逆)
                    (when (and (< (+ c 1) W) (< (+ r 1) H) (zerop (logand mask m-l1)))
                      (incf (aref next-dp (ash (logior mask m-l1) -1)) ways))
                    ;; L2 (「Г」)
                    (when (and (< (+ c 1) W) (< (+ r 1) H) (zerop (logand mask m-l2)))
                      (incf (aref next-dp (ash (logior mask m-l2) -1)) ways))
                    ;; L3 (「L」)
                    (when (and (< (+ c 1) W) (< (+ r 1) H) (zerop (logand mask m-l3)))
                      (incf (aref next-dp (ash (logior mask m-l3) -1)) ways))
                    ;; L4 (「L」の逆、左下に広がるため c >= 1 の条件)
                    (when (and (>= c 1) (< (+ r 1) H) (zerop (logand mask m-l4)))
                      (incf (aref next-dp (ash (logior mask m-l4) -1)) ways)))))))
        ;; 配列のポインタをスワップして次のセルへ
        (rotatef dp next-dp)))
    ;; 最後のセルまで到達し、はみ出し（フロンティアのビット残り）が全く無い状態が答え
    (aref dp 0)))

(defun solve ()
  (format t "観測: テストケース T(9x2) を検証中...~%")
  (let ((ans-test (solve-for 9 2)))
    (format t "観測: T(9x2) = ~D (Expected: 41)~%" ans-test))
  
  (format t "観測: 本探索 T(9x12) を実行中...~%")
  (let ((ans (solve-for 9 12)))
    (format t "Answer: ~D~%" ans)
    ans))

#+| Do it | (project-euler-0161:solve)
#|------------------------------------------------------------|
Timing the evaluation of (solve)
観測: テストケース T(9x2) を検証中...
観測: T(9x2) = 41 (Expected: 41)
観測: 本探索 T(9x12) を実行中...
Answer: 20574308184277971

User time    =        0.264
System time  =        0.015
Elapsed time =        0.224
Allocation   = 8608456 bytes
3744 Page faults
GC time      =        0.000
 |------------------------------------------------------------|#
;;→ 20574308184277971
:ok