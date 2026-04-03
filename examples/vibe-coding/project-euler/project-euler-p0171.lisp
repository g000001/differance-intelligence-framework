;;; -*- mode: Lisp; coding: utf-8 -*-
;;; llm-model: gemini-3.1-pro
(cl:in-package cl-user)
(defpackage #:project-euler-0171 (:use cl iterate alexandria) (:export #:solve))
(in-package #:project-euler-0171)

(defmacro source-pathname ()
  "Compute source pathname"
  `(load-time-value (or *compile-file-truename* *load-truename* (uiop:getcwd))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :cffi :silent t))

#||
【数学的考察と次元崩壊の構築】
1. 問題の抽象化（桁DPへの帰着）:
   $f(n)$ は各桁の2乗和です。$0 < n < 10^{20}$ における最大の $f(n)$ は、9が20個並んだ場合の $9^2 \times 20 = 1620$ に過ぎません。
   したがって、$f(n)$ が完全平方数となる条件は、$f(n) \in \{1^2, 2^2, 3^2, \dots, 40^2\}$ に限定されます。

2. 計算量のフェルミ推定とレッドラインの突破:
   20桁の数を生成する全ての組み合わせ $10^{20}$ を全探索することは不可能ですが、
   「現在の桁数 $i$ (0〜20)」と「ここまでの2乗和 $s$ (0〜1620)」を状態とする桁DP (Digit DP) に帰着させれば、
   状態数はわずか $20 \times 1620 \approx 32,400$ になります。
   各状態からの遷移は $0$ から $9$ の数字を付加する $10$ パターンのみであるため、
   最悪ケースの総演算回数は約 $3.2 \times 10^5$ 回に収まります。
   これはフェルミ推定のレッドライン（$10^7$ 回）を圧倒的に下回っており、純粋な $\mathcal{O}(L \cdot \text{max\_f} \cdot 10)$ への次元崩壊が達成されます。

3. モジュロ演算とCへのオフロード:
   値の和は $10^9$ を法として求めます（下9桁）。状態の数も $10^9$ を法として管理します。
   中間演算で最大 $10^9 \times 10^9 = 10^{18}$ が発生しますが、これは C言語の 64-bit 符号なし整数（`uint64_t`、上限 $\approx 1.8 \times 10^{19}$）の器にすっぽり収まるため、安全かつ高速にインプレース計算が可能です。
||#

(defparameter *c-source* "
#include <stdint.h>
#include <stdlib.h>

uint64_t solve_171_core(int digits) {
    // DP arrays: count[i][s] と sum[i][s]
    // i: 処理した桁数 (0 to 20)
    // s: 桁の2乗和 (0 to 1620)
    static uint64_t count[21][1621] = {0};
    static uint64_t sum[21][1621] = {0};
    
    // DP初期化
    for (int i = 0; i <= digits; i++) {
        for (int s = 0; s <= 1620; s++) {
            count[i][s] = 0;
            sum[i][s] = 0;
        }
    }
    
    count[0][0] = 1;
    uint64_t p10 = 1; // 10^i mod 10^9
    
    for (int i = 0; i < digits; i++) {
        for (int s = 0; s <= 1620; s++) {
            if (count[i][s] == 0) continue;
            
            for (int d = 0; d <= 9; d++) {
                int ns = s + d * d;
                if (ns > 1620) continue; // 最大値クリップ
                
                // パターン数の遷移
                count[i+1][ns] = (count[i+1][ns] + count[i][s]) % 1000000000ULL;
                
                // 和の遷移
                // 新しい桁 d は 10^i の位になるため、d * 10^i をパターンの数だけ足す
                uint64_t term = (d * p10) % 1000000000ULL;
                term = (term * count[i][s]) % 1000000000ULL;
                
                sum[i+1][ns] = (sum[i+1][ns] + sum[i][s] + term) % 1000000000ULL;
            }
        }
        p10 = (p10 * 10) % 1000000000ULL;
    }
    
    uint64_t ans = 0;
    // 2乗和が完全平方数になるものの和を合算
    for (int k = 1; k * k <= 1620; k++) {
        ans = (ans + sum[digits][k * k]) % 1000000000ULL;
    }
    
    return ans;
}
")

(defun compile-and-load-c-code ()
  (let* ((c-file (uiop:native-namestring (merge-pathnames "pe171_solver.c" (source-pathname))))
         (so-file (uiop:native-namestring (merge-pathnames 
                                            #-windows "libpe171_solver.so"
                                            #+windows "libpe171_solver.dll"
                                            (source-pathname)))))
    (with-open-file (out c-file :direction :output :if-exists :supersede)
      (write-string *c-source* out))
    (uiop:run-program (format nil "gcc -O3 -shared -fPIC -o ~A ~A" so-file c-file)
                      :output *standard-output* :error-output *error-output*)
    (cffi:load-foreign-library so-file)))

(cffi:defcfun ("solve_171_core" c-solve-171) :uint64
  (digits :int))

(defun solve ()
  (format t "観測: C共有ライブラリをコンパイルおよびロード中...~%")
  (compile-and-load-c-code)
  
  (format t "観測: テストケース T(2) [2桁での検証]~%")
  ;; 1〜99 で f(n) が平方数なのは:
  ;; 1(1), 2(4), 3(9), 4(16), 5(25), 8(64)
  ;; 10(1), 20(4), 30(9), 40(16), 50(25), 80(64)
  ;; 34, 43, 68, 86 等... これらを瞬時にC層で解く
  (let ((ans-test (c-solve-171 2)))
    (format t "観測: T(2) = ~D~%" ans-test))

  (format t "観測: 本探索 T(20) を実行中...~%")
  (let ((ans (c-solve-171 20)))
    ;; 下9桁が要求されているため、ゼロパディングを含めて出力
    (format t "Answer: ~9,'0D~%" ans)
    ans))

#+| Do it | (project-euler-0171:solve)
#|------------------------------------------------------------|
Timing the evaluation of (solve)
観測: C共有ライブラリをコンパイルおよびロード中...
観測: テストケース T(2) [2桁での検証]
観測: T(2) = 726
観測: 本探索 T(20) を実行中...
Answer: 142989277

User time    =        0.051
System time  =        0.030
Elapsed time =        0.144
Allocation   = 688896 bytes
5747 Page faults
GC time      =        0.000
 |------------------------------------------------------------|#
;;→ 142989277
:ok