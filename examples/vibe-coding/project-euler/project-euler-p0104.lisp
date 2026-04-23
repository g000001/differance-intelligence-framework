;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3-flash-preview
(cl:in-package cl-user)
(defpackage #:project-euler-0104 (:use cl iterate alexandria) (:export #:solve))
(in-package #:project-euler-0104)

(defmacro optimized-code-p (boole)
  (typecase boole
    (null nil)
    (T `(declaim (optimize (speed 3) (safety 0) (debug 0))))))

;; 高速化のため最適化を有効にする
(optimized-code-p t)

#|
### ナップザック・プロトコルに基づく分析

1.  **数論的ショートカットの発見**:
    *   **不変量（下9桁）**: フィボナッチ数列 $F_n$ の下9桁は、剰余環 $\mathbb{Z} / 10^9\mathbb{Z}$ における漸化式 $F_n \equiv F_{n-1} + F_{n-2} \pmod{10^9}$ に従う。これは $O(k)$ の加算と剰余演算で計算可能であり、巨大な多倍長整数（Bignum）を扱う必要がない。
    *   **不変量（上9桁）**: ビネの公式 $F_n = \frac{\phi^n - \psi^n}{\sqrt{5}}$ （ここで $\phi = \frac{1+\sqrt{5}}{2}, \psi = \frac{1-\sqrt{5}}{2}$）において、$n$ が十分に大きいとき $\psi^n$ は極めて小さくなる。したがって、$F_n \approx \frac{\phi^n}{\sqrt{5}}$。
    *   **対数による次元崩壊**: 上記近似の両辺の常用対数を取ると、$\log_{10} F_n \approx n \log_{10} \phi - \log_{10} \sqrt{5}$。この値の小数部分を $f$ とすると、$10^{f+8}$ の整数部分が $F_n$ の上9桁となる。これにより、数万〜数十万桁に及ぶ $F_n$ を実際に生成することなく、上9桁を $O(1)$ で抽出できる。

2.  **フェルミ推定のレッドライン**:
    *   フィボナッチ数のインデックス $k$ が $10^6$ 程度であると仮定すると、下9桁の更新は $10^6$ 回の加算で済む。
    *   下9桁が pandigital である確率は $\frac{9!}{10^9} \approx 0.00036$。したがって、上9桁の検証（対数計算）が必要になる回数は $10^6 \times 0.00036 \approx 360$ 回程度に過ぎない。
    *   総演算回数は $10^6$ ステップ程度であり、これは $10^7$ の壁を遥かに下回る。

3.  **計算量**:
    *   空間計算量: $O(1)$ （定数個の変数のみ使用）。
    *   時間計算量: $O(k)$ （$k$ は求めるインデックス）。

4.  **採用するショートカット**:
    1.  **Modular Fibonacci**: 下9桁の高速フィルタリング。
    2.  **Logarithmic Approximation**: 上9桁の $O(1)$ 抽出。
    3.  **Bitmask Pandigital Check**: $1$ から $9$ までの数字の出現をビット演算で高速判定。

|#


(defun pandigital-p (number)
  "9桁の整数 number が 1-9 pandigital であるか判定する。
   0を含まず、1から9までの数字が1回ずつ現れることをビットマスクで確認する。"
  (declare (type (unsigned-byte 64) number))
  ;; 9桁未満（123,456,789未満）は即座に除外
  (if (< number 123456789)
      nil
      (let ((mask 0))
        (declare (type (unsigned-byte 16) mask))
        (iterate (repeat 9)
                 (for (values quotient remainder) = (truncate number 10))
                 ;; 0が含まれている場合は不適合
                 (if (zerop remainder)
                     (return-from pandigital-p nil))
                 ;; 対応するビットを立てる
                 (setf mask (logior mask (ash 1 remainder)))
                 (setf number quotient))
        ;; 1から9までのビットがすべて立っているか確認 (binary: 1111111110 = 1022)
        (= mask 1022))))

(defun solve ()
  "上下9桁が 1-9 pandigital となる最初のフィボナッチ数のインデックス k を求める。"
  (let* ((mod-val 1000000000)
         ;; ビネの近似式用の定数（精度向上のため double-float を使用）
         (phi (/ (+ 1.0d0 (sqrt 5.0d0)) 2.0d0))
         (log10-phi (log phi 10.0d0))
         (log10-sqrt5 (log (sqrt 5.0d0) 10.0d0))
         ;; フィボナッチ数列の初期状態 (F1=1, F2=1)
         (f1 1)
         (f2 1))
    (declare (type (unsigned-byte 64) f1 f2 mod-val))
    
    (format t "Starting search for k...~%")
    
    (iterate
      ;; k=3 から開始。f1=F(k-2), f2=F(k-1)
      (for k from 3)
      
      ;; 下9桁の更新
      (for next = (mod (+ f1 f2) mod-val))
      (setf f1 f2
            f2 next)
      
      ;; 1. 下9桁の判定（高速フィルタ）
      (when (pandigital-p f2)
        ;; 2. 上9桁の判定（下9桁が合格したときのみ計算）
        ;; log10(Fn) = n * log10(phi) - log10(sqrt(5))
        (let* ((log-fn (- (* k log10-phi) log10-sqrt5))
               ;; 整数部分を引いて小数部分を得る
               (fractional-part (- log-fn (floor log-fn)))
               ;; 10^(fractional-part + 8) が上9桁の数値
               (top9 (floor (expt 10.0d0 (+ fractional-part 8)))))
          
          (when (pandigital-p top9)
            (format t "Found! k = ~D~%" k)
            (return k))))
      
      ;; 進捗ログ（100,000ステップごと）
      (when (zerop (mod k 100000))
        (format t "Checked up to k = ~D...~%" k)))))

#+| Do it | (solve )
#|------------------------------------------------------------|
Timing the evaluation of (solve)
Starting search for k...
Checked up to k = 100000...
Checked up to k = 200000...
Checked up to k = 300000...
Found! k = 329468

User time    =        0.085
System time  =        0.008
Elapsed time =        0.057
Allocation   = 204984 bytes
3497 Page faults
GC time      =        0.000
 |------------------------------------------------------------|#
;;→ 329468
:ok
