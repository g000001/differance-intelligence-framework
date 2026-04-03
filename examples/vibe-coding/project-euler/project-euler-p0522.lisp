;;; -*- mode: Lisp; coding: utf-8 -*-
;;; llm-model: gemini-3.1-pro
(cl:in-package cl-user)
(defpackage #:project-euler-0522 (:use cl iterate alexandria cffi) (:export #:solve))
(in-package #:project-euler-0522)

(defmacro source-pathname ()
  `,(or *compile-file-truename* *load-truename*))

#||
【数学的考察と次元崩壊の構築】
1. 問題の抽象化（関数グラフの単一サイクル化）:
   各階から別の1つの階に電力を送る配線は、自己ループを持たない「関数グラフ (Functional Graph)」を形成します。
   このグラフにジェネレータを置いたときに全階に電力が届くための必要十分条件は、グラフ全体が「1つの長さ n のサイクル」になることです。
   関数グラフを単一サイクルにするための最小辺変更数 $M(f)$ は、グラフの「入次数0の頂点（葉）の数 $L(f)$」と「葉を持たない純粋なサイクル成分の数 $C_0(f)$」を用いて、以下のように表されます。
   $M(f) = L(f) + C_0(f)$
   （※ただし、最初から単一サイクルの場合のみ $M(f) = 0$ となり、上式だと $0 + 1 = 1$ になってしまうため、全パターンから単一サイクルの数 $(n-1)!$ を引くことで相殺します）

2. O(N) への次元崩壊:
   $F(n) = \sum_{f} L(f) + \sum_{f} C_0(f) - (n-1)! \pmod M$
   
   - $\sum_{f} L(f)$ : 任意の頂点が入次数0になる組み合わせは独立して数えられ、$n(n-1)(n-2)^{n-1}$ 通りとなります。
   - $\sum_{f} C_0(f)$ : 長さ $k$ の純粋なサイクルを選ぶ方法は $\binom{n}{k}(k-1)!$ 通り。残りの $n-k$ 頂点が内部で完結する関数グラフの数は $A(n-k) = (n-k-1)^{n-k}$ 通り。
   したがって、$\sum C_0(f) = \sum_{k=2}^n \frac{n!}{k(n-k)!} A(n-k)$ となります。
   
   ここで $k=n$ の項がちょうど $(n-1)!$ となるため、末尾の $-(n-1)!$ と完全に相殺され、総和の範囲は $k=2 \dots n-2$ に縮退します。

3. C言語によるオフロードとフェルミ推定の突破:
   $N \approx 1.23 \times 10^7$ という制約に対し、LispのBignumオブジェクトを数千万回生成するとGCが致命的なボトルネックになります。
   そこで、この $\mathcal{O}(N)$ のループ演算をピュアなC言語にオフロードします。
   モジュラ逆元は $\mathcal{O}(N)$ の線形時間アルゴリズムで事前に一括計算（約48MBの配列確保）し、ループ内部はレジスタ上の乗算と $\mathcal{O}(\log N)$ の二乗法のみに留めます。総演算回数は約 $3 \times 10^8$ 回となり、Cの最適化によって1秒未満で完結します。
||#

;; C言語のソルバーコード
(defparameter *c-source* "
#include <stdint.h>
#include <stdlib.h>

uint64_t power_mod(uint64_t base, uint64_t exp, uint64_t mod) {
    uint64_t res = 1;
    base %= mod;
    while (exp > 0) {
        if (exp % 2 == 1) res = (res * base) % mod;
        base = (base * base) % mod;
        exp /= 2;
    }
    return res;
}

uint64_t solve_522(uint64_t n, uint64_t M) {
    // 葉の総和の寄与
    uint64_t L_sum = (n * (n - 1)) % M;
    L_sum = (L_sum * power_mod(n - 2, n - 1, M)) % M;
    
    uint64_t sum_C0 = 0;
    uint64_t P_k = n;
    
    // O(N) 線形時間モジュラ逆元テーブルの構築 (Mが素数であることを前提)
    uint32_t *inv = (uint32_t *)malloc((n + 1) * sizeof(uint32_t));
    if (!inv) return 0;
    inv[1] = 1;
    for (uint32_t i = 2; i <= n; i++) {
        inv[i] = (uint64_t)(M - M / i) * inv[M % i] % M;
    }
    
    // k = 2 ... n-2 までの和
    for (uint64_t k = 2; k <= n - 2; k++) {
        P_k = (P_k * (n - k + 1)) % M;
        uint64_t inv_k = inv[k];
        
        uint64_t m = n - k;
        uint64_t A_m = power_mod(m - 1, m, M);
        
        uint64_t term = (P_k * inv_k) % M;
        term = (term * A_m) % M;
        
        sum_C0 = (sum_C0 + term) % M;
    }
    
    free(inv);
    
    return (L_sum + sum_C0) % M;
}
")

(defun compile-and-load-c-code ()
  (let* ((c-file (uiop:native-namestring (merge-pathnames "pe522_solver.c"
                                                          (source-pathname))))
         (so-file (uiop:native-namestring (merge-pathnames 
                                            #-windows "libpe522_solver.so"
                                            #+windows "libpe522_solver.dll"
                                            (source-pathname)))))
    ;; Cのソースコードをカレントディレクトリに書き出し
    (with-open-file (out c-file :direction :output :if-exists :supersede)
      (write-string *c-source* out))
    ;; GCCで共有ライブラリとしてコンパイル
    (uiop:run-program (format nil "gcc -O3 -shared -fPIC -o ~A ~A" so-file c-file)
                      :output *standard-output* :error-output *error-output*)
    ;; CFFIでロード
    (load-foreign-library so-file)))

;; C関数のバインディング
(defcfun ("solve_522" c-solve-522) :uint64
  (n :uint64)
  (m :uint64))

(defun solve ()
  (format t "観測: C共有ライブラリをコンパイルおよびロード中...~%")
  (compile-and-load-c-code)
  
  (format t "観測: テストケース T(3) を検証中...~%")
  (let ((ans-test (c-solve-522 3 135707531)))
    (format t "観測: T(3) = ~D (Expected: 6)~%" ans-test))
    
  (format t "観測: テストケース T(8) を検証中...~%")
  (let ((ans-test2 (c-solve-522 8 135707531)))
    (format t "観測: T(8) = ~D (Expected: 16276736)~%" ans-test2))
    
  (format t "観測: テストケース T(100) を検証中...~%")
  (let ((ans-test3 (c-solve-522 100 135707531)))
    (format t "観測: T(100) = ~D (Expected: 84326147)~%" ans-test3))

  (format t "観測: 本探索 T(12344321) を実行中...~%")
  (let ((ans (c-solve-522 12344321 135707531)))
    (format t "Answer: ~D~%" ans)
    ans))

#+| Do it | (project-euler-0522:solve)
#|------------------------------------------------------------|
Timing the evaluation of (solve)
観測: C共有ライブラリをコンパイルおよびロード中...
観測: テストケース T(3) を検証中...
観測: T(3) = 6 (Expected: 6)
観測: テストケース T(8) を検証中...
観測: T(8) = 16276736 (Expected: 16276736)
観測: テストケース T(100) を検証中...
観測: T(100) = 84326147 (Expected: 84326147)
観測: 本探索 T(12344321) を実行中...
Answer: 96772715

User time    =        3.403
System time  =        0.069
Elapsed time =        3.723
Allocation   = 971656 bytes
18010 Page faults
GC time      =        0.000
 |------------------------------------------------------------|#
;;→ 96772715
:ok
