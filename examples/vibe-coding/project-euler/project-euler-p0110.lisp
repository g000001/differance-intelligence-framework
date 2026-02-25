
(defpackage :euler
  (:use :cl)
  (:export :solve))

(in-package :euler)

(defvar *primes* nil) ; 素数のリスト
(defvar *min-n* most-positive-fixnum) ; 見つかった最小の n
(defvar *min-div-count* 8000001) ; d(n^2) の目標値 (8,000,001以上)

(defun make-primes (limit)
  "指定された制限までの素数をエラトステネスの篩で生成します。"
  (let ((sieve (make-array (1+ limit) :element-type 'boolean :initial-element nil)))
    (loop for i from 2 to limit
          when (not (svref sieve i))
            do (loop for j from (* i i) to limit by i
                     do (setf (svref sieve j) t)))
    (loop for i from 2 to limit
          when (not (svref sieve i))
            collect i)))

(defun search-exponents (prime-idx current-n current-div-count max-e)
  "n を最小化するための指数を探索する再帰関数。
   prime-idx: *primes* リストにおける現在の素数のインデックス。
   current-n: これまでに構築された n の値。
   current-div-count: これまでに構築された d(n^2) の値。
   max-e: 現在の素数に許容される最大の指数 (前の素数の指数以下でなければならない)。"

  ;; 枝刈り1: もし現在の n が既に見つかっている最小の n よりも大きければ、この経路は探索しない。
  (when (> current-n *min-n*)
    (return-from search-exponents))

  ;; 枝刈り2: もし d(n^2) が目標値以上であれば、現在の n は候補となる。
  ;; これ以上素因数を追加しても n は大きくなるだけなので、現在の n を最小値として更新して終了。
  (when (>= current-div-count *min-div-count*)
    (setf *min-n* (min *min-n* current-n))
    (return-from search-exponents))

  ;; ベースケース: もし素数のリストを使い果たしたら、この経路は探索終了。
  (when (>= prime-idx (length *primes*))
    (return-from search-exponents))

  (let ((p (nth prime-idx *primes*)))
    ;; 現在の素数 p の指数 e を探索するループ。
    ;; e は 1 から max-e まで (max-e は前の素数の指数)。
    ;; 指数は降順で探索することで、e_1 >= e_2 >= ... の条件を満たす。
    (loop for e from max-e downto 1
          for new-n = (* current-n (expt p e)) ; 新しい n の値
          for new-div-factor = (+ (* 2 e) 1) ; d(n^2) の新しい因数
          for new-div-count = (* current-div-count new-div-factor) ; 新しい d(n^2) の値
          do
             ;; 次の素数で再帰呼び出し。現在の指数 e が次の max-e となる。
             (search-exponents (1+ prime-idx) new-n new-div-count e))))

(defun solve ()
  "問題の解を計算します。"
  ;; 100 までの素数で十分。なぜなら、d(n^2) >= 8,000,001 を満たすには、
  ;; 最低でも15個の素因数 (全ての指数が1の場合、d(n^2) = 3^15 = 14348907) が必要であり、
  ;; 15番目の素数は 47 だから。
  (setf *primes* (make-primes 100))
  (setf *min-n* most-positive-fixnum) ; 最小の n を初期化

  ;; 初期の再帰呼び出し。
  ;; prime-idx: 0 (素数 2 から開始)
  ;; current-n: 1 (n=1 から構築開始)
  ;; current-div-count: 1 (d(1^2) = 1)
  ;; max-e: 最初の素数 (2) の最大指数。
  ;; 経験的に、解となる n は複数の小さい素因数を持つため、
  ;; 最初の素数の指数は極端に大きくならない。
  ;; 例えば、n が最初の15個の素数の積 (2*3*...*47) の場合、d(n^2) = 3^15。
  ;; この n は約 1.3 * 10^11 で、log_2(1.3 * 10^11) は約 37 なので、
  ;; 最初の素数の指数は最大で 37 程度までを考慮すれば十分と推測できる。
  ;; 安全のため 40 を設定。
  (search-exponents 0 1 1 40)
  *min-n*)

#+| Do it | (solve )
;→ 9350130049860600
