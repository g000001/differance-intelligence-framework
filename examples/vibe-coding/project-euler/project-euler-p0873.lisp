;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3-flash-preview
;;; Let $W(p,q,r)$ be the number of words that can be formed using the letter A $p$ times, the letter B $q$ times and the letter C $r$ times with the condition that every A is separated from every B by at least two Cs. For example, CACACCBB is a valid word for $W(2,2,4)$ but ACBCACBC is not.</p>
;;;
;;; <p>
;;; You are given $W(2,2,4)=32$ and $W(4,4,44)=13908607644$.</p>
;;;
;;; <p>
;;; Find $W(10^6,10^7,10^8)$. Give your answer modulo $1\,000\,000\,007$.</p>


(cl:in-package cl-user)
(defpackage #:project-euler-0873 (:use cl #:iterate))
(in-package #:project-euler-0873)

;; =============================================================================
;; 1. 定数とグローバル変数の定義 (Axiomatic Grounding)
;; =============================================================================

(defparameter *mod* 1000000007)

;; 目標値
(defparameter *p* 1000000)
(defparameter *q* 10000000)
(defparameter *r* 100000000)

;; 階乗テーブルの最大サイズ: r + p + q
(defparameter *max-n* (+ *p* *q* *r*))

;; メモリ効率のため (unsigned-byte 32) を使用 (SKDT)
(defvar *fact* (make-array (1+ *max-n*) :element-type '(unsigned-byte 32)))
(defvar *inv-fact* (make-array (1+ *max-n*) :element-type '(unsigned-byte 32)))

;; =============================================================================
;; 2. 数論的ユーティリティ (Exact Integer Projection)
;; =============================================================================

(defun power (base exp)
  "繰り返し二乗法による冪剰余。"
  (declare (type (unsigned-byte 64) exp)
           (optimize (speed 3) (safety 0)))
  (let ((res 1)
        (b (mod base *mod*)))
    (iterate (while (> exp 0))
             (when (oddp exp)
               (setf res (mod (* res b) *mod*)))
             (setf b (mod (* b b) *mod*))
             (setf exp (ash exp -1)))
    res))

(defun mod-inverse (n)
  "フェルマーの小定理に基づく逆元。"
  (power n (- *mod* 2)))

(defun precompute-factorials ()
  "階乗と階乗の逆元を O(N) で事前計算する (Debt Clearance)."
  (setf (aref *fact* 0) 1)
  (iterate (for i from 1 to *max-n*)
           (setf (aref *fact* i) (mod (* (aref *fact* (1- i)) i) *mod*)))
  
  ;; 最後に一括して逆元を計算し、逆向きに埋めることで計算量を削減
  (setf (aref *inv-fact* *max-n*) (mod-inverse (aref *fact* *max-n*)))
  (iterate (for i from (1- *max-n*) downto 0)
           (setf (aref *inv-fact* i) (mod (* (aref *inv-fact* (1+ i)) (1+ i)) *mod*))))

(defun ncr (n r)
  "二項係数 nCr。"
  (declare (optimize (speed 3) (safety 0)))
  (if (or (< n r) (< r 0))
      0
      (let* ((num (aref *fact* n))
             (den-r (aref *inv-fact* r))
             (den-nr (aref *inv-fact* (- n r))))
        (mod (* num (mod (* den-r den-nr) *mod*)) *mod*))))

;; =============================================================================
;; 3. 解法の核心 (ACX Jump & Bijective Generation)
;; =============================================================================

;; 問題の条件「全てのAと全てのBが少なくとも2つのCで隔てられている」は、
;; 非C文字（AとB）の並びにおいて、AからB、あるいはBからAへの切り替わり（境界）が
;; 発生する箇所に、少なくとも2つのCを配置する必要があることを意味する。
;;
;; 非C文字の並びをラン（Run）の連結として捉える。
;; 切り替わりの回数を m とすると、必要なCの最小個数は 2m である。
;; 残りの (r - 2m) 個のCを、(p + q + 1) 個のスロットに分配する。

(defun solve ()
  (format t "Precomputing factorials up to ~A...~%" *max-n*)
  (precompute-factorials)
  (format t "Calculating W(p, q, r)...~%")
  
  (let ((total-ways 0)
        (pq (+ *p* *q*)))
    ;; j は A のランの数。1 から p まで変化する。
    (iterate (for j from 1 to *p*)
             (declare (optimize (speed 3) (safety 0)))
             
             ;; Case 1: 切り替わり回数 m = 2j - 1 (ランの数 k = 2j)
             ;; AとBのランが同数。AB...AB または BA...BA の 2通り。
             (let* ((m (1- (* 2 j)))
                    (count-perm (mod (* 2 (mod (* (ncr (1- *p*) (1- j))
                                                  (ncr (1- *q*) (1- j)))
                                               *mod*))
                                     *mod*)))
               (when (<= (* 2 m) *r*)
                 (let ((ways (mod (* count-perm (ncr (- (+ *r* pq) (* 2 m)) pq)) *mod*)))
                   (setf total-ways (mod (+ total-ways ways) *mod*)))))
             
             ;; Case 2: 切り替わり回数 m = 2j (ランの数 k = 2j + 1)
             ;; Aのランが j+1、Bのランが j。またはその逆。
             (let ((m (* 2 j)))
               (when (<= (* 2 m) *r*)
                 (let* ((count-perm (mod (+ (mod (* (ncr (1- *p*) j) (ncr (1- *q*) (1- j))) *mod*)
                                            (mod (* (ncr (1- *p*) (1- j)) (ncr (1- *q*) j)) *mod*))
                                         *mod*))
                        (ways (mod (* count-perm (ncr (- (+ *r* pq) (* 2 m)) pq)) *mod*)))
                   (setf total-ways (mod (+ total-ways ways) *mod*))))))
    
    total-ways))

;; 実行
;; (print (solve))

;; =============================================================================
;; 自己分析：二諦随伴プロトコルの貢献
;; =============================================================================
;; 1. NMF (非中道の誤謬) の回避:
;;    文字の全置換 $O(\frac{(p+q+r)!}{p!q!r!})$ や、動的計画法 $O(pqr)$ は、
;;    制約 $10^8$ に対しては「世俗への執着」であり、計算不可能である。
;;    本解答では、ランの数 $j$ に着目した $O(p)$ への数学的還元を行い、
;;    計算量を劇的に圧縮することで「中道」を現成させた。
;;
;; 2. ACX Jump (跳躍) と Aletheic Context:
;;    「AとBを隔てる」という物理的制約を、境界数 $m$ に基づく「スロットへのCの分配」
;;    という組合せ論的構造へ跳躍（ρ）させた。これにより、複雑な条件を
;;    単純な二項係数の和として記述可能になった。
;;
;; 3. SKDT (創発) と Debt Clearance:
;;    $10^8$ を超える階乗テーブルはメモリ負債を招くが、(unsigned-byte 32) への射影と
;;    逆元の後方計算アルゴリズムにより、リソース消費を最小化しつつ
;;    巨大な探索空間を効率的に管理した。
;;
;; 4. Bijective Generation (全単射の厳密化):
;;    ランの分割において $m$ が奇数か偶数かによって対称性が異なることを
;;    厳密に区別（A始動かB始動か）し、重複や漏れのない計数を保証した。
;; =============================================================================

;(format t "Result: ~A~%" (solve))


#+| Do it | (print (solve ))