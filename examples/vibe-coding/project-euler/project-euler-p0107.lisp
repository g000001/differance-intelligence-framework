;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-1.5-pro
(cl:in-package cl-user)
(defpackage #:project-euler-0107 (:use cl iterate alexandria) (:export #:solve))
(in-package #:project-euler-0107)

(defmacro optimized-code-p (boole)
  (typecase boole
    (null nil)
    (T `(declaim (optimize (speed 3) (safety 0) (debug 0) #+lispworks (hcl:fixnum-safety 0))))))

(optimized-code-p t)

#||
(cl-comment "=== 6. Exact Integer Projection (勝義的整数化による浮動小数点の排除) ===")
(cl-comment "グラフの重みはすべて整数であり、最小全域木（MST）の計算においても整数演算のみで完結する。")
(cl-comment "浮動小数点による誤差の介入を完全に排除し、純粋な整数比較と加算のみで解を導出する。")

(cl-comment "=== 7. Bijective Generation (対称性の破れと一意生成の厳密化) ===")
(cl-comment "隣接行列は対称行列であるため、全エッジの重みの総和を求める際、上三角成分のみを抽出することで二重計上を防止する。")
(cl-comment "これにより、入力データからグラフの全エッジ集合への全単射な写像を保証する。")

(cl-comment "=== 8. Verification against Emptiness (境界値における自己検算) ===")
(cl-comment "問題文の7頂点の例（Total: 243, MST: 93, Saving: 150）をテストケースとして想定し、")
(cl-comment "アルゴリズムが150を導出することを論理的に確認済みである。")

(cl-comment "=== 9. Axiomatic Grounding (公理的定礎と幻覚の超克) ===")
(cl-comment "最小全域木の構成問題はクラスカル法（Kruskal's algorithm）により最適性が保証されている。")
(cl-comment "計算量は O(E log E) であり、40頂点（最大780エッジ）の制約下ではフェルミ推定を待つまでもなく")
(cl-comment "1ミリ秒未満で終了する計算規模である。")

||#

(defun split-at-comma (string)
  "文字列をカンマで分割し、リストとして返す。"
  (let ((length (length string)))
    (iterate
      (for start initially 0 then (1+ pos))
      (for pos = (position #\, string :start start))
      (collect (subseq string start (or pos length)))
      (while pos))))

(defun find-root (parent-vector node-index)
  "経路圧縮を用いたUnion-FindのFind操作。"
  (let ((parent (aref parent-vector node-index)))
    (if (= parent node-index)
        node-index
        (setf (aref parent-vector node-index)
              (find-root parent-vector parent)))))

(defun union-sets (parent-vector root-1 root-2)
  "Union-FindのUnion操作（単純な併合）。"
  (setf (aref parent-vector root-1) root-2))

(defun solve (&optional (filename "/tmp/0107_network.txt"))
  "クラスカル法を用いて最小全域木を求め、節約された重みの合計を計算する。"
  (format t "Reading file: ~A~%" filename)
  (let ((all-edges '())
        (total-initial-weight 0)
        (vertex-count 0))
    ;; データの読み込みとパース
    (with-open-file (stream filename :if-does-not-exist :error)
      (iterate
        (for line = (read-line stream nil))
        (while line)
        (for row-index from 0)
        (setf vertex-count (1+ row-index))
        (iterate
          (for col-value in-sequence (split-at-comma line))
          (for col-index from 0)
          (unless (string= col-value "-")
            (let ((weight (parse-integer col-value)))
              ;; 無向グラフのため、上三角成分のみを処理して二重計上を防ぐ
              (when (< row-index col-index)
                (incf total-initial-weight weight)
                (push (list weight row-index col-index) all-edges)))))))

    (format t "Vertices: ~A, Edges: ~A, Total Weight: ~A~%" 
            vertex-count (length all-edges) total-initial-weight)

    ;; クラスカル法
    ;; 1. エッジを重みの昇順にソート
    (let ((sorted-edges (sort (copy-list all-edges) #'< :key #'first))
          (parent-vector (make-array vertex-count :element-type 'fixnum))
          (mst-weight 0)
          (edges-count-in-mst 0))
      
      ;; Union-Find構造の初期化
      (iterate (for i from 0 below vertex-count)
               (setf (aref parent-vector i) i))

      ;; 2. 重みの小さい順にエッジを走査
      (iterate (for (weight u v) in-sequence sorted-edges)
               (let ((root-u (find-root parent-vector u))
                     (root-v (find-root parent-vector v)))
                 ;; 閉路を形成しない（異なる連結成分に属する）場合のみ採用
                 (when (/= root-u root-v)
                   (incf mst-weight weight)
                   (incf edges-count-in-mst)
                   (union-sets parent-vector root-u root-v))))

      (let ((saving (- total-initial-weight mst-weight)))
        (format t "MST Weight: ~A~%" mst-weight)
        (format t "Edges in MST: ~A~%" edges-count-in-mst)
        (format t "Maximum Saving: ~A~%" saving)
        saving))))

;; フェルミ推定と計算量の証明:
;; 頂点数 V = 40, 最大エッジ数 E = V(V-1)/2 = 780.
;; 1. パース: O(V^2) = 1600 回の文字列処理。
;; 2. ソート: O(E log E) = 780 * log2(780) ≒ 7800 回の比較。
;; 3. Union-Find: O(E α(V)) ≒ 780 回のほぼ定数時間操作。
;; 総演算回数は 10^4 オーダーであり、10^7 のレッドラインに対して 1000 倍以上の余裕がある。
;; Common Lisp のネイティブコンパイル環境下では、マイクロ秒単位での実行が期待される。

#+| Do it | (solve )
#|------------------------------------------------------------|
Timing the evaluation of (solve)
Reading file: /tmp/0107_network.txt
Vertices: 40, Edges: 513, Total Weight: 261832
MST Weight: 2153
Edges in MST: 39
Maximum Saving: 259679

User time    =        0.002
System time  =        0.000
Elapsed time =        0.001
Allocation   = 223960 bytes
11 Page faults
GC time      =        0.000
 |------------------------------------------------------------|#
;;→ 259679
:ok