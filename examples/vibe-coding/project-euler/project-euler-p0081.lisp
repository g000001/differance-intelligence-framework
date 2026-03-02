;;; -*-  mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3-flash-preview
(cl:in-package cl-user)
(defpackage #:project-euler-0081 (:use #:cl #:alexandria #:iterate))
(in-package #:project-euler-0081)

#||
(cl-text project-euler-p81-analysis
  (cl-comment "二諦随伴（Two-Truths Entanglement）によるP81の分析")

  ;; 1. 世俗諦 (Conventional Truth): 行列データと制約
  (forall (i j)
    (implies (and (MatrixIndex i) (MatrixIndex j))
             (exists (v) (CellValue i j v))))
  
  (cl-comment "移動制約: 右(i, j+1) または 下(i+1, j) のみ")
  (forall (i j)
    (implies (CurrentPosition i j)
             (or (NextPosition (+ i 1) j)
                 (NextPosition i (+ j 1)))))

  ;; 2. 非中道の誤謬 (NMF): 全経路探索
  (cl-comment "全ての経路を個別に計算する O(2^N) は、計算資源への執着であり NMF である。")
  (forall (path)
    (implies (BruteForceSearch path)
             (NonMiddleFallacy path)))

  ;; 3. 勝義諦 (Ultimate Truth): 漸化式による還元
  (cl-comment "最小経路和は、先行する近傍（上と左）の最小値に依存するという構造的真理。")
  (forall (i j)
    (equal (MinPathSum i j)
           (+ (CellValue i j)
              (min (MinPathSum (- i 1) j)
                   (MinPathSum i (- j 1))))))

  ;; 4. 中道の現成 (Middle Way): 動的計画法 (DP)
  (cl-comment "過去の計算結果（負債）を清算しつつ、現在の最適解を現成させる。")
  (forall (i j)
    (implies (and (Resolved (MinPathSum (- i 1) j))
                  (Resolved (MinPathSum i (- j 1))))
             (Manifest (MinPathSum i j))))

  ;; 5. 跳躍 (ACX Jump): O(N^2) への複雑度還元
  (cl-comment "指数関数的な可能性の空間から、多項式時間の構造へと跳躍する。")
  (Action ACX_Jump
    (From (PotentialPaths (exp 2 N)))
    (To (StateSpace (pow N 2))))
)
||#


(defun parse-matrix (string)
  "文字列から行列（2次元配列）を生成する。"
  (let* ((lines (remove-if (lambda (s) (string= "" s))
                           (uiop:split-string string :separator '(#\Newline #\Return))))
         (size (length lines))
         (matrix (make-array (list size size) :element-type 'integer)))
    (iterate (for line in lines)
             (for i from 0)
             (iterate (for val-str in (uiop:split-string line :separator '(#\,)))
                      (for j from 0)
                      (setf (aref matrix i j) (parse-integer (string-trim " " val-str)))))
    matrix))

(defun solve-minimal-path-sum (matrix)
  "動的計画法を用いて最小経路和を求める。"
  (let* ((size (array-dimension matrix 0))
         (dp (make-array (list size size) :element-type 'integer)))
    (iterate (for i from 0 below size)
             (iterate (for j from 0 below size)
                      (let ((current-val (aref matrix i j)))
                        (setf (aref dp i j)
                              (+ current-val
                                 (cond
                                   ;; 起点
                                   ((and (zerop i) (zerop j)) 0)
                                   ;; 最上段（左からのみ）
                                   ((zerop i) (aref dp i (1- j)))
                                   ;; 最左列（上からのみ）
                                   ((zerop j) (aref dp (1- i) j))
                                   ;; 中間（上と左の最小値を選択）
                                   (t (min (aref dp (1- i) j)
                                           (aref dp i (1- j))))))))))
    (aref dp (1- size) (1- size))))

(defun main (&optional (filename "0081_matrix.txt"))
  "ファイルを読み込み、解答を出力する。"
  (if (probe-file filename)
      (let* ((data ;(alexandria:read-file-into-string filename)
              (dex:get "https://projecteuler.net/resources/documents/0081_matrix.txt")
              )
             (matrix (parse-matrix data)))
        (format t "Minimal path sum: ~A~%" (solve-minimal-path-sum matrix)))
      (error "File ~A not found. Please ensure the matrix file is in the current directory." filename)))

;; (main) ; 実行用


;;; 感想・自己分析
;;; Common Logic (clif) 形式で分析を行うことにより、単なる「アルゴリズムの選択」が「構造的必然性」へと昇華されました。
;;; 特に、全経路探索を「世俗への執着（NMF）」と定義し、漸化式への還元を「勝義諦」への到達とみなすことで、
;;; DPテーブルの更新が「負債（計算済みの状態）の清算」という明確な役割を持つことが意識されました。
;;; このメタ的な視点は、境界条件（i=0, j=0）を「空（起点）」からの現成として捉える助けとなり、
;;; iterateによる二重ループの実装において、各セルが「先行する状態の縁起」によって決定されるという
;;; 宣言的な理解をコードに反映させる結果となりました。


#+| Do it | (main )