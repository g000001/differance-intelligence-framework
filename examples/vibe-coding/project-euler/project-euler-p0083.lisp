;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3-pro
(cl:in-package cl-user)
(defpackage #:project-euler-0083 (:use cl iterate alexandria) (:export #:solve))
(in-package #:project-euler-0083)

(defmacro optimized-code-p (boole)
  (typecase boole
    (null nil)
    (T `(declaim (optimize (speed 3) (safety 0) (debug 0) #+lispworks (hcl:fixnum-safety 0))))))

(optimized-code-p nil)


(defconstant $infinity-cost #.(expt 10 18)
  "到達不可能な初期距離を示す無限大の代わりとなる巨大な数値")

(defun split-string-by-character (target-string delimiter-character)
  "文字列を指定された文字で分割する。"
  (iterate
    (with result-list = nil)
    (with current-start-index = 0)
    (for found-delimiter-index = (position delimiter-character target-string :start current-start-index))
    (push (subseq target-string current-start-index found-delimiter-index) result-list)
    (if (null found-delimiter-index)
        (return (nreverse result-list)))
    (setf current-start-index (1+ found-delimiter-index))))

(defun parse-matrix-from-file (file-path)
  "テキストファイルから行列を読み込み、2次元配列を生成する。"
  (let* ((file-content (read-file-into-string file-path))
         (line-strings (split-string-by-character file-content #\Newline))
         (valid-lines (remove-if (lambda (line) (zerop (length line))) line-strings))
         (matrix-size (length valid-lines))
         (result-matrix (make-array (list matrix-size matrix-size) :initial-element 0)))
    (iterate
      (for row-index from 0 below matrix-size)
      (for current-line in valid-lines)
      (let ((number-strings (split-string-by-character current-line #\,)))
        (iterate
          (for column-index from 0 below matrix-size)
          (for number-string in number-strings)
          (setf (aref result-matrix row-index column-index)
                (parse-integer number-string)))))
    result-matrix))

;;; 優先度付きキュー (Binary Min-Heap) の実装
(defstruct (priority-queue (:constructor make-priority-queue-internal))
  (heap-array (make-array 1000 :adjustable t :fill-pointer 0)))

(defun create-priority-queue ()
  (make-priority-queue-internal))

(defun queue-is-empty-p (queue)
  (zerop (fill-pointer (priority-queue-heap-array queue))))

(defun queue-push (queue cost row column)
  "キューに要素を追加し、ヒープ条件をボトムアップで維持する。"
  (let ((heap (priority-queue-heap-array queue))
        (new-element (list cost row column)))
    (vector-push-extend new-element heap)
    (iterate
      (with current-index = (1- (length heap)))
      (while (> current-index 0))
      (for parent-index = (floor (1- current-index) 2))
      (if (< (first (aref heap current-index)) (first (aref heap parent-index)))
          (progn
            (rotatef (aref heap current-index) (aref heap parent-index))
            (setf current-index parent-index))
          (finish)))))

(defun queue-pop (queue)
  "キューから最小コストの要素を取り出し、ヒープ条件をトップダウンで再構築する。"
  (let* ((heap (priority-queue-heap-array queue))
         (last-index (1- (length heap)))
         (minimum-element (aref heap 0)))
    (setf (aref heap 0) (aref heap last-index))
    (decf (fill-pointer heap))
    (when (> (length heap) 0)
      (iterate
        (with current-index = 0)
        (while t)
        (for left-child-index = (+ (* current-index 2) 1))
        (for right-child-index = (+ (* current-index 2) 2))
        (for smallest-index = current-index)
        (when (and (< left-child-index (length heap))
                   (< (first (aref heap left-child-index)) (first (aref heap smallest-index))))
          (setf smallest-index left-child-index))
        (when (and (< right-child-index (length heap))
                   (< (first (aref heap right-child-index)) (first (aref heap smallest-index))))
          (setf smallest-index right-child-index))
        (if (= smallest-index current-index)
            (finish)
            (progn
              (rotatef (aref heap current-index) (aref heap smallest-index))
              (setf current-index smallest-index)))))
    minimum-element))

;;; Dijkstra法による最短経路探索
(defun find-minimal-path-sum (matrix)
  "ダイクストラ法を用いて左上から右下への最小経路和を計算する。"
  (let* ((matrix-size (array-dimension matrix 0))
         (distances (make-array (list matrix-size matrix-size) :initial-element $infinity-cost))
         (queue (create-priority-queue)))
    
    ;; 始点の初期化
    (setf (aref distances 0 0) (aref matrix 0 0))
    (queue-push queue (aref matrix 0 0) 0 0)
    
    (iterate
      (while (not (queue-is-empty-p queue)))
      (for current-element = (queue-pop queue))
      (for current-cost = (first current-element))
      (for current-row = (second current-element))
      (for current-column = (third current-element))
      
      ;; 既に確定したより短い経路が存在する場合は枝刈り
      (when (> current-cost (aref distances current-row current-column))
        (next-iteration))
      
      ;; 右下に到達した時点で終了 (Dijkstraの性質上、これが最小値)
      (if (and (= current-row (1- matrix-size))
               (= current-column (1- matrix-size)))
          (return current-cost))
      
      ;; 4方向への遷移
      (let ((direction-rows '(-1 1 0 0))
            (direction-columns '(0 0 -1 1)))
        (iterate
          (for direction-index from 0 below 4)
          (for next-row = (+ current-row (nth direction-index direction-rows)))
          (for next-column = (+ current-column (nth direction-index direction-columns)))
          
          ;; グリッド境界内の検証
          (when (and (>= next-row 0) (< next-row matrix-size)
                     (>= next-column 0) (< next-column matrix-size))
            (let ((new-cost (+ current-cost (aref matrix next-row next-column))))
              ;; より小さなコストで到達できる場合のみ更新してキューへ
              (when (< new-cost (aref distances next-row next-column))
                (setf (aref distances next-row next-column) new-cost)
                (queue-push queue new-cost next-row next-column)))))))
    (aref distances (1- matrix-size) (1- matrix-size))))

(defun solve (&optional (file-path "/tmp/0082_matrix.txt"))
  (format t "--- 最小経路和の探索を開始します ---~%")
  (let* ((matrix (parse-matrix-from-file file-path))
         (matrix-size (array-dimension matrix 0))
         (minimal-cost (find-minimal-path-sum matrix)))
    (format t "行列サイズ: ~D x ~D~%" matrix-size matrix-size)
    (format t "最小経路和: ~D~%" minimal-cost)
    (format t "--- 探索完了 ---~%")
    minimal-cost))


#+| Do it | (solve )
#|------------------------------------------------------------|
Timing the evaluation of (solve)
--- 最小経路和の探索を開始します ---
行列サイズ: 80 x 80
最小経路和: 425185
--- 探索完了 ---

User time    =        0.016
System time  =        0.000
Elapsed time =        0.008
Allocation   = 1208248 bytes
100 Page faults
GC time      =        0.000
 |------------------------------------------------------------|#
;;→ 425185
:ok