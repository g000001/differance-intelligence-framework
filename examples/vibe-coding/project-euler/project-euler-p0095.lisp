;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3-flash-preview
(cl:in-package cl-user)
(defpackage #:project-euler-0095 (:use cl iterate alexandria) (:export #:solve))
(in-package #:project-euler-0095)

(defmacro optimized-code-p (boole)
  (typecase boole
    (null nil)
    (T `(declaim (optimize (speed 3) (safety 0) (debug 0) #+lispworks (hcl:fixnum-safety 0))))))

(optimized-code-p nil)


(defun solve (&optional (limit 1000000))
  (declare (type fixnum limit))
  ;; メモリアロケーションは気にしない前提だが、配列アクセス速度を極限まで高めるために1次元の型付配列を使用
  (let ((div-sums (make-array (1+ limit) :element-type 'fixnum :initial-element 1))
        (visited  (make-array (1+ limit) :element-type '(unsigned-byte 8) :initial-element 0))
        (longest-len 0)
        (best-min limit))
    (declare (type (simple-array fixnum (*)) div-sums)
             (type (simple-array (unsigned-byte 8) (*)) visited)
             (type fixnum longest-len best-min))

    (setf (aref div-sums 0) 0
          (aref div-sums 1) 0)

    (format t "[DEBUG] Phase 1: Constructing Divisor Sum Sieve...~%")
    ;; 約数和のエラトステネスの篩: O(N log N) の加算のみに計算量を崩壊させる
    (iter (for i from 2 to (ash limit -1))
          (declare (type fixnum i))
          (iter (for j from (ash i 1) to limit by i)
                (declare (type fixnum j))
                (incf (aref div-sums j) i)))

    (format t "[DEBUG] Phase 2: Starting 3-State Graph Traversal...~%")
    ;; グラフの閉路探索: 状態メモ化により全体で O(N) 回の実質ステップ数に抑え込む
    (iter (for i from 1 to limit)
          (declare (type fixnum i))
          
          ;; デバッグ用: 10万ノードごとに観測
          (when (zerop (mod i 100000))
            (format t "[DEBUG] Graph traversal swept ~D nodes...~%" i))

          ;; 状態 0 (未訪問) のみ探索を開始
          (when (zerop (aref visited i))
            (let ((path nil)
                  (curr i))
              (declare (type fixnum curr)
                       (type list path))
              
              ;; 状態 1 (現在のパス) でマーキングしながら探索。
              ;; limit を超えるか、状態 1 or 2 のノードに衝突するまで進む
              (iter (while (and (<= curr limit) (zerop (aref visited curr))))
                    (push curr path)
                    (setf (aref visited curr) 1)
                    (setf curr (aref div-sums curr)))

              ;; 衝突したノードが limit以内で、かつ「現在のパス内(状態1)」であれば閉路確定
              (when (and (<= curr limit) (= (aref visited curr) 1))
                (let ((c-len 0)
                      (c-min limit))
                  (declare (type fixnum c-len c-min))
                  ;; 閉路の長さを測り、最小要素を取り出す (逆順に積まれたパスから抽出)
                  (iter (for node in path)
                        (incf c-len)
                        (setf c-min (min c-min node))
                        (until (= node curr)))
                  
                  (when (> c-len longest-len)
                    (format t "[DEBUG] > New longest chain found! Length: ~D, Min Member: ~D~%" c-len c-min)
                    (setf longest-len c-len
                          best-min c-min))))

              ;; バックトラック: 探索済みのパス上のノードは全て 状態 2 (探索完了) に沈める
              ;; これにより他の探索がこのパスに合流した際、即座に刈り取られる(正当性原理の担保)
              (iter (for node in path)
                    (setf (aref visited node) 2)))))
            
    (format t "[DEBUG] Finished. Final Longest Chain Length: ~D~%" longest-len)
    best-min))


#+| Do it | (solve )
#|------------------------------------------------------------|
Timing the evaluation of (solve)
[DEBUG] Phase 1: Constructing Divisor Sum Sieve...
[DEBUG] Phase 2: Starting 3-State Graph Traversal...
[DEBUG] > New longest chain found! Length: 1, Min Member: 0
[DEBUG] > New longest chain found! Length: 2, Min Member: 220
[DEBUG] > New longest chain found! Length: 28, Min Member: 14316
[DEBUG] Graph traversal swept 100000 nodes...
[DEBUG] Graph traversal swept 200000 nodes...
[DEBUG] Graph traversal swept 300000 nodes...
[DEBUG] Graph traversal swept 400000 nodes...
[DEBUG] Graph traversal swept 500000 nodes...
[DEBUG] Graph traversal swept 600000 nodes...
[DEBUG] Graph traversal swept 700000 nodes...
[DEBUG] Graph traversal swept 800000 nodes...
[DEBUG] Graph traversal swept 900000 nodes...
[DEBUG] Graph traversal swept 1000000 nodes...
[DEBUG] Finished. Final Longest Chain Length: 28

User time    =        0.646
System time  =        0.018
Elapsed time =        0.602
Allocation   = 25133304 bytes
4868 Page faults
GC time      =        0.005
 |------------------------------------------------------------|#
;;→ 14316
:ok

