;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3-flash-preview
(cl:in-package cl-user)
(defpackage #:project-euler-0189 (:use cl iterate alexandria) (:export #:solve))
(in-package #:project-euler-0189)

(defun solve ()
  (let* ((pow3 #(1 3 9 27 81 243 729 2187 6561))
         ;; 1行目の状態空間（上向き三角形1つにつき、色0, 1, 2がそれぞれ1通り）
         (dp (make-array 3 :initial-element 1)))
    
    (format t "Starting topological DP...~%")
    
    ;; 2行目から8行目まで状態を押し進める
    (iterate (for i from 2 to 8)
      (let ((next-dp (make-array (aref pow3 i) :initial-element 0)))
               
        ;; prev-state を走査
        (iterate (for prev-state from 0 below (aref pow3 (1- i)))
          (let ((count (aref dp prev-state)))
            (when (> count 0)
                            
              ;; prev-colors のデコード（整数値から色配列への展開）
              (let ((prev-colors (make-array (1- i) :element-type 'fixnum)))
                (let ((temp prev-state))
                  (iterate (for j from 0 below (1- i))
                    (setf (aref prev-colors j) (mod temp 3))
                    (setf temp (truncate temp 3))))
                              
                ;; DFSを用いた次状態(curr-state)の動的構築と枝刈り
                (labels ((dfs (j curr-state ways curr-colors)
                           (if (= j i)
                               ;; 行の端まで到達したら重みを掛けて合流
                               (incf (aref next-dp curr-state) (* count ways))
                               ;; 現在の上向き三角形の色 c を試す
                               (iterate (for c from 0 to 2)
                                 (setf (aref curr-colors j) c)
                                 (let ((new-ways ways))
                                   (when (> j 0)
                                     ;; 左側の下向き三角形に対する色の制約をチェック
                                     (let* ((c1 (aref prev-colors (1- j))) ; 上
                                            (c2 (aref curr-colors (1- j))) ; 左
                                            (c3 c)                         ; 右
                                            (w (cond ((= c1 c2 c3) 2)
                                                     ((/= c1 c2 c3) 0)
                                                     (t 1))))
                                       (setf new-ways (* new-ways w))))
                                   ;; 0通りでない場合のみ探索を継続（強力な枝刈り）
                                   (when (> new-ways 0)
                                     (dfs (1+ j)
                                          (+ curr-state (* c (aref pow3 j)))
                                          new-ways
                                          curr-colors)))))))
                  (dfs 0 0 1 (make-array i :element-type 'fixnum)))))))
        (setf dp next-dp)
        (format t "Row ~A completed, Max Target States: ~A~%" i (length dp))))
               
    (let ((ans 0))
      (format t "Aggregating expectations...~%")
      (iterate (for count in-vector dp)
        (incf ans count))
      (format t "Final Answer: ~A~%" ans)
      ans)))


#+| Do it | (solve )
#|------------------------------------------------------------|
Timing the evaluation of (solve)
Starting topological DP...
Row 2 completed, Max Target States: 9
Row 3 completed, Max Target States: 27
Row 4 completed, Max Target States: 81
Row 5 completed, Max Target States: 243
Row 6 completed, Max Target States: 729
Row 7 completed, Max Target States: 2187
Row 8 completed, Max Target States: 6561
Aggregating expectations...
Final Answer: 10834893628237824

User time    =        0.389
System time  =        0.018
Elapsed time =        0.341
Allocation   = 1187904 bytes
4012 Page faults
GC time      =        0.000
 |------------------------------------------------------------|#
;;→ 10834893628237824
:ok