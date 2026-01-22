;;; -*- mode: Lisp; coding: utf-8  -*-

(in-package "CL-USER") 
(defpackage "PROJECT-EULER-62" (:use "CL")) 
(in-package "PROJECT-EULER-62") 
#||
<p>The cube, $41063625$ ($345^3$), can be permuted to produce two other cubes: $56623104$ ($384^3$) and $66430125$ ($405^3$). In fact, $41063625$ is the smallest cube which has exactly three permutations of its digits which are also cube.</p>
<p>Find the smallest cube for which exactly five permutations of its digits are cube.</p>

||#

(defun sort-digits (n)
  "数値を各桁の数字に分解し、降順にソートして一意のキー（不動点）を生成する"
  (let ((digits (loop :for c :across (write-to-string n)
                      :collect c)))
    (sort digits #'char>)))

(defun solve-euler-62 ()
  (let ((cubes-map (make-hash-table :test 'equal))
        (target-count 5))
    (loop :for n :from 1
          :for cube := (* n n n)
          :for key := (sort-digits cube)
          :do
          ;; ハッシュテーブルに (ソート済み文字列 . (出現回数 . 最小の立方数)) を格納
          (let ((entry (gethash key cubes-map)))
            (if entry
                (progn
                  (incf (car entry))
                  (when (= (car entry) target-count)
                    ;; 5回目に達した時、その最小の立方数を返して終了
                    (return-from solve-euler-62 (cdr entry))))
                (setf (gethash key cubes-map) (cons 1 cube)))))))

;; 実行
;(format t "Result: ~A~%" (solve-euler-62))
;▻ Result: 127035954683
;→ nil
