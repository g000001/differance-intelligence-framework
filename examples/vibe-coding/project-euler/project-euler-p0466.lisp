;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3.5-pro
(cl:in-package cl-user)
(defpackage #:project-euler-0466 (:use cl iterate alexandria) (:export #:solve))
(in-package #:project-euler-0466)

(defun solve (&optional (m 64) (n 10000000000000000))
  (declare (type fixnum m) (type (unsigned-byte 64) n))
  (format t "Calculating P(~A, ~A) using Reverse LCM Annihilation...~%" m n)

  ;; PIEの係数を保持するハッシュテーブル
  ;; key: 最小公倍数 (lcm), value: 係数
  (let ((h (make-hash-table :test 'eql))
        (ans 0))
    (declare (type integer ans))

    ;; 魔法の不変量：m から 1 へと「逆順」に計算する
    (iterate (for i from m downto 1)
      
      ;; 1. 行 i 単独の寄与分を追加
      (incf ans n)

      ;; 2. 過去の行（iより大きい行）の組み合わせとの重複（PIEの積項）を追加
      (maphash (lambda (l c)
                 (let* ((l-new (lcm i l))
                        ;; 寄与は floor((n * i) / lcm(i, l))
                        (term (floor (* n i) l-new)))
                   (incf ans (* (- c) term))))
               h)

      ;; 3. 次のステップ（i-1 以下）のために、係数テーブルを更新する
      (let ((next-h (make-hash-table :test 'eql)))
        ;; 現在の状態をコピー
        (maphash (lambda (l c)
                   (setf (gethash l next-h) c))
                 h)

        ;; 行 i 単独の状態を追加
        (incf (gethash i next-h 0) 1)

        ;; 行 i と過去の組み合わせを追加（符号反転）
        (maphash (lambda (l c)
                   (let ((l-new (lcm i l)))
                     (incf (gethash l-new next-h 0) (- c))))
                 h)

        ;; 4. 対消滅（Annihilation）の刈り込み
        ;; 不変量：ここで c=0 になった項（iの倍数だったもの）は完全に消滅する
        (clrhash h)
        (maphash (lambda (l c)
                   (unless (zerop c)
                     (setf (gethash l h) c)))
                 next-h))
      
      ;; デバッグ：状態空間の崩壊を観測
      (when (zerop (mod i 10))
        (format t "Processed row ~2D, Active LCMs remaining: ~D~%" i (hash-table-count h))))

    (format t "Final ans = ~A~%" ans)
    ans))


#+| Do it | (solve )
#|------------------------------------------------------------|
Timing the evaluation of (solve)
Calculating P(64, 10000000000000000) using Reverse LCM Annihilation...
Processed row 60, Active LCMs remaining: 31
Processed row 50, Active LCMs remaining: 9535
Processed row 40, Active LCMs remaining: 240383
Processed row 30, Active LCMs remaining: 1006335
Processed row 20, Active LCMs remaining: 1095679
Processed row 10, Active LCMs remaining: 901119
Final ans = 258381958195474745

User time    =       49.613
System time  =        4.471
Elapsed time =       53.670
Allocation   = 3294489768 bytes
4487572 Page faults
GC time      =       27.768
 |------------------------------------------------------------|#
;;→ 258381958195474745
:ok