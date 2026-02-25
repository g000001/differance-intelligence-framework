;;; -*- mode: Lisp; coding: utf-8  -*-
(cl:in-package cl-user)
(defpackage #:project-euler-0166 (:use cl alexandria))
(in-package #:project-euler-0166)

(declaim (optimize (speed 3) (safety 0) (debug 0)))

(defun get-partitions ()
  "合計がSになる4つの数字（0-9）の組み合わせを、Sごとにフラットな配列としてプリコンパイルする。"
  (let ((lists (make-array 37 :initial-element nil)))
    (dotimes (a 10)
      (dotimes (b 10)
        (dotimes (c 10)
          (dotimes (d 10)
            (let ((s (+ a b c d)))
              ;; (a b c d) の順で格納されるようにプッシュ
              (push a (svref lists s))
              (push b (svref lists s))
              (push c (svref lists s))
              (push d (svref lists s)))))))
    (let ((vectors (make-array 37)))
      (dotimes (s 37)
        (setf (svref vectors s)
              (make-array (length (svref lists s))
                          :element-type '(unsigned-byte 8)
                          :initial-contents (svref lists s))))
      vectors)))

(declaim (ftype (function (fixnum simple-vector) (values integer &optional)) count-for-s))
(defun count-for-s (s partitions)
  "特定の合計値 S に対して、条件を満たすグリッドの数を算出する。"
  (declare (fixnum s) (simple-vector partitions))
  (let ((total 0)
        (parts (svref partitions s)))
    (declare (integer total)
             (type (simple-array (unsigned-byte 8) (*)) parts))
    (let ((len (length parts)))
      ;; Row 1: a b c d
      (loop for idx1 from 0 by 4 below len do
              (let ((a (aref parts idx1))
                    (b (aref parts (+ idx1 1)))
                    (c (aref parts (+ idx1 2)))
                    (d (aref parts (+ idx1 3))))
                (declare (fixnum a b c d))
                ;; Row 2: e f g h
                (loop for idx2 from 0 by 4 below len do
                        (let ((e (aref parts idx2))
                              (f (aref parts (+ idx2 1)))
                              (g (aref parts (+ idx2 2)))
                              (h (aref parts (+ idx2 3))))
                          (declare (fixnum e f g h))
                          ;; i = grid[2][0] (3行目1列目)
                          ;; a+e+i+m = S (1列目) より、m = S-a-e-i
                          ;; 0 <= m <= 9 の制約から i の範囲を限定する (跳躍)
                          (let ((i-min (max 0 (- s a e 9)))
                                (i-max (min 9 (- s a e))))
                            (declare (fixnum i-min i-max))
                            (loop for i from i-min to i-max do
                                    (let* ((m (- s a e i))         ; Col 1
                                           (j (- s d g m)))        ; Diag 2 (d+g+j+m = S)
                                      (declare (fixnum m j))
                                      (when (and (>= j 0) (<= j 9))
                                        (let ((n (- s b f j)))     ; Col 2 (b+f+j+n = S)
                                          (declare (fixnum n))
                                          (when (and (>= n 0) (<= n 9))
                                            ;; Diag 1: a+f+k+p = S
                                            ;; Row 3: i+j+k+l = S  => k = S-i-j-l
                                            ;; a+f+(S-i-j-l)+p = S => l-p = a+f-i-j (C1)
                                            ;; Col 4: d+h+l+p = S  => l+p = S-d-h (C2)
                                            (let* ((c1 (- (+ a f) (+ i j)))
                                                   (c2 (- s (+ d h))))
                                              (declare (fixnum c1 c2))
                                              ;; l = (C1+C2)/2, p = (C2-C1)/2
                                              (when (evenp (+ c1 c2))
                                                (let ((l (ash (+ c1 c2) -1))
                                                      (p (ash (- c2 c1) -1)))
                                                  (declare (fixnum l p))
                                                  (when (and (>= l 0) (<= l 9)
                                                             (>= p 0) (<= p 9))
                                                    (let ((k (- s i j l))) ; Row 3
                                                      (declare (fixnum k))
                                                      (when (and (>= k 0) (<= k 9))
                                                        (let ((o (- s m n p))) ; Row 4
                                                          (declare (fixnum o))
                                                          (when (and (>= o 0) (<= o 9)
                                                                     (= (+ c g k o) s)) ; Col 3 Check
                                                            (incf total))))))))))))))))))))
    total))

(defun solve ()
  "Sと36-Sの対称性を利用して、全グリッド数を計算する。"
  (let ((partitions (get-partitions))
        (grand-total 0))
    (declare (integer grand-total))
    ;; 0 <= S <= 17 の結果を2倍し、S=18 を加える (中道の現成)
    (loop for s from 0 to 17 do
      (incf grand-total (* 2 (count-for-s s partitions))))
    (incf grand-total (count-for-s 18 partitions))
    grand-total))

;; 実行と出力
;(format t "~A~%" (solve))


#+| Do it | (solve )
;;→ 7130034
