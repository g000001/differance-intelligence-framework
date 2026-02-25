;;; -*- mode: Lisp; coding: utf-8  -*-

(cl:in-package "CL-USER")

(defun solve-sudoku (grid)
  "ACX的思想に基づく数独ソルバー"
  (labels ((get-candidates (r c)
             (let ((used (make-array 10 :element-type 'bit :initial-element 0)))
               (loop :for i :from 0 :to 8
                     :do (setf (bit used (aref grid r i)) 1
                               (bit used (aref grid i c)) 1
                               (bit used (aref grid (+ (* (floor r 3) 3) (floor i 3))
                                               (+ (* (floor c 3) 3) (mod i 3)))) 1))
               (loop :for v :from 1 :to 9 :when (zerop (bit used v)) collect v)))

           (step-acx (pos κ ρ)
             "ACXのコア：posは現在のSeed位置、κは成功時の継続、ρは失敗時の跳躍(Restart)"
             (if (= pos 81)
                 (funcall κ) ; すべての境界Cを突破（成功）
                 (let ((r (floor pos 9))
                       (c (mod pos 9)))
                   (if (not (zerop (aref grid r c)))
                       (step-acx (1+ pos) κ ρ) ; 既に確定している「色」はスキップ
                       (let ((candidates (get-candidates r c)))
                         (if (null candidates)
                             (funcall ρ) ; 空（不全）に遭遇：直近の分岐へRestart
                             (loop-candidates candidates r c pos κ ρ)))))))

           (loop-candidates (vals r c pos κ ρ)
             "候補数字を次々に顕現させるトランポリン的試行"
             (if (null vals)
                 (funcall ρ) ; このパスの可能性が尽きた：上位の空へ
                 (progn
                   (setf (aref grid r c) (car vals))
                   (step-acx (1+ pos) 
                             κ 
                             (lambda () ; 新たなRestart Mapの登録
                               (setf (aref grid r c) 0) ; Debtの清算（色を空に戻す）
                               (loop-candidates (cdr vals) r c pos κ ρ)))))))

    ;; 初期呼び出し：不全時のρは「解なし」を意味する沈黙
    (step-acx 0 (lambda () grid) (lambda () nil))))

;; --- 補助関数：Euler 96 仕様の入出力と集計 ---

(defun parse-grid (lines)
  (let ((grid (make-array '(9 9) :initial-element 0)))
    (loop :for r :from 0 :to 8
          :for line :in lines
          :do (loop :for c :from 0 :to 8
                    :do (setf (aref grid r c) (digit-char-p (char line c)))))
    grid))

(defun solve-euler-96 (filename)
  (with-open-file (in filename)
    (let ((total 0))
      (loop :for line := (read-line in nil)
            :while line
            :do (let* ((grid-lines (loop repeat 9 collect (read-line in)))
                       (grid (parse-grid grid-lines))
                       (solved (solve-sudoku grid)))
                  (incf total (+ (* (aref solved 0 0) 100)
                                 (* (aref solved 0 1) 10)
                                 (aref solved 0 2)))))
      total)))



;(solve-euler-96 "~/Desktop/0096_sudoku.txt")
;→ 24702

