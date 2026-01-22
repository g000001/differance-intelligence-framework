;;; -*- mode: Lisp; coding: utf-8  -*-

(in-package "CL-USER") 
(defpackage "PROJECT-EULER-68" (:use "CL")) 
(in-package "PROJECT-EULER-68") 
#||
Project Euler 68を解くCommon Lispコードを生成してください

<p>Consider the following "magic" 3-gon ring, filled with the numbers 1 to 6, and each line adding to nine.</p>
<div class="center">
<img src="resources/images/0068_1.png?1678992052" class="dark_img" alt=""><br></div>
<p>Working <b>clockwise</b>, and starting from the group of three with the numerically lowest external node (4,3,2 in this example), each solution can be described uniquely. For example, the above solution can be described by the set: 4,3,2; 6,2,1; 5,1,3.</p>
<p>It is possible to complete the ring with four different totals: 9, 10, 11, and 12. There are eight solutions in total.</p>
<div class="center">
<table width="400" cellspacing="0" cellpadding="0"><tr><td width="100"><b>Total</b></td><td width="300"><b>Solution Set</b></td>
</tr><tr><td>9</td><td>4,2,3; 5,3,1; 6,1,2</td>
</tr><tr><td>9</td><td>4,3,2; 6,2,1; 5,1,3</td>
</tr><tr><td>10</td><td>2,3,5; 4,5,1; 6,1,3</td>
</tr><tr><td>10</td><td>2,5,3; 6,3,1; 4,1,5</td>
</tr><tr><td>11</td><td>1,4,6; 3,6,2; 5,2,4</td>
</tr><tr><td>11</td><td>1,6,4; 5,4,2; 3,2,6</td>
</tr><tr><td>12</td><td>1,5,6; 2,6,4; 3,4,5</td>
</tr><tr><td>12</td><td>1,6,5; 3,5,4; 2,4,6</td>
</tr></table></div>
<p>By concatenating each group it is possible to form 9-digit strings; the maximum string for a 3-gon ring is 432621513.</p>
<p>Using the numbers 1 to 10, and depending on arrangements, it is possible to form 16- and 17-digit strings. What is the maximum <b>16-digit</b> string for a "magic" 5-gon ring?</p>
<div class="center">
<img src="resources/images/0068_2.png?1678992052" class="dark_img" alt=""><br></div>

||#

;; https://projecteuler.net/problem=68

(defun next-permutation (array)
  "順列を辞書順で次に進める (色の遷移)"
  (let ((n (length array)))
    (loop :for i :from (- n 2) :downto 0
          :when (char< (aref array i) (aref array (1+ i)))
            :do (loop :for j :from (- n 1) :downto (1+ i)
                      :when (char< (aref array i) (aref array j))
                        :do (rotatef (aref array i) (aref array j))
                            (setf (subseq array (1+ i)) (reverse (subseq array (1+ i))))
                            (return-from next-permutation array)))
    nil))

(defun solve-euler-68 ()
  (let ((nums '(1 2 3 4 5 6 7 8 9 10))
        (max-string ""))
    (labels ((get-string (p)
               (let* ((lines (list (list (nth 0 p) (nth 5 p) (nth 6 p))
                                   (list (nth 1 p) (nth 6 p) (nth 7 p))
                                   (list (nth 2 p) (nth 7 p) (nth 8 p))
                                   (list (nth 3 p) (nth 8 p) (nth 9 p))
                                   (list (nth 4 p) (nth 9 p) (nth 5 p))))
                      (min-outer (loop :for i :from 0 :to 4 :minimize (nth i p)))
                      (start-idx (position-if (lambda (l) (= (car l) min-outer)) lines))
                      (ordered-lines (append (subseq lines start-idx) (subseq lines 0 start-idx))))
                 ;; 各列を連結して一つの文字列へ (ACの顕現)
                 (apply #'concatenate 'string 
                        (mapcar (lambda (l) (format nil "~{~A~}" l)) ordered-lines)))))

      ;; 順列生成と検証のループ
      (alexandria:map-permutations
         (lambda (p)
           (let ((s1 (+ (nth 0 p) (nth 5 p) (nth 6 p)))
                 (s2 (+ (nth 1 p) (nth 6 p) (nth 7 p)))
                 (s3 (+ (nth 2 p) (nth 7 p) (nth 8 p)))
                 (s4 (+ (nth 3 p) (nth 8 p) (nth 9 p)))
                 (s5 (+ (nth 4 p) (nth 9 p) (nth 5 p))))
             ;; 全ての列の和が一致し(mweq)、かつ文字列長が16(10が外側)の場合
             (when (and (= s1 s2 s3 s4 s5)
                        (= (length (get-string p)) 16)) ; 修正点: string= ではなく = を使用
               (let ((current-s (get-string p)))
                 (when (string> current-s max-string)
                   (setf max-string current-s))))))
         nums)
      (values (parse-integer max-string)))))


#+| Do it | (solve-euler-68 )
;→ 6531031914842725
