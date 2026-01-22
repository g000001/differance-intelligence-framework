;;; -*- mode: Lisp; coding: utf-8  -*-

(in-package "CL-USER") 
(defpackage "PROJECT-EULER-67" (:use "CL")) 
(in-package "PROJECT-EULER-67") 
#||
Project Euler 67を解くCommon Lispコードを生成してください

<p>By starting at the top of the triangle below and moving to adjacent numbers on the row below, the maximum total from top to bottom is 23.</p>
<p class="monospace center"><span class="red"><b>3</b></span><br><span class="red"><b>7</b></span> 4<br>
2 <span class="red"><b>4</b></span> 6<br>
8 5 <span class="red"><b>9</b></span> 3</p>
<p>That is, 3 + 7 + 4 + 9 = 23.</p>
<p>Find the maximum total from top to bottom in <a href="resources/documents/0067_triangle.txt">triangle.txt</a> (right click and 'Save Link/Target As...'), a 15K text file containing a triangle with one-hundred rows.</p>
<p class="smaller"><b>NOTE:</b> This is a much more difficult version of <a href="problem=18">Problem 18</a>. It is not possible to try every route to solve this problem, as there are $2^{99}$ altogether! If you could check one trillion ($10^{12}$) routes every second it would take over twenty billion years to check them all. There is an efficient algorithm to solve it. ;o)</p>

||#

;; https://projecteuler.net/problem=67


(defun read-triangle (filename)
  "Read triangle from file. Returns a list of lists of integers."
  (with-open-file (in filename)
    (loop for line = (read-line in nil nil)
          while line
          collect
            (mapcar #'parse-integer
                    (split-sequence:split-sequence #\Space line)))))

(defun max-path-sum (triangle)
  "Compute maximum path sum using bottom-up DP."
  (let ((tri (mapcar #'copy-list triangle)))
    ;; Process from second-last row up to top
    (loop for i from (- (length tri) 2) downto 0 do
      (loop for j from 0 to i do
        (setf (nth j (nth i tri))
              (+ (nth j (nth i tri))
                 (max (nth j     (nth (1+ i) tri))
                      (nth (1+ j) (nth (1+ i) tri)))))))
    ;; Top element now contains max sum
    (caar tri)))

(defun solve-euler-67 (&optional (filename "0067_triangle.txt"))
  (let ((triangle (read-triangle filename)))
    (max-path-sum triangle)))


#+| Do it | (solve-euler-67 "~/Desktop/0067_triangle.txt")
;→ 7273
