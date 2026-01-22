;;; -*- mode: Lisp; coding: utf-8  -*-

(in-package "CL-USER") 
(defpackage "PROJECT-EULER-65" (:use "CL")) 
(in-package "PROJECT-EULER-65") 
#||
Project Euler 65を解くCommon Lispコードを生成してください

<p>The square root of $2$ can be written as an infinite continued fraction.</p>
<p>$$\sqrt{2} = 1 + \dfrac{1}{2 + \dfrac{1}{2 + \dfrac{1}{2 + \dfrac{1}{2 + ...}}}}$$</p>
<p>The infinite continued fraction can be written, $\sqrt{2} = [1; (2)]$, $(2)$ indicates that $2$ repeats <i>ad infinitum</i>. In a similar way, $\sqrt{23} = [4; (1, 3, 1, 8)]$.</p>
<p>It turns out that the sequence of partial values of continued fractions for square roots provide the best rational approximations. Let us consider the convergents for $\sqrt{2}$.</p>
<p>$$\begin{align}
&amp;1 + \dfrac{1}{2} &amp;= \dfrac{3}{2} \\
&amp;1 + \dfrac{1}{2 + \dfrac{1}{2}} &amp;= \dfrac{7}{5}\\
&amp;1 + \dfrac{1}{2 + \dfrac{1}{2 + \dfrac{1}{2}}} &amp;= \dfrac{17}{12}\\
&amp;1 + \dfrac{1}{2 + \dfrac{1}{2 + \dfrac{1}{2 + \dfrac{1}{2}}}} &amp;= \dfrac{41}{29}
\end{align}$$</p>
<p>Hence the sequence of the first ten convergents for $\sqrt{2}$ are:</p>
<p>$$1, \dfrac{3}{2}, \dfrac{7}{5}, \dfrac{17}{12}, \dfrac{41}{29}, \dfrac{99}{70}, \dfrac{239}{169}, \dfrac{577}{408}, \dfrac{1393}{985}, \dfrac{3363}{2378}, ...$$</p>
<p>What is most surprising is that the important mathematical constant,</p>
<p>$$e = [2; 1, 2, 1, 1, 4, 1, 1, 6, 1, ... , 1, 2k, 1, ...]$$</p>
<p>The first ten terms in the sequence of convergents for $e$ are:</p>
<p>$$2, 3, \dfrac{8}{3}, \dfrac{11}{4}, \dfrac{19}{7}, \dfrac{87}{32}, \dfrac{106}{39}, \dfrac{193}{71}, \dfrac{1264}{465}, \dfrac{1457}{536}, ...$$</p>
<p>The sum of digits in the numerator of the $10$<sup>th</sup> convergent is $1 + 4 + 5 + 7 = 17$.</p>
<p>Find the sum of digits in the numerator of the $100$<sup>th</sup> convergent of the continued fraction for $e$.</p>
||#

;; https://projecteuler.net/problem=65

(defun get-e-coefficient (n)
  "eの連分数展開のn番目の係数 a_n を返す (nは0オリジン)"
  (cond ((= n 0) 2)
        ((= (mod n 3) 2) (* 2 (1+ (floor n 3))))
        (t 1)))

(defun sum-of-digits (n)
  "数値の各桁の和を計算する"
  (loop :for char :across (write-to-string n)
        :sum (digit-char-p char)))

(defun solve-euler-65 (&optional (target-index 100))
  "eの第target-index近似分数の分子の桁和を求める"
  (let ((coeffs (loop :for i :from 0 :below target-index
                      :collect (get-e-coefficient i))))
    ;; 連分数を下から順に計算する
    ;; a0 + 1/(a1 + 1/(a2 + ... + 1/an))
    (let ((reversed-coeffs (reverse coeffs)))
      (let ((current-frac (rational (car reversed-coeffs))))
        (loop :for a :in (cdr reversed-coeffs)
              :do (setf current-frac (+ a (/ 1 current-frac))))
        
        ;; 分子 (numerator) を取得し、その桁和を計算
        (let ((final-numerator (numerator current-frac)))
          (format t "100th Convergent Numerator: ~A~%" final-numerator)
          (format t "Sum of Digits: ~A~%" (sum-of-digits final-numerator))
          (sum-of-digits final-numerator))))))


#+| Do it | (solve-euler-65 100)
;▻ 100th Convergent Numerator: 6963524437876961749120273824619538346438023188214475670667
;▻ Sum of Digits: 272
;→ 272
