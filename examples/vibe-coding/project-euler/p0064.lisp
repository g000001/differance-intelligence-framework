;;; -*- mode: Lisp; coding: utf-8  -*-

(in-package "CL-USER") 
(defpackage "PROJECT-EULER-64" (:use "CL")) 
(in-package "PROJECT-EULER-64") 
#||
<p>All square roots are periodic when written as continued fractions and can be written in the form:</p>

$$\sqrt{N}=a_0 + \dfrac 1 {a_1 + \dfrac 1 {a_2 + \dfrac 1 {a_3 + \dots}}}$$

<p>For example, let us consider $\sqrt{23}:$</p>
$$\sqrt{23} = 4 + \sqrt{23}-4=4 + \dfrac 1 {\dfrac 1 {\sqrt{23}-4}} = 4+\dfrac 1  {1 + \dfrac{\sqrt{23}-3}7}$$

<p>If we continue we would get the following expansion:</p>

$$\sqrt{23}=4 + \dfrac 1 {1 + \dfrac 1 {3+ \dfrac 1 {1 + \dfrac 1 {8+ \dots}}}}$$

<p>The process can be summarised as follows:</p>

$$\begin{align} \quad \quad a_0 &amp;= 4, \frac 1 {\sqrt{23}-4}=\frac {\sqrt{23}+4} 7=1+\frac {\sqrt{23}-3} 7 \\
\quad \quad a_1 &amp;= 1, \frac 7 {\sqrt{23}-3}=\frac {7(\sqrt{23}+3)} {14}=3+\frac {\sqrt{23}-3} 2 \\
\quad \quad a_2 &amp;= 3, \frac 2 {\sqrt{23}-3}=\frac {2(\sqrt{23}+3)} {14}=1+\frac {\sqrt{23}-4} 7 \\
\quad \quad a_3 &amp;= 1, \frac 7 {\sqrt{23}-4}=\frac {7(\sqrt{23}+4)} 7=8+\sqrt{23}-4 \\
\quad \quad a_4 &amp;= 8, \frac 1 {\sqrt{23}-4}=\frac {\sqrt{23}+4} 7=1+\frac {\sqrt{23}-3} 7 \\
\quad \quad a_5 &amp;= 1, \frac 7 {\sqrt{23}-3}=\frac {7 (\sqrt{23}+3)} {14}=3+\frac {\sqrt{23}-3} 2 \\
\quad \quad a_6 &amp;= 3, \frac 2 {\sqrt{23}-3}=\frac {2(\sqrt{23}+3)} {14}=1+\frac {\sqrt{23}-4} 7 \\
\quad \quad a_7 &amp;= 1, \frac 7 {\sqrt{23}-4}=\frac {7(\sqrt{23}+4)} {7}=8+\sqrt{23}-4 \end{align}$$


<p>It can be seen that the sequence is repeating. For conciseness, we use the notation $\sqrt{23}=[4;(1,3,1,8)]$, to indicate that the block (1,3,1,8) repeats indefinitely.</p>

<p>The first ten continued fraction representations of (irrational) square roots are:</p>
<p>
$\quad \quad \sqrt{2}=[1;(2)]$, period=$1$<br>
$\quad \quad \sqrt{3}=[1;(1,2)]$, period=$2$<br>
$\quad \quad \sqrt{5}=[2;(4)]$, period=$1$<br>
$\quad \quad \sqrt{6}=[2;(2,4)]$, period=$2$<br>
$\quad \quad \sqrt{7}=[2;(1,1,1,4)]$, period=$4$<br>
$\quad \quad \sqrt{8}=[2;(1,4)]$, period=$2$<br>
$\quad \quad \sqrt{10}=[3;(6)]$, period=$1$<br>
$\quad \quad \sqrt{11}=[3;(3,6)]$, period=$2$<br>
$\quad \quad \sqrt{12}=[3;(2,6)]$, period=$2$<br>
$\quad \quad \sqrt{13}=[3;(1,1,1,1,6)]$, period=$5$
</p>
<p>Exactly four continued fractions, for $N \le 13$, have an odd period.</p>
<p>How many continued fractions for $N \le 10\,000$ have an odd period?</p>
||#

;; https://projecteuler.net/problem=64


(defun count-odd-period-sqrt (limit)
  (loop :for n :from 2 :to limit
        :for m0 := 0
        :for d0 := 1
        :for a0 := (floor (sqrt n))
        ;; 完全平方数は無理数ではない（連分数にならない）ため除外
        :when (/= (* a0 a0) n)
        :count (let ((period-length (get-period-length n m0 d0 a0)))
                 (oddp period-length))))

(defun get-period-length (n m d a0)
  "平方根の連分数展開の周期を、整数演算のみで算出する (Dfix0への整列)"
  (loop :with a := a0
        :with m-val := m
        :with d-val := d
        :for count :from 0
        ;; アルゴリズムの核心: m と d を更新
        :do (setf m-val (- (* d-val a) m-val))
            (setf d-val (/ (- n (* m-val m-val)) d-val))
            (setf a (floor (/ (+ a0 m-val) d-val)))
        ;; a が a0 の2倍に達したとき、周期が一周したと判定される
        :until (= a (* 2 a0))
        :finally (return (1+ count))))

(defun solve-euler-64 ()
  (let ((result (count-odd-period-sqrt 10000)))
    (format t "Result: ~A~%" result)
    result))

#+| Do it | (solve-euler-64 )
;▻ Result: 1322
;→ 1322
