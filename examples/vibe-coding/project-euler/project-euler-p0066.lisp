;;; -*- mode: Lisp; coding: utf-8  -*-

(in-package "CL-USER") 
(defpackage "PROJECT-EULER-66" (:use "CL")) 
(in-package "PROJECT-EULER-66") 
#||
Project Euler 66を解くCommon Lispコードを生成してください

<p>Consider quadratic Diophantine equations of the form:
$$x^2 - Dy^2 = 1$$</p>
<p>For example, when $D=13$, the minimal solution in $x$ is $649^2 - 13 \times 180^2 = 1$.</p>
<p>It can be assumed that there are no solutions in positive integers when $D$ is square.</p>
<p>By finding minimal solutions in $x$ for $D = \{2, 3, 5, 6, 7\}$, we obtain the following:</p>
$$\begin{align}
3^2 - 2 \times 2^2 &amp;= 1\\
2^2 - 3 \times 1^2 &amp;= 1\\
{\color{red}{\mathbf 9}}^2 - 5 \times 4^2 &amp;= 1\\
5^2 - 6 \times 2^2 &amp;= 1\\
8^2 - 7 \times 3^2 &amp;= 1
\end{align}$$
<p>Hence, by considering minimal solutions in $x$ for $D \le 7$, the largest $x$ is obtained when $D=5$.</p>
<p>Find the value of $D \le 1000$ in minimal solutions of $x$ for which the largest value of $x$ is obtained.</p>

||#

;; https://projecteuler.net/problem=66


(defun solve-pell-minimal-x (d)
  "ペル方程式 x^2 - Dy^2 = 1 の最小のxを連分数展開を用いて求める"
  (let* ((a0 (floor (sqrt d)))
         (m 0)
         (d-val 1)
         (a a0)
         ;; 近似分数の漸化式用変数 (P_n / Q_n)
         ;; P_{-1}=1, P_0=a0
         ;; Q_{-1}=0, Q_0=1
         (p-prev 1)
         (p-curr a0)
         (q-prev 0)
         (q-curr 1))
    
    (loop :while (/= (- (* p-curr p-curr) (* d q-curr q-curr)) 1)
          :do (setf m (- (* d-val a) m))
              (setf d-val (/ (- d (* m m)) d-val))
              (setf a (floor (/ (+ a0 m) d-val)))
              
              ;; 漸化式: P_n = a_n * P_{n-1} + P_{n-2}
              (let ((p-next (+ (* a p-curr) p-prev))
                    (q-next (+ (* a q-curr) q-prev)))
                (setf p-prev p-curr
                      p-curr p-next
                      q-prev q-curr
                      q-curr q-next))
          :finally (return p-curr))))

(defun solve-euler-66 ()
  (let ((max-x 0)
        (result-d 0))
    (loop :for d :from 2 :to 1000
          :for root = (sqrt d)
          ;; Dが平方数の場合は解を持たないためスキップ
          :unless (= (* (floor root) (floor root)) d)
          :do (let ((x (solve-pell-minimal-x d)))
                (when (> x max-x)
                  (setf max-x x
                        result-d d))))
    (format t "Largest minimal x is obtained at D = ~A~%" result-d)
    result-d))

#+| Do it | (solve-euler-66 )
;▻ Largest minimal x is obtained at D = 661
;→ 661
