;;; -*- mode: Lisp; coding: utf-8 -*-
;;; llm-model: gemini-3.1-pro
(cl:in-package cl-user)
(defpackage #:project-euler-0279 (:use cl iterate alexandria) (:export #:solve))
(in-package #:project-euler-0279)

(defmacro source-pathname ()
  "Compute source pathname"
  `(load-time-value ,(or *compile-file-truename* *load-truename* (uiop:getcwd))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :cffi :silent t))

#||
【数学的考察と次元崩壊の構築】
1. Nivenの定理による角度空間の次元崩壊:
   「整数辺」を持ち、「少なくとも1つの角度が整数（度）」である三角形を考えます。
   余弦定理より、辺が全て整数ならその角度のコサイン（$\cos\theta$）は有理数でなければなりません。
   Nivenの定理によれば、$0^\circ < \theta < 180^\circ$ で $\theta$ が有理数度かつ $\cos\theta$ が有理数になるのは、
   $\theta \in \{60^\circ, 90^\circ, 120^\circ\}$ の3つのみです。
   これにより、無限に思える「整数の角度」という条件が、わずか3種類の特定のディオファントス方程式に完全に崩壊します。

2. ピタゴラスとアイゼンシュタインのパラメータ化:
   - $90^\circ$ (直角): $a^2 + b^2 = c^2$
   - $120^\circ$: $a^2 + b^2 + ab = c^2$
   - $60^\circ$: $a^2 + b^2 - ab = c^2$
   これらは互いに素なパラメータ $m > n > 0$ を用いて原始解を生成できます。
   特に $60^\circ$ の場合は対称性の破れから2つの異なるタイプ（Type 1, Type 2）のパラメータ化が存在します。
   重複を防ぐための不変量として、アイゼンシュタイン三つ組には $m - n \not\equiv 0 \pmod 3$ という極めて重要な制約が存在します。

3. 計算量のフェルミ推定とレッドラインの突破:
   周長 $P \le 10^8$ という制約から、$m$ の上限は高々 $\approx 7071$ に制限されます。
   パラメータ $m, n$ の二重ループの総実行回数は $\mathcal{O}(N)$ 相当であり、各タイプを合計しても約 $2.5 \times 10^7$ 回です。
   これはフェルミ推定のレッドライン内に収まっており、Fortranのネイティブ演算であれば数ミリ秒〜数十ミリ秒で完結することが保証されます。
||#

(defparameter *fortran-source* "
module solver
    implicit none
contains
    pure integer(8) function gcd(a, b)
        integer(8), intent(in) :: a, b
        integer(8) :: x, y, r
        x = a
        y = b
        do while (y /= 0_8)
            r = mod(x, y)
            x = y
            y = r
        end do
        gcd = x
    end function gcd

    integer(8) function solve_279_fortran(limit) bind(C, name=\"solve_279_fortran\")
        use iso_c_binding
        integer(8), intent(in), value :: limit
        integer(8) :: ans
        integer(8) :: m, n, p

        ! 正三角形の数 (60度)
        ans = limit / 3_8

        ! 1. 90度 (直角三角形)
        m = 2_8
        do while (2_8 * m * (m + 1_8) <= limit)
            do n = 1_8 + mod(m, 2_8), m - 1_8, 2_8
                p = 2_8 * m * (m + n)
                if (p > limit) exit
                if (gcd(m, n) == 1_8) then
                    ans = ans + limit / p
                end if
            end do
            m = m + 1_8
        end do

        ! 2. 120度
        m = 2_8
        do while ((2_8 * m + 1_8) * (m + 1_8) <= limit)
            do n = 1_8, m - 1_8
                if (mod(m - n, 3_8) == 0_8) cycle
                p = (2_8 * m + n) * (m + n)
                if (p > limit) exit
                if (gcd(m, n) == 1_8) then
                    ans = ans + limit / p
                end if
            end do
            m = m + 1_8
        end do

        ! 3. 60度 Type 1
        m = 2_8
        do while ((2_8 * m + 1_8) * (m + 2_8) <= limit)
            do n = 1_8, m - 1_8
                if (mod(m - n, 3_8) == 0_8) cycle
                p = (2_8 * m + n) * (m + 2_8 * n)
                if (p > limit) exit
                if (gcd(m, n) == 1_8) then
                    ans = ans + limit / p
                end if
            end do
            m = m + 1_8
        end do

        ! 4. 60度 Type 2
        m = 2_8
        do while (3_8 * m * (m + 1_8) <= limit)
            do n = 1_8, m - 1_8
                if (mod(m - n, 3_8) == 0_8) cycle
                p = 3_8 * m * (m + n)
                if (p > limit) exit
                if (gcd(m, n) == 1_8) then
                    ans = ans + limit / p
                end if
            end do
            m = m + 1_8
        end do

        solve_279_fortran = ans
    end function solve_279_fortran
end module solver
")

(defun compile-and-load-fortran-code ()
  (let* ((f90-file (uiop:native-namestring (merge-pathnames "pe279_solver.f90" (source-pathname))))
         (so-file (uiop:native-namestring (merge-pathnames 
                                            #-windows "libpe279_solver.so"
                                            #+windows "libpe279_solver.dll"
                                            (source-pathname)))))
    (with-open-file (out f90-file :direction :output :if-exists :supersede)
      (write-string *fortran-source* out))
    (uiop:run-program (format nil "gfortran -O3 -shared -fPIC -o ~A ~A" so-file f90-file)
                      :output *standard-output* :error-output *error-output*)
    (cffi:load-foreign-library so-file)))

(cffi:defcfun ("solve_279_fortran" c-solve-279) :int64
  (limit :int64))

(defun solve ()
  (format t "観測: Fortran共有ライブラリをコンパイルおよびロード中...~%")
  (compile-and-load-fortran-code)
  
  (format t "観測: テストケース T(10^5) を検証中...~%")
  (let ((ans-test (c-solve-279 100000)))
    (format t "観測: T(10^5) = ~D~%" ans-test))

  (format t "観測: 本探索 T(10^8) を実行中...~%")
  (let ((ans (c-solve-279 100000000)))
    (format t "Answer: ~D~%" ans)
    ans))

#+| Do it | (project-euler-0279:solve)
#|------------------------------------------------------------|
Timing the evaluation of (SOLVE)
観測: Fortran共有ライブラリをコンパイルおよびロード中...
観測: テストケース T(10^5) を検証中...
観測: T(10^5) = 250018
観測: 本探索 T(10^8) を実行中...
Answer: 416577688

User time    =        2.814
System time  =        0.029
Elapsed time =        2.795
Allocation   = 102032 bytes
360 Page faults
GC time      =        0.000
 |------------------------------------------------------------|#
;;→ 416577688
:ok