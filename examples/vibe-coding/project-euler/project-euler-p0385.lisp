;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3.5-pro
(cl:in-package cl-user)
(defpackage #:project-euler-0385 (:use cl iterate alexandria) (:export #:solve))
(in-package #:project-euler-0385)

(defun solve (&optional (n 1000000000))
  (declare (type fixnum n))
  (let ((total-area 0))
    (declare (type integer total-area)) ; Accumulated area easily exceeds fixnum bounds (10^20)

    (format t "Calculating A(~D) using Marden's Theorem and Pell's Equations...~%" n)

    ;; Case 1: q^2 - 3p^2 = 1
    ;; Generates exactly 4 distinct triangles per solution, each with area 117*p*q
    ;; Total area added = 468 * p * q
    ;; Bounding box limits: max X is 7q <= n, max Y is 12p <= n
    (let ((q 2) (p 1))
      (declare (type integer q p))
      (iterate
        (while (and (<= (* 7 q) n) (<= (* 12 p) n)))
        (let ((area (* 468 p q)))
          (incf total-area area)
          (format t "Case 1    [q^2-3p^2=1] : p=~10D, q=~10D, Added Area=~D~%" p q area)
          (let ((next-q (+ (* 2 q) (* 3 p)))
                (next-p (+ q (* 2 p))))
            (setf q next-q p next-p)))))

    ;; Case 2, Class 1: q^2 - 3p^2 = 13 (Fundamental solution 4, 1)
    ;; Generates exactly 2 distinct triangles per solution, each with area 9*p*q
    ;; Total area added = 18 * p * q
    ;; Bounding box limits: max X is 2q <= n, max Y is 3p <= n
    (let ((q 4) (p 1))
      (declare (type integer q p))
      (iterate
        (while (and (<= (* 2 q) n) (<= (* 3 p) n)))
        (let ((area (* 18 p q)))
          (incf total-area area)
          (format t "Case 2-C1 [q^2-3p^2=13]: p=~10D, q=~10D, Added Area=~D~%" p q area)
          (let ((next-q (+ (* 2 q) (* 3 p)))
               (next-p (+ q (* 2 p))))
            (setf q next-q p next-p)))))

    ;; Case 2, Class 2: q^2 - 3p^2 = 13 (Fundamental solution 5, 2)
    ;; Generates exactly 2 distinct triangles per solution, each with area 9*p*q
    ;; Total area added = 18 * p * q
    ;; Bounding box limits: max X is 2q <= n, max Y is 3p <= n
    (let ((q 5) (p 2))
      (declare (type integer q p))
      (iterate
        (while (and (<= (* 2 q) n) (<= (* 3 p) n)))
        (let ((area (* 18 p q)))
          (incf total-area area)
          (format t "Case 2-C2 [q^2-3p^2=13]: p=~10D, q=~10D, Added Area=~D~%" p q area)
          (let ((next-q (+ (* 2 q) (* 3 p)))
                (next-p (+ q (* 2 p))))
            (setf q next-q p next-p)))))

    (format t "Final ans = ~D~%" total-area)
    total-area))


#+| Do it | (solve )
#|------------------------------------------------------------|
Timing the evaluation of (solve)
Calculating A(1000000000) using Marden's Theorem and Pell's Equations...
Case 1    [q^2-3p^2=1] : p=         1, q=         2, Added Area=936
Case 1    [q^2-3p^2=1] : p=         4, q=         7, Added Area=13104
Case 1    [q^2-3p^2=1] : p=        15, q=        26, Added Area=182520
Case 1    [q^2-3p^2=1] : p=        56, q=        97, Added Area=2542176
Case 1    [q^2-3p^2=1] : p=       209, q=       362, Added Area=35407944
Case 1    [q^2-3p^2=1] : p=       780, q=      1351, Added Area=493169040
Case 1    [q^2-3p^2=1] : p=      2911, q=      5042, Added Area=6868958616
Case 1    [q^2-3p^2=1] : p=     10864, q=     18817, Added Area=95672251584
Case 1    [q^2-3p^2=1] : p=     40545, q=     70226, Added Area=1332542563560
Case 1    [q^2-3p^2=1] : p=    151316, q=    262087, Added Area=18559923638256
Case 1    [q^2-3p^2=1] : p=    564719, q=    978122, Added Area=258506388372024
Case 1    [q^2-3p^2=1] : p=   2107560, q=   3650401, Added Area=3600529513570080
Case 1    [q^2-3p^2=1] : p=   7865521, q=  13623482, Added Area=50148906801609096
Case 1    [q^2-3p^2=1] : p=  29354524, q=  50843527, Added Area=698484165708957264
Case 2-C1 [q^2-3p^2=13]: p=         1, q=         4, Added Area=72
Case 2-C1 [q^2-3p^2=13]: p=         6, q=        11, Added Area=1188
Case 2-C1 [q^2-3p^2=13]: p=        23, q=        40, Added Area=16560
Case 2-C1 [q^2-3p^2=13]: p=        86, q=       149, Added Area=230652
Case 2-C1 [q^2-3p^2=13]: p=       321, q=       556, Added Area=3212568
Case 2-C1 [q^2-3p^2=13]: p=      1198, q=      2075, Added Area=44745300
Case 2-C1 [q^2-3p^2=13]: p=      4471, q=      7744, Added Area=623221632
Case 2-C1 [q^2-3p^2=13]: p=     16686, q=     28901, Added Area=8680357548
Case 2-C1 [q^2-3p^2=13]: p=     62273, q=    107860, Added Area=120901784040
Case 2-C1 [q^2-3p^2=13]: p=    232406, q=    402539, Added Area=1683944619012
Case 2-C1 [q^2-3p^2=13]: p=    867351, q=   1502296, Added Area=23454322882128
Case 2-C1 [q^2-3p^2=13]: p=   3236998, q=   5606645, Added Area=326676575730780
Case 2-C1 [q^2-3p^2=13]: p=  12080641, q=  20924284, Added Area=4550017737348792
Case 2-C1 [q^2-3p^2=13]: p=  45085566, q=  78090491, Added Area=63373571747152308
Case 2-C1 [q^2-3p^2=13]: p= 168261623, q= 291437680, Added Area=882679986722783520
Case 2-C2 [q^2-3p^2=13]: p=         2, q=         5, Added Area=180
Case 2-C2 [q^2-3p^2=13]: p=         9, q=        16, Added Area=2592
Case 2-C2 [q^2-3p^2=13]: p=        34, q=        59, Added Area=36108
Case 2-C2 [q^2-3p^2=13]: p=       127, q=       220, Added Area=502920
Case 2-C2 [q^2-3p^2=13]: p=       474, q=       821, Added Area=7004772
Case 2-C2 [q^2-3p^2=13]: p=      1769, q=      3064, Added Area=97563888
Case 2-C2 [q^2-3p^2=13]: p=      6602, q=     11435, Added Area=1358889660
Case 2-C2 [q^2-3p^2=13]: p=     24639, q=     42676, Added Area=18926891352
Case 2-C2 [q^2-3p^2=13]: p=     91954, q=    159269, Added Area=263617589268
Case 2-C2 [q^2-3p^2=13]: p=    343177, q=    594400, Added Area=3671719358400
Case 2-C2 [q^2-3p^2=13]: p=   1280754, q=   2218331, Added Area=51140453428332
Case 2-C2 [q^2-3p^2=13]: p=   4779839, q=   8278924, Added Area=712294628638248
Case 2-C2 [q^2-3p^2=13]: p=  17838602, q=  30897365, Added Area=9920984347507140
Case 2-C2 [q^2-3p^2=13]: p=  66574569, q= 115310536, Added Area=138181486236461712
Case 2-C2 [q^2-3p^2=13]: p= 248459674, q= 430344779, Added Area=1924619822962956828
Final ans = 3776957309612153700

User time    =        0.002
System time  =        0.000
Elapsed time =        0.002
Allocation   = 28792 bytes
18 Page faults
GC time      =        0.000
 |------------------------------------------------------------|#
;;→ 3776957309612153700
:ok