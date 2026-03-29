;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3-flash-preview
(cl:in-package cl-user)
(defpackage #:project-euler-0070 (:use cl series alexandria) (:export #:solve))
(in-package #:project-euler-0070)
(eval-when (:compile-toplevel :load-toplevel :execute) (series::install))


;; ------------------------------------------------------------
;; Helper utilities
;; ------------------------------------------------------------

(defun is-prime? (number)
  "Returns true if NUMBER is prime."
  (if (<= number 1)
      nil
      (let ((limit-index (isqrt number)))
        (not (collect-or (mapping ((index-i (scan-range :from 2 :upto limit-index)))
                           (zerop (mod number index-i))))))))

(defun get-primes (limit-number)
  "Returns a list of prime numbers strictly less than LIMIT-NUMBER."
  (collect (choose-if #'is-prime? (scan-range :from 2 :below limit-number))))

(defun integer-digits (number)
  "Returns a list of the decimal digits of NUMBER."
  (labels ((recursive-extract (accumulator current-value)
             (if (zerop current-value)
                 accumulator
                 (multiple-value-bind (quotient remainder) (truncate current-value 10)
                   (recursive-extract (cons remainder accumulator) quotient)))))
    (if (zerop number) '(0) (recursive-extract nil number))))

(defun is-permutation? (number-a number-b)
  "True iff the decimal representations of NUMBER-A and NUMBER-B are permutations of each other."
  (equal (sort (integer-digits number-a) #'<)
         (sort (integer-digits number-b) #'<)))

;; ------------------------------------------------------------
;; Main solver
;; ------------------------------------------------------------

(defun solve ()
  "Finds the value of n (1 < n < 10^7) for which phi(n) is a permutation of n
   and the ratio n/phi(n) produces a minimum."
  (let* ((limit-prime 10000) ; Upper bound for prime factors (sqrt(10^7) ~ 3162)
         (primes (get-primes limit-prime))
         (best-candidate-n 0)
         (best-candidate-phi 1))
    
    (iterate ((prime-1 (scan primes)))
      (iterate ((prime-2 (scan primes)))
        (when (< prime-1 prime-2)
          (let* ((candidate-n (* prime-1 prime-2))
                 (candidate-phi (* (1- prime-1) (1- prime-2))))
            ;; Constraint check and Alethetic leap (mod 9 equivalence)
            (when (and (< candidate-n #.(expt 10 7))
                       (= (mod candidate-n 9) (mod candidate-phi 9))
                       (is-permutation? candidate-n candidate-phi)
                       (or (zerop best-candidate-n)
                           (< (* candidate-n best-candidate-phi) (* best-candidate-n candidate-phi))))
              ;; Intermediate debug log
              (format t "Found candidate: n=~A, phi=~A, prime-1=~A, prime-2=~A~%" 
                      candidate-n candidate-phi prime-1 prime-2)
              (setf best-candidate-n candidate-n
                    best-candidate-phi candidate-phi))))))
    best-candidate-n))

#+| Do it | (project-euler-0070:solve)
#|------------------------------------------------------------|
Timing the evaluation of (solve)
Found candidate: n=502, phi=250, prime-1=2, prime-2=251
Found candidate: n=2518, phi=1258, prime-1=2, prime-2=1259
Found candidate: n=21, phi=12, prime-1=3, prime-2=7
Found candidate: n=291, phi=192, prime-1=3, prime-2=97
Found candidate: n=2991, phi=1992, prime-1=3, prime-2=997
Found candidate: n=5367, phi=3576, prime-1=3, prime-2=1789
Found candidate: n=5637, phi=3756, prime-1=3, prime-2=1879
Found candidate: n=4435, phi=3544, prime-1=5, prime-2=887
Found candidate: n=44305, phi=35440, prime-1=5, prime-2=8861
Found candidate: n=49435, phi=39544, prime-1=5, prime-2=9887
Found candidate: n=87109, phi=79180, prime-1=11, prime-2=7919
Found candidate: n=35683, phi=33568, prime-1=17, prime-2=2099
Found candidate: n=119221, phi=112192, prime-1=17, prime-2=7013
Found candidate: n=167569, phi=157696, prime-1=17, prime-2=9857
Found candidate: n=22471, phi=21472, prime-1=23, prime-2=977
Found candidate: n=157021, phi=150172, prime-1=23, prime-2=6827
Found candidate: n=201679, phi=196720, prime-1=41, prime-2=4919
Found candidate: n=243007, phi=237040, prime-1=41, prime-2=5927
Found candidate: n=20617, phi=20176, prime-1=53, prime-2=389
Found candidate: n=45421, phi=44512, prime-1=53, prime-2=857
Found candidate: n=69271, phi=67912, prime-1=53, prime-2=1307
Found candidate: n=114109, phi=111904, prime-1=53, prime-2=2153
Found candidate: n=94813, phi=93148, prime-1=59, prime-2=1607
Found candidate: n=140083, phi=138040, prime-1=71, prime-2=1973
Found candidate: n=440413, phi=434140, prime-1=71, prime-2=6203
Found candidate: n=84283, phi=83248, prime-1=89, prime-2=947
Found candidate: n=199627, phi=197296, prime-1=89, prime-2=2243
Found candidate: n=736297, phi=727936, prime-1=89, prime-2=8273
Found candidate: n=746539, phi=739456, prime-1=107, prime-2=6977
Found candidate: n=779281, phi=771892, prime-1=107, prime-2=7283
Found candidate: n=212101, phi=210112, prime-1=113, prime-2=1877
Found candidate: n=162619, phi=161296, prime-1=137, prime-2=1187
Found candidate: n=224269, phi=222496, prime-1=137, prime-2=1637
Found candidate: n=261259, phi=259216, prime-1=137, prime-2=1907
Found candidate: n=525121, phi=521152, prime-1=137, prime-2=3833
Found candidate: n=1210669, phi=1201696, prime-1=137, prime-2=8837
Found candidate: n=182401, phi=181204, prime-1=179, prime-2=1019
Found candidate: n=504601, phi=501604, prime-1=179, prime-2=2819
Found candidate: n=818053, phi=813580, prime-1=191, prime-2=4283
Found candidate: n=1399291, phi=1391992, prime-1=197, prime-2=7103
Found candidate: n=1576969, phi=1569796, prime-1=227, prime-2=6947
Found candidate: n=1876867, phi=1868776, prime-1=239, prime-2=7853
Found candidate: n=1884067, phi=1876480, prime-1=257, prime-2=7331
Found candidate: n=1921741, phi=1914172, prime-1=263, prime-2=7307
Found candidate: n=1368403, phi=1363048, prime-1=269, prime-2=5087
Found candidate: n=1362157, phi=1357216, prime-1=293, prime-2=4649
Found candidate: n=2042503, phi=2035240, prime-1=293, prime-2=6971
Found candidate: n=1021057, phi=1017520, prime-1=317, prime-2=3221
Found candidate: n=579067, phi=577096, prime-1=359, prime-2=1613
Found candidate: n=2649211, phi=2641912, prime-1=383, prime-2=6917
Found candidate: n=3703669, phi=3693760, prime-1=389, prime-2=9521
Found candidate: n=2580397, phi=2573980, prime-1=431, prime-2=5987
Found candidate: n=1980547, phi=1975840, prime-1=467, prime-2=4241
Found candidate: n=4082047, phi=4072840, prime-1=467, prime-2=8741
Found candidate: n=4227019, phi=4217920, prime-1=491, prime-2=8609
Found candidate: n=2340853, phi=2335840, prime-1=521, prime-2=4493
Found candidate: n=4600951, phi=4591600, prime-1=521, prime-2=8831
Found candidate: n=3952843, phi=3945328, prime-1=569, prime-2=6947
Found candidate: n=5525431, phi=5515432, prime-1=587, prime-2=9413
Found candidate: n=5845003, phi=5835400, prime-1=653, prime-2=8951
Found candidate: n=6400393, phi=6390340, prime-1=683, prime-2=9371
Found candidate: n=3194809, phi=3189904, prime-1=773, prime-2=4133
Found candidate: n=3518341, phi=3513184, prime-1=809, prime-2=4349
Found candidate: n=3940639, phi=3934960, prime-1=809, prime-2=4871
Found candidate: n=6484507, phi=6475840, prime-1=827, prime-2=7841
Found candidate: n=8428921, phi=8418292, prime-1=863, prime-2=9767
Found candidate: n=7923571, phi=7913752, prime-1=887, prime-2=8933
Found candidate: n=9376567, phi=9365776, prime-1=953, prime-2=9839
Found candidate: n=7665937, phi=7657396, prime-1=1019, prime-2=7523
Found candidate: n=9080017, phi=9070180, prime-1=1031, prime-2=8807
Found candidate: n=9687607, phi=9677680, prime-1=1097, prime-2=8831
Found candidate: n=7665883, phi=7658368, prime-1=1217, prime-2=6299
Found candidate: n=9983167, phi=9973816, prime-1=1229, prime-2=8123
Found candidate: n=8769067, phi=8760976, prime-1=1289, prime-2=6803
Found candidate: n=7841203, phi=7834120, prime-1=1373, prime-2=5711
Found candidate: n=8579401, phi=8571904, prime-1=1409, prime-2=6089
Found candidate: n=8252341, phi=8245132, prime-1=1427, prime-2=5783
Found candidate: n=9179251, phi=9171592, prime-1=1487, prime-2=6173
Found candidate: n=9848203, phi=9840328, prime-1=1559, prime-2=6317
Found candidate: n=7276201, phi=7270612, prime-1=2063, prime-2=3527
Found candidate: n=7507321, phi=7501732, prime-1=2243, prime-2=3347
Found candidate: n=8316907, phi=8310976, prime-1=2273, prime-2=3659
Found candidate: n=8357821, phi=8351872, prime-1=2273, prime-2=3677
Found candidate: n=8319823, phi=8313928, prime-1=2339, prime-2=3557

User time    =        0.126
System time  =        0.012
Elapsed time =        0.083
Allocation   = 6054664 bytes
3737 Page faults
GC time      =        0.002
 |------------------------------------------------------------|#
;;→ 8319823
:ok