;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3.1-pro
(cl:in-package cl-user)
(defpackage #:project-euler-0450 (:use cl series alexandria) (:export #:solve))
(in-package #:project-euler-0450)
(eval-when (:compile-toplevel :load-toplevel :execute) (series::install))

(defvar *min-prime-factor* nil)
(defvar *prime-1-mod-4-factors* nil)

(defun init-sieves (limit-n)
  (setq *min-prime-factor* (make-array (1+ limit-n) :element-type 'fixnum :initial-element 0))
  (setq *prime-1-mod-4-factors* (make-array (1+ (floor limit-n 3)) :initial-element nil))
  (loop for prime-cand from 2 to limit-n do
    (when (= (aref *min-prime-factor* prime-cand) 0)
      (loop for multiple from prime-cand to limit-n by prime-cand do
        (when (= (aref *min-prime-factor* multiple) 0)
          (setf (aref *min-prime-factor* multiple) prime-cand)))))
  (loop for prime-cand from 5 to (floor limit-n 3) by 2 do
    (when (and (= (mod prime-cand 4) 1)
               (= (aref *min-prime-factor* prime-cand) prime-cand))
      (loop for multiple from prime-cand to (floor limit-n 3) by prime-cand do
        (push prime-cand (aref *prime-1-mod-4-factors* multiple))))))

(defun get-prime-factors-fast (target-val)
  (let ((factor-list nil)
        (temp-val target-val))
    (loop while (> temp-val 1) do
      (let ((prime-factor (aref *min-prime-factor* temp-val)))
        (push prime-factor factor-list)
        (loop while (= (mod temp-val prime-factor) 0) do
          (setq temp-val (/ temp-val prime-factor)))))
    factor-list))

(defun get-ab-coefficients (mod-a mod-b)
  (let ((coeff-a 0) (coeff-b 0))
    (dotimes (index-m 4)
      (let ((cos-1 (round (cos (* index-m mod-b (/ pi 2)))))
            (cos-2 (round (cos (* -1 index-m mod-a (/ pi 2)))))
            (sin-1 (round (sin (* index-m mod-b (/ pi 2)))))
            (sin-2 (round (sin (* -1 index-m mod-a (/ pi 2))))))
        (if (/= cos-1 0)
            (progn (incf coeff-a (abs cos-1)) (incf coeff-b (* cos-2 (signum cos-1))))
            (incf coeff-b (abs cos-2)))
        (if (/= sin-1 0)
            (progn (incf coeff-a (abs sin-1)) (incf coeff-b (* sin-2 (signum sin-1))))
            (incf coeff-b (abs sin-2)))))
    (cons coeff-a coeff-b)))

(defun solve-congruence-mod4 (divisor target-mod)
  (let ((divisor-mod (mod divisor 4)))
    (cond ((= divisor-mod 1) (values 4 (mod target-mod 4)))
          ((= divisor-mod 3) (values 4 (mod (- target-mod) 4)))
          ((= divisor-mod 2)
           (if (= (mod target-mod 2) 0)
               (values 2 (mod (/ target-mod 2) 2))
               (values nil nil)))
          ((= divisor-mod 0)
           (if (= (mod target-mod 4) 0)
               (values 1 0)
               (values nil nil))))))

(defun sum-arithmetic-progression (start-exc end-inc power-k step-m offset-x0)
  (let* ((idx-min (1+ (floor (- start-exc offset-x0) step-m)))
         (idx-max (floor (- end-inc offset-x0) step-m)))
    (if (> idx-min idx-max)
        0
        (let ((count-terms (1+ (- idx-max idx-min))))
          (if (= power-k 0)
              count-terms
              (+ (* step-m (/ (* count-terms (+ idx-min idx-max)) 2))
                 (* offset-x0 count-terms)))))))

(defun compute-trivial-sum-for-s (sum-pq primes-list ab-table)
  (let ((total-for-s 0))
    (labels ((mobius-dfs (idx divisor mu-val)
               (if (= idx (length primes-list))
                   (dotimes (mod-c 4)
                     (multiple-value-bind (step-m offset-x0) (solve-congruence-mod4 divisor mod-c)
                       (when step-m
                         (let* ((term-count (sum-arithmetic-progression (/ sum-pq (* 2 divisor)) (/ sum-pq divisor) 0 step-m offset-x0))
                                (sum-x (sum-arithmetic-progression (/ sum-pq (* 2 divisor)) (/ sum-pq divisor) 1 step-m offset-x0))
                                (sum-p (* divisor sum-x))
                                (sum-q (- (* sum-pq term-count) sum-p)))
                           (let* ((mod-b (mod (- sum-pq mod-c) 4))
                                  (ab-pair (aref ab-table mod-c mod-b))
                                  (coeff-p (car ab-pair))
                                  (coeff-q (cdr ab-pair)))
                             (incf total-for-s (* mu-val (+ (* coeff-p sum-p) (* coeff-q sum-q)))))))))
                   (progn
                     (mobius-dfs (1+ idx) divisor mu-val)
                     (mobius-dfs (1+ idx) (* divisor (nth idx primes-list)) (- mu-val))))))
      (mobius-dfs 0 1 1)
      total-for-s)))

(defun count-prime-factor (target-n prime-p)
  (if (= target-n 0) 0
      (loop for count = 0 then (1+ count)
            for temp-val = target-n then (/ temp-val prime-p)
            while (= (mod temp-val prime-p) 0)
            finally (return count))))

(defun get-omega-generator (prime-p)
  (let ((limit-x (floor (sqrt prime-p))))
    (loop for coord-x from 1 to limit-x do
      (let* ((coord-y2 (- prime-p (* coord-x coord-x)))
             (coord-y (round (sqrt coord-y2))))
        (when (= (* coord-y coord-y) coord-y2)
          (return (complex (/ (- (* coord-x coord-x) (* coord-y coord-y)) prime-p)
                           (/ (* 2 coord-x coord-y) prime-p))))))))

(defun complex-expt-exact (base-val power-val)
  (cond ((= power-val 0) #c(1 0))
        ((< power-val 0) (complex-expt-exact (/ 1 base-val) (- power-val)))
        ((evenp power-val)
         (let ((half-val (complex-expt-exact base-val (ash power-val -1))))
           (* half-val half-val)))
        (t
         (let ((half-val (complex-expt-exact base-val (ash power-val -1))))
           (* half-val half-val base-val)))))

(defun process-extra-points (limit-n)
  (let ((extra-tuple-list nil)
        (extra-total-sum 0))
    (loop for gcd-g from 1 to (floor limit-n 3) do
      (let ((prime-factors (aref *prime-1-mod-4-factors* gcd-g)))
        (dolist (prime-p prime-factors)
          (let ((exp-v (count-prime-factor gcd-g prime-p)))
            (when (>= exp-v 2)
              (loop for val-p from 2 to (+ exp-v 2) do
                (loop for val-q from 1 to (1- val-p) do
                  (when (and (= (gcd val-p val-q) 1) (<= (* gcd-g (+ val-p val-q)) limit-n))
                    (let* ((vp-q (count-prime-factor val-q prime-p))
                           (vp-p (count-prime-factor val-p prime-p))
                           (k-max (min (floor (+ exp-v vp-q) val-p)
                                       (floor (+ exp-v vp-p) val-q))))
                      (when (> k-max 0)
                        (pushnew (list gcd-g val-p val-q) extra-tuple-list :test #'equal)))))))))))
    (dolist (tuple extra-tuple-list)
      (destructuring-bind (gcd-g val-p val-q) tuple
        (let ((prime-factors (aref *prime-1-mod-4-factors* gcd-g))
              (w-choices (list #c(1 0))))
          (dolist (prime-p prime-factors)
            (let* ((vp-g (count-prime-factor gcd-g prime-p))
                   (vp-q (count-prime-factor val-q prime-p))
                   (vp-p (count-prime-factor val-p prime-p))
                   (k-max (min (floor (+ vp-g vp-q) val-p)
                               (floor (+ vp-g vp-p) val-q))))
              (when (> k-max 0)
                (let ((new-choices nil)
                      (omega-p (get-omega-generator prime-p)))
                  (loop for exp-e from (- k-max) to k-max do
                    (let ((w-factor (complex-expt-exact omega-p exp-e)))
                      (dolist (choice w-choices)
                        (push (* choice w-factor) new-choices))))
                  (setq w-choices new-choices)))))
          (dolist (w-val w-choices)
            (unless (= w-val #c(1 0))
              (let ((sum-for-w 0))
                (dotimes (index-m 4)
                  (let* ((z-m (* (complex-expt-exact #c(0 1) index-m) w-val))
                         (z-power-q (complex-expt-exact z-m val-q))
                         (z-conj-power-p (complex-expt-exact (conjugate z-m) val-p))
                         (point-coord (+ (* val-p z-power-q) (* val-q z-conj-power-p))))
                    (incf sum-for-w (+ (abs (realpart point-coord)) (abs (imagpart point-coord))))))
                (incf extra-total-sum (* gcd-g sum-for-w))))))))
    extra-total-sum))

(defun solve-euler-450 (&optional (limit-n #.(expt 10 6)))
  (init-sieves limit-n)
  (let ((ab-table (make-array '(4 4)))
        (grand-total 0))
    (dotimes (mod-a 4)
      (dotimes (mod-b 4)
        (setf (aref ab-table mod-a mod-b) (get-ab-coefficients mod-a mod-b))))
    (loop for sum-s from 3 to limit-n do
      (let* ((primes-of-s (get-prime-factors-fast sum-s))
             (trivial-s (compute-trivial-sum-for-s sum-s primes-of-s ab-table))
             (max-g (floor limit-n sum-s))
             (g-sum (/ (* max-g (1+ max-g)) 2)))
        (incf grand-total (* g-sum trivial-s))))
    (+ grand-total (process-extra-points limit-n))))

(defun solve ()
  (format t "Testing N=3: ~A~%" (solve-euler-450 3))
  (format t "Testing N=10: ~A~%" (solve-euler-450 10))
  (format t "Testing N=100: ~A~%" (solve-euler-450 100))
  (format t "Testing N=1000: ~A~%" (solve-euler-450 1000))
  (format t "Solving for N=1000000...~%")
  (let ((ans (solve-euler-450 #.(expt 10 6))))
    (format t "Answer: ~A~%" ans)
    ans))


#+| Do it | (solve )
#|------------------------------------------------------------|
Timing the evaluation of (solve)
Testing N=3: 10
Testing N=10: 524
Testing N=100: 580442
Testing N=1000: 583108600
Solving for N=1000000...
Answer: 583333163984220940

User time    =       17.082
System time  =        0.051
Elapsed time =       17.003
Allocation   = 2284157864 bytes
10407 Page faults
GC time      =        0.038
 |------------------------------------------------------------|#
;;→ 583333163984220940
:ok