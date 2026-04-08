;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3-flash-preview
(cl:in-package cl-user)
(defpackage #:project-euler-0603 (:use cl iterate alexandria) (:export #:solve))
(in-package #:project-euler-0603)

(defmacro optimized-code-p (boole)
  (typecase boole
    (null nil)
    (T `(declaim (optimize (speed 3) (safety 0) (debug 0) #+lispworks (hcl:fixnum-safety 0))))))

(optimized-code-p T)

(defun my-mod-expt (base exp m)
  "Safe modular exponentiation."
  (let ((res 1))
    (setf base (mod base m))
    (iterate (while (> exp 0))
             (when (oddp exp)
               (setf res (mod (* res base) m)))
             (setf base (mod (* base base) m))
             (setf exp (ash exp -1)))
    res))

(defun my-mod-inverse (n m)
  "Modular inverse using Fermat's Little Theorem (m must be prime)."
  (my-mod-expt n (- m 2) m))

(defun generate-primes (n)
  "Generate the first N primes using a sieve."
  (let* ((limit 16000000) ; Approximately 10^6-th prime is 15.48M
         (sieve (make-array limit :element-type 'bit :initial-element 1)))
    (setf (bit sieve 0) 0 (bit sieve 1) 0)
    (iterate (for i from 2 below (isqrt limit))
             (when (= (bit sieve i) 1)
               (iterate (for j from (* i i) below limit by i)
                        (setf (bit sieve j) 0))))
    (let ((primes (make-array n :element-type '(unsigned-byte 32)))
          (count 0))
      (iterate (for i from 2 below limit)
               (while (< count n))
               (when (= (bit sieve i) 1)
                 (setf (aref primes count) i)
                 (incf count)))
      primes)))

(defun count-digits (n)
  "Return the number of digits in integer n."
  (if (zerop n) 1
      (iterate (for c initially 0 then (1+ c))
               (for v initially n then (truncate v 10))
               (until (zerop v))
               (finally (return c)))))

(defun get-digits (n)
  "Return digits of n as a list (most significant first)."
  (let ((res '()))
    (iterate (while (> n 0))
             (multiple-value-bind (q r) (truncate n 10)
               (push r res)
               (setf n q)))
    res))

(defun solve ()
  (let* ((n-primes #.(expt 10 6))
         (k-val #.(expt 10 12))
         (mod-m 1000000007)
         (mod-phi (1- mod-m))
         (primes (generate-primes n-primes))
         (total-l (iterate (for p in-vector primes) (sum (count-digits p))))
         (total-l-m (mod total-l mod-m))
         (sum-d 0)
         (sum-rd 0)
         (sum-d10 0)
         (sum-rd10 0)
         (inv10 (my-mod-inverse 10 mod-m))
         (p10 (my-mod-expt 10 (1- total-l) mod-m))
         (pos 1))
    
    (format t "Calculating P(10^6) statistics (Total digits L = ~A)...~%" total-l)
    
    (iterate (for p in-vector primes)
             (let ((ds (get-digits p)))
               (iterate (for d in ds)
                        (setf sum-d (mod (+ sum-d d) mod-m))
                        (setf sum-rd (mod (+ sum-rd (* pos d)) mod-m))
                        (let ((term (* d p10)))
                          (setf sum-d10 (mod (+ sum-d10 term) mod-m))
                          (setf sum-rd10 (mod (+ sum-rd10 (* pos term)) mod-m)))
                        (setf p10 (mod (* p10 inv10) mod-m))
                        (incf pos))))

    (format t "Statistics: SumD=~A, SumRD=~A, SumD10=~A, SumRD10=~A~%" sum-d sum-rd sum-d10 sum-rd10)

    (let* ((k-mod-m (mod k-val mod-m))
           (k-mod-phi (mod k-val mod-phi))
           (e (my-mod-expt 10 total-l mod-m))
           (inv-e-1 (my-mod-inverse (mod (1- e) mod-m) mod-m))
           (inv-e-1-sq (mod (* inv-e-1 inv-e-1) mod-m))
           (e-pow-k (my-mod-expt e k-mod-phi mod-m))
           (e-pow-k+1 (mod (* e-pow-k e) mod-m))
           ;; Geometric sums
           (g1 (mod (* (mod (1- e-pow-k) mod-m) inv-e-1) mod-m))
           (g2 (mod (* (mod (+ (mod (- (mod (* (mod (1- k-mod-m) mod-m) e-pow-k+1) mod-m)
                                       (mod (* k-mod-m e-pow-k) mod-m))
                                    mod-m)
                               e)
                            mod-m)
                       inv-e-1-sq)
                    mod-m))
           ;; Term1 = sum_{j=0}^{k-1} (k-1-j) E^j = (k-1)G1 - G2
           (term1 (mod (- (mod (* (mod (1- k-mod-m) mod-m) g1) mod-m) g2) mod-m))
           ;; A_total = 10 * [ L * SumD10 * Term1 + SumRD10 * G1 ]
           (a-total (mod (* 10 (mod (+ (mod (* (mod (* total-l-m sum-d10) mod-m) term1) mod-m)
                                       (mod (* sum-rd10 g1) mod-m))
                                    mod-m))
                         mod-m))
           ;; B_total = L * SumD * k(k-1)/2 + k * SumRD
           (k-k-1-2 (let ((k-1 (1- k-val)))
                      (if (evenp k-val)
                          (mod (* (mod (truncate k-val 2) mod-m) (mod k-1 mod-m)) mod-m)
                          (mod (* (mod k-val mod-m) (mod (truncate k-1 2) mod-m)) mod-m))))
           (b-total (mod (+ (mod (* (mod (* total-l-m sum-d) mod-m) k-k-1-2) mod-m)
                            (mod (* k-mod-m sum-rd) mod-m))
                         mod-m))
           ;; Final result S = (A_total - B_total) / 9
           (ans (mod (* (mod (- a-total b-total) mod-m) (my-mod-inverse 9 mod-m)) mod-m)))
      (format t "A_total: ~A, B_total: ~A~%" a-total b-total)
      (format t "Result: ~A~%" ans)
      ans)))

#+| Do it | (solve )
#|------------------------------------------------------------|
Timing the evaluation of (solve)
Calculating P(10^6) statistics (Total digits L = 7245905)...
Statistics: SumD=31352962, SumRD=508677097, SumD10=318174633, SumRD10=648163303
A_total: 506940940, B_total: 591652703
Result: 879476477

User time    =        0.645
System time  =        0.011
Elapsed time =        0.605
Allocation   = 122027520 bytes
2154 Page faults
GC time      =        0.003
 |------------------------------------------------------------|#
;;→ 879476477
:ok