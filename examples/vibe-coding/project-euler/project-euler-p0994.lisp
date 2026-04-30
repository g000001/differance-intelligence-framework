;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3-pro
(cl:in-package cl-user)
(defpackage #:project-euler-0994 (:use cl iterate alexandria) (:export #:solve))
(in-package #:project-euler-0994)

(defmacro optimized-code-p (boole)
  (typecase boole
    (null nil)
    (T `(declaim (optimize (speed 3) (safety 0) (debug 0))))))

(optimized-code-p T)


(defconstant $mod 1000000007)
(defconstant $limit 20000000)
(defconstant $inv-2 500000004)
(defconstant $inv-6 166666668)
(defconstant $inv-9 111111112)

(defvar *phi0-arr*)
(defvar *phi1-arr*)
(defvar *phi2-arr*)

(defvar *phi0-memo* (make-hash-table :test 'eql))
(defvar *phi1-memo* (make-hash-table :test 'eql))
(defvar *phi2-memo* (make-hash-table :test 'eql))

(defun build-phis ()
  (let ((phi (make-array (1+ $limit) :element-type '(unsigned-byte 32) :initial-element 0))
        (phi0 (make-array (1+ $limit) :element-type '(unsigned-byte 32)))
        (phi1 (make-array (1+ $limit) :element-type '(unsigned-byte 32)))
        (phi2 (make-array (1+ $limit) :element-type '(unsigned-byte 32))))
    (iterate (for i from 1 to $limit)
      (setf (aref phi i) i))
    (iterate (for i from 2 to $limit)
      (when (= (aref phi i) i)
        (iterate (for j from i to $limit by i)
          (setf (aref phi j) (- (aref phi j) (truncate (aref phi j) i))))))
    (setf (aref phi0 0) 0
          (aref phi1 0) 0
          (aref phi2 0) 0)
    (iterate (for i from 1 to $limit)
      (let* ((p-val (aref phi i))
             (p1 (mod (* p-val i) $mod))
             (p2 (mod (* p1 i) $mod)))
        (setf (aref phi0 i) (mod (+ (aref phi0 (1- i)) p-val) $mod)
              (aref phi1 i) (mod (+ (aref phi1 (1- i)) p1) $mod)
              (aref phi2 i) (mod (+ (aref phi2 (1- i)) p2) $mod))))
    (values phi0 phi1 phi2)))

(defun init-phis ()
  (multiple-value-bind (p0 p1 p2) (build-phis)
    (setf *phi0-arr* p0
          *phi1-arr* p1
          *phi2-arr* p2)
    (clrhash *phi0-memo*)
    (clrhash *phi1-memo*)
    (clrhash *phi2-memo*)))

(defun s0 (x)
  (mod x $mod))

(defun s1 (x)
  (let ((x-mod (mod x $mod)))
    (mod (* x-mod (1+ x-mod) $inv-2) $mod)))

(defun s2 (x)
  (let ((x-mod (mod x $mod)))
    (mod (* x-mod (1+ x-mod) (1+ (* 2 x-mod)) $inv-6) $mod)))

(defun s3 (x)
  (let ((v (s1 x)))
    (mod (* v v) $mod)))

(defmacro def-get-phi (name arr memo s-ans s-sum)
  `(defun ,name (x)
     (if (<= x $limit)
         (aref ,arr x)
         (or (gethash x ,memo)
             (let ((ans (,s-ans x))
                   (l-var 2))
               (iterate (while (<= l-var x))
                 (let* ((q (truncate x l-var))
                        (r-var (truncate x q))
                        (sum-c (mod (- (,s-sum r-var) (,s-sum (1- l-var))) $mod)))
                   (setf ans (mod (- ans (* sum-c (,name q))) $mod))
                   (setf l-var (1+ r-var))))
               (setf (gethash x ,memo) (mod ans $mod)))))))

(def-get-phi get-phi0 *phi0-arr* *phi0-memo* s1 s0)
(def-get-phi get-phi1 *phi1-arr* *phi1-memo* s2 s1)
(def-get-phi get-phi2 *phi2-arr* *phi2-memo* s3 s2)

(defun calc-s (m-val n-val)
  (let* ((m-prime (1- m-val))
         (n-prime (1- n-val))
         (k-var 1)
         (total-s 0)
         (limit-k (min m-prime n-prime)))
    (iterate (while (<= k-var limit-k))
      (let* ((i-m (truncate m-prime k-var))
             (i-n (truncate n-prime k-var))
             (r-m (if (> i-m 0) (truncate m-prime i-m) limit-k))
             (r-n (if (> i-n 0) (truncate n-prime i-n) limit-k))
             (r-var (min r-m r-n limit-k)))
        (let* ((a-val (mod (* (mod m-val $mod) (mod i-m $mod)) $mod))
               (b-val (s1 i-m))
               (c-val (mod (* (mod n-val $mod) (mod i-n $mod)) $mod))
               (d-val (s1 i-n))
               (c0 (mod (* a-val c-val) $mod))
               (c1-neg (mod (+ (* a-val d-val) (* b-val c-val)) $mod))
               (c2 (mod (* b-val d-val) $mod))
               (sum0 (mod (- (get-phi0 r-var) (get-phi0 (1- k-var))) $mod))
               (sum1 (mod (- (get-phi1 r-var) (get-phi1 (1- k-var))) $mod))
               (sum2 (mod (- (get-phi2 r-var) (get-phi2 (1- k-var))) $mod))
               (term0 (mod (* c0 sum0) $mod))
               (term1 (mod (* c1-neg sum1) $mod))
               (term2 (mod (* c2 sum2) $mod))
               (term (mod (+ (- term0 term1) term2) $mod)))
          (setf total-s (mod (+ total-s term) $mod))
          (setf k-var (1+ r-var)))))
    ;; k=1 の時の寄与（共点が発生しないベース成分）を控除する
    (let* ((fm1 (mod (* (mod m-val $mod) (mod m-prime $mod) $inv-2) $mod))
           (fn1 (mod (* (mod n-val $mod) (mod n-prime $mod) $inv-2) $mod))
           (sub (mod (* fm1 fn1) $mod)))
      (mod (- total-s sub) $mod))))

(defun calc-t-base (m-val n-val)
  (let* ((m-mod (mod m-val $mod))
         (n-mod (mod n-val $mod))
         (fm1 (mod (* m-mod (mod (1- m-val) $mod) $inv-2) $mod))
         (fn1 (mod (* n-mod (mod (1- n-val) $mod) $inv-2) $mod))
         (term3 (mod (+ (* m-mod n-mod) (* 4 m-mod) (* 4 n-mod) -2) $mod))
         (term3-div9 (mod (* term3 $inv-9) $mod)))
    (mod (* fm1 fn1 term3-div9) $mod)))

(defun solve-mn (m-val n-val)
  (let ((t-base (calc-t-base m-val n-val))
        (s-total (calc-s m-val n-val)))
    (mod (- t-base s-total) $mod)))

(defun solve ()
  (format t "Initializing Mertens/Dirichlet Sieve Arrays...~%")
  (init-phis)
  (format t "Performing verifications against emptiness...~%")
  (format t "T(2,3) = ~A~%" (solve-mn 2 3))
  (format t "T(3,5) = ~A~%" (solve-mn 3 5))
  (format t "T(12,23) = ~A~%" (solve-mn 12 23))
  (format t "Solving target...~%")
  (let ((m-val (* 1234 #.(expt 10 8)))
        (n-val (* 2345 #.(expt 10 8))))
    (format t "Target: ~A~%" (solve-mn m-val n-val))))


#+| Do it | (solve )
#|------------------------------------------------------------|
Timing the evaluation of (solve)
Initializing Mertens/Dirichlet Sieve Arrays...
Performing verifications against emptiness...
T(2,3) = 8
T(3,5) = 146
T(12,23) = 756716
Solving target...
Target: 350247268

User time    =  0:03:53.723
System time  =        9.788
Elapsed time =  0:07:43.681
Allocation   = 16183970904 bytes
90875 Page faults
GC time      =        0.380
 |------------------------------------------------------------|#
;;→ nil
:ok