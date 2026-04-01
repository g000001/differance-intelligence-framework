;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3.1-pro
(cl:in-package cl-user)

(defpackage #:project-euler-0824 (:use cl series alexandria) (:export #:solve))
(in-package #:project-euler-0824)

(declaim (optimize (speed 3) (safety 0) (debug 0)))

(defparameter *p* 10000019)
(defparameter *p2* (* *p* *p*))

(defvar *inv* (make-array *p* :initial-element 0))
(defvar *H* (make-array *p* :initial-element 0))
(defvar *fact-p2* (make-array *p* :initial-element 0))
(defvar *inv-fact-p2* (make-array *p* :initial-element 0))

(defun add-p2 (A B)
  (let ((sum (+ A B)))
    (if (>= sum *p2*) (- sum *p2*) sum)))

(defun sub-p2 (A B)
  (let ((diff (- A B)))
    (if (< diff 0) (+ diff *p2*) diff)))

(defun mul-p2 (A B)
  "Multiplies A and B modulo p^2 using base-p decomposition to avoid large Bignums."
  (let* ((a0 (mod A *p*))
         (a1 (floor A *p*))
         (b0 (mod B *p*))
         (b1 (floor B *p*))
         (p0 (* a0 b0))
         (c0 (mod p0 *p*))
         (c1 (floor p0 *p*))
         (cross (mod (+ (* a0 b1) (* a1 b0)) *p*)))
    (+ c0 (* *p* (mod (+ c1 cross) *p*)))))

(defun inv-mod-p2 (a m)
  (let ((m0 m) (y 0) (x 1) (a0 a))
    (if (= m 1) 0
        (progn
          (loop while (> a0 1) do
            (multiple-value-bind (q r) (floor a0 m0)
              (setq a0 m0)
              (setq m0 r)
              (let ((t0 y))
                (setq y (- x (* q y)))
                (setq x t0))))
          (if (< x 0) (+ x m) x)))))

(defun inv-fixnum-p2 (x)
  "O(1) modular inverse mod p^2 using Hensel's lifting from mod p."
  (let* ((y (aref *inv* x))
         (xy (mul-p2 x y))
         (two-minus-xy (sub-p2 2 xy)))
    (mul-p2 y two-minus-xy)))

(defun precompute ()
  (format t "Precomputing modulo invariants...~%")
  (setf (aref *inv* 1) 1)
  (loop for i from 2 below *p* do
    (setf (aref *inv* i) (- *p* (* (floor *p* i) (aref *inv* (mod *p* i))))))
  
  (setf (aref *H* 0) 0)
  (loop for i from 1 below *p* do
    (setf (aref *H* i) (mod (+ (aref *H* (1- i)) (aref *inv* i)) *p*)))
    
  (setf (aref *fact-p2* 0) 1)
  (loop for i from 1 below *p* do
    (setf (aref *fact-p2* i) (mul-p2 (aref *fact-p2* (1- i)) i)))
    
  (setf (aref *inv-fact-p2* (1- *p*)) (inv-mod-p2 (aref *fact-p2* (1- *p*)) *p2*))
  (loop for i from (- *p* 2) downto 0 do
    (setf (aref *inv-fact-p2* i) (mul-p2 (aref *inv-fact-p2* (1+ i)) (1+ i))))
  (format t "Precomputation complete.~%"))

(defun binom-mod-p (n k)
  (if (or (< k 0) (> k n))
      0
      (let ((fn (mod (aref *fact-p2* n) *p*))
            (ik (mod (aref *inv-fact-p2* k) *p*))
            (ink (mod (aref *inv-fact-p2* (- n k)) *p*)))
        (mod (* fn (mod (* ik ink) *p*)) *p*))))

(defun lucas-mod-p (n k)
  (if (or (< k 0) (> k n))
      0
      (if (zerop n)
          1
          (mod (* (binom-mod-p (mod n *p*) (mod k *p*))
                  (lucas-mod-p (floor n *p*) (floor k *p*)))
               *p*))))

(defun binom-mod-p2-base (n0 k0)
  (if (or (< k0 0) (> k0 n0))
      0
      (mul-p2 (aref *fact-p2* n0)
              (mul-p2 (aref *inv-fact-p2* k0)
                      (aref *inv-fact-p2* (- n0 k0))))))

(defun binom-mod-p2 (n k)
  (cond
    ((or (< k 0) (> k n)) 0)
    ((zerop n) 1)
    (t
     (let* ((n0 (mod n *p*))
            (k0 (mod k *p*))
            (n1 (floor n *p*))
            (k1 (floor k *p*)))
       (if (>= n0 k0)
           (let ((ans1 (binom-mod-p2 n1 k1)))
             (if (zerop ans1)
                 0
                 (let* ((b0 (binom-mod-p2-base n0 k0))
                        (ans (mul-p2 ans1 b0))
                        (hn0 (aref *H* n0))
                        (hn0k0 (aref *H* (- n0 k0)))
                        (hk0 (aref *H* k0))
                        (term1 (mod (* (mod n1 *p*) (mod (- hn0 hn0k0) *p*)) *p*))
                        (term2 (mod (* (mod k1 *p*) (mod (- hn0k0 hk0) *p*)) *p*))
                        (H-term-p (mod (+ term1 term2) *p*)))
                   (if (zerop H-term-p)
                       ans
                       (mul-p2 ans (add-p2 1 (* *p* H-term-p)))))))
           ;; n0 < k0 branch with the EXACT mathematical fix (missing c1+1 factor added)
           (let ((ans1 (lucas-mod-p n1 (1+ k1))))
             (if (zerop ans1)
                 0
                 (let* ((c1-plus-1 (mod (1+ k1) *p*))
                        (sgn-val (if (oddp (- k0 n0)) 1 (1- *p*)))
                        (denom (mod (* k0 (binom-mod-p (1- k0) n0)) *p*))
                        (inv-denom (aref *inv* denom))
                        (val (mod (* c1-plus-1 ans1) *p*))
                        (val (mod (* val sgn-val) *p*))
                        (val (mod (* val inv-denom) *p*)))
                   (* *p* val)))))))))

(defun compute-L (n-val k-val)
  (let ((limit-j (floor k-val n-val))
        (binom-n-j 1)
        (total-sum 0))
    (dotimes (j (1+ limit-j))
      (let* ((m-j (- k-val (* n-val j)))
             (n-j (- (* n-val (- n-val j)) k-val 1)))
        (let* ((term1 (binom-mod-p2 (1+ n-j) m-j))
               (term2 (binom-mod-p2 n-j (1- m-j)))
               (s-j (add-p2 term1 term2)))
          (when (oddp m-j)
            (setq s-j (sub-p2 0 s-j)))
          (setq total-sum (add-p2 total-sum (mul-p2 binom-n-j s-j))))
        
        (when (< j limit-j)
          (let ((num (mod (- n-val j) *p2*))
                (den-inv (inv-fixnum-p2 (1+ j))))
            (setq binom-n-j (mul-p2 binom-n-j (mul-p2 num den-inv)))))))
    total-sum))

(defun solve ()
  (precompute)
  (format t "--- Mathematical Grounding Validation ---~%")
  (format t "Testing L(2, 2)... Expected: 4, Got: ~A~%" (compute-L 2 2))
  (format t "Testing L(6, 12)... Expected: 4204761, Got: ~A~%" (compute-L 6 12))
  (format t "-----------------------------------------~%")
  (format t "Solving for L(10^9, 10^15)...~%")
  (let ((ans (compute-L #.(expt 10 9) #.(expt 10 15))))
    (format t "Answer modulo (10^7+19)^2: ~A~%" ans)
    ans))


#+| Do it | (solve )
#|------------------------------------------------------------|
Timing the evaluation of (solve)
Precomputing modulo invariants...
Precomputation complete.
--- Mathematical Grounding Validation ---
Testing L(2, 2)... Expected: 4, Got: 4
Testing L(6, 12)... Expected: 4204761, Got: 4204761
-----------------------------------------
Solving for L(10^9, 10^15)...
Answer modulo (10^7+19)^2: 26532152736197

User time    =       16.668
System time  =        0.332
Elapsed time =       16.943
Allocation   = 2238456536 bytes
209800 Page faults
GC time      =        1.227
 |------------------------------------------------------------|#
;;→ 26532152736197
:ok