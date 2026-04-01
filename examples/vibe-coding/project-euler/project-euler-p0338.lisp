;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3.1-pro
(cl:in-package cl-user)
(defpackage #:project-euler-0338 (:use cl series alexandria) (:export #:solve))
(in-package #:project-euler-0338)

(defconstant +modulo-base+ 100000000)

(defun mod-choose-2 (m)
  "Computes m(m-1)/2 mod 10^8 securely."
  (prog ((m-mod (mod m (* 2 +modulo-base+))))
    (return (mod (floor (* m-mod (1- m-mod)) 2) +modulo-base+))))

(defun sum-M-choose-2 (N)
  "Computes Sum_{k=1}^N M_k(M_k-1)/2 in O(sqrt(N)) blocks using a perfectly flat prog."
  (prog ((sum 0) (sq (isqrt N)) (k 1) (M 0)
         \m k-start k-end count)
    L-K
      (when (> k sq)
        (setf (values \m) (floor N (1+ sq)))
        (setf (values k-start) (1+ sq))
        (go L-M))
      (setf (values M) (floor N k))
      (setf (values sum) (mod (+ sum (mod-choose-2 M)) +modulo-base+))
      (incf k)
      (go L-K)
    L-M
      (when (<= \m 0) (return sum))
      (setf (values k-end) (floor N \m))
      (setf (values count) (max 0 (1+ (- k-end k-start))))
      (when (> count 0)
        (setf (values sum) (mod (+ sum (* (mod count +modulo-base+) (mod-choose-2 m))) +modulo-base+))
        (setf (values k-start) (1+ k-end)))
      (decf \m)
      (go L-M)))

(defun sum-Yk-terms (N)
  "Computes the Y_k compensation sum in O(sqrt(N)). Returns a cons to avoid multiple value issues in prog."
  (prog ((sum-term 0) (sum-Yk 0) (k 1) (Yk 0) (Mk 0) (Mk1 0) (term 0))
    L-K
      (setf (values Yk) (floor N (* k (1+ k))))
      (when (<= Yk 0) (return (cons sum-term sum-Yk)))
      (setf (values Mk) (floor N k))
      (setf (values Mk1) (floor N (1+ k)))
      (setf (values sum-Yk) (mod (+ sum-Yk Yk) +modulo-base+))
      
      (setf (values term) (+ (* 2 (- Mk1 Mk)) Yk 3))
      (setf (values term) (mod (floor (* (mod Yk (* 2 +modulo-base+)) (mod term (* 2 +modulo-base+))) 2) +modulo-base+))
      (setf (values sum-term) (mod (+ sum-term term) +modulo-base+))
      
      (incf k)
      (go L-K)))

(defun compute-D (V)
  "Computes Dirichlet's divisor sum D_1(V) in O(sqrt(V))."
  (prog ((sq (isqrt V)) (sum 0) (i 1) (ans 0))
    L-START
      (when (> i sq)
        (setf (values ans) (mod (- (* 2 sum) (mod (* sq sq) +modulo-base+)) +modulo-base+))
        (when (< ans 0) (setf (values ans) (+ ans +modulo-base+)))
        (return ans))
      (setf (values sum) (mod (+ sum (floor V i)) +modulo-base+))
      (incf i)
      (go L-START)))

(defun compute-S2 (N u)
  "Computes the 2D Dirichlet intersection for D_3(N) efficiently."
  (prog ((sum 0) (x 1) (y 1) (sub-sum 0))
    L-X
      (when (> x u) (return sum))
      (setf (values sum) (mod (+ sum (floor N (* x x))) +modulo-base+))
      (setf (values y) 1)
      (setf (values sub-sum) 0)
    L-Y
      (when (>= y x)
        (setf (values sum) (mod (+ sum (* 2 sub-sum)) +modulo-base+))
        (incf x)
        (go L-X))
      (setf (values sub-sum) (mod (+ sub-sum (floor N (* x y))) +modulo-base+))
      (incf y)
      (go L-Y)))

(defun icbrt (n)
  "Exact Integer Cube Root via Binary Search."
  (prog ((low 0) (high (isqrt n)) (mid 0) (mid3 0))
    L-SEARCH
      (when (> low high) (return high))
      (setf (values mid) (floor (+ low high) 2))
      (setf (values mid3) (* mid mid mid))
      (cond ((= mid3 n) (return mid))
            ((< mid3 n) (setf (values low) (1+ mid)))
            (t (setf (values high) (1- mid))))
      (go L-SEARCH)))

(defun compute-D3 (N)
  "Computes 3D Dirichlet divisor sum D_3(N) in strictly O(N^{2/3})."
  (prog ((u (icbrt N)) (sum-D 0) (x 1) (s2 0) (u3 0) (ans 0))
    L-X
      (when (> x u)
        (setf (values s2) (compute-S2 N u))
        (setf (values u3) (mod (* u (mod (* u u) +modulo-base+)) +modulo-base+))
        (setf (values ans) (mod (+ (* 3 sum-D) u3 (- (mod (* 3 s2) +modulo-base+))) +modulo-base+))
        (when (< ans 0) (setf (values ans) (+ ans +modulo-base+)))
        (return ans))
      (setf (values sum-D) (mod (+ sum-D (compute-D (floor N x))) +modulo-base+))
      (incf x)
      (go L-X)))

(defun solve-euler-0338 (N)
  "Executes the ACX Jump combining O(sqrt(N)) blocks and O(N^{2/3}) hyperbola method."
  (prog ((s-m2 0) (sum-term 0) (sum-Yk 0) (M1 0) (sa-sb 0)
         (d3 0) (dn 0) (sd-sbb 0) (ans 0) (yk-cons nil))
    
    (setf (values s-m2) (sum-M-choose-2 N))
    (setf (values yk-cons) (sum-Yk-terms N))
    (setf (values sum-term) (car yk-cons))
    (setf (values sum-Yk) (cdr yk-cons))
    
    (setf (values M1) (floor N 1))
    
    (setf (values sa-sb) (mod (+ (* 2 s-m2) sum-term (- (mod-choose-2 M1))) +modulo-base+))
    (when (< sa-sb 0) (setf (values sa-sb) (+ sa-sb +modulo-base+)))
    
    (setf (values d3) (compute-D3 N))
    (setf (values dn) (compute-D N))
    
    (setf (values sd-sbb) (mod (+ d3 (mod N +modulo-base+) sum-Yk (- (mod (* 2 dn) +modulo-base+))) +modulo-base+))
    (when (< sd-sbb 0) (setf (values sd-sbb) (+ sd-sbb +modulo-base+)))
    
    (setf (values ans) (mod (- sa-sb sd-sbb) +modulo-base+))
    (when (< ans 0) (setf (values ans) (+ ans +modulo-base+)))
    (return ans)))

(defun solve ()
  (format t "--- Mathematical Grounding Validation ---~%")
  (format t "Testing G(10)... Expected: 55, Got: ~A~%" (solve-euler-0338 10))
  (format t "Testing G(10^3)... Expected: 971745, Got: ~A~%" (solve-euler-0338 1000))
  (format t "Testing G(10^5)... Expected: 92617687, Got: ~A~%" (solve-euler-0338 100000))
  (format t "-----------------------------------------~%")
  (format t "Solving for G(10^12)...~%")
  (let ((ans (solve-euler-0338 1000000000000)))
    (format t "Answer modulo 10^8: ~A~%" ans)
    ans))


#+| Do it | (solve )