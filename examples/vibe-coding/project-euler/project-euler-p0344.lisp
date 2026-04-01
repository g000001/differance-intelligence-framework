;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3.1-pro
(cl:in-package cl-user)
(defpackage #:project-euler-0344 (:use cl series alexandria) (:export #:solve))
(in-package #:project-euler-0344)

(defconstant +modulo-p+ 1000036000099)

(defun exact-binom (n m)
  "Computes binomial coefficient dynamically securely over Bignums."
  (prog ((res 1) (i 1))
    (when (> m (- n m))
      (setf (values m) (- n m)))
    L-LOOP
      (when (> i m) (return res))
      (setf (values res) (floor (* res (- (1+ n) i)) i))
      (incf i)
      (go L-LOOP)))

(defun compute-binom-array (a P)
  "Precomputes coefficients Binom(a, 2j) mod P."
  (prog ((max-j (floor a 2))
         (binom (make-array (1+ (floor a 2)) :initial-element 0))
         (j 0)
         (val 1))
    L-BINOM
      (when (> j max-j) (return binom))
      (setf (aref binom j) (mod val P))
      (setf (values val) (floor (* val (- a (* 2 j)) (- a (1+ (* 2 j))))
                                (* (1+ (* 2 j)) (+ 2 (* 2 j)))))
      (incf j)
      (go L-BINOM)))

(defun compute-D (S_max binom P)
  "Computes D(S), the number of ways 'a' gaps sum to S with XOR sum 0.
   Runs DP in O(a * S_max) time safely using a flat state machine."
  (prog ((max-j (1- (length binom)))
         (D (make-array (1+ S_max) :initial-element 0))
         (S 2)
         (K 0)
         (sum 0)
         (j 0)
         (idx 0))
    (setf (aref D 0) 1)
    L-OUTER
      (when (> S S_max) (return D))
      (setf (values K) (ash S -1))
      (setf (values sum) 0)
      (setf (values j) (logand K 1))
    L-INNER
      (when (> j max-j) (go L-INNER-END))
      (setf (values idx) (- K j))
      (when (>= idx 0)
        (setf (values sum) (mod (+ sum (* (aref binom j) (aref D idx))) P)))
      (incf j 2)
      (go L-INNER)
    L-INNER-END
      (setf (aref D S) sum)
      (incf S 2)
      (go L-OUTER)))

(defun compute-L (D M S_max c0 c1 b P)
  "Orchestrates Stars and Bars combination natively against the P-position density."
  (prog ((C (make-array (1+ b) :initial-element 0))
         (V 0)
         (S 0)
         (term 0)
         (L 0)
         (k 0)
         (val-S 0)
         (val-Sm1 0))
    (setf (aref C 0) 1)
    L-V
      (when (> V M) (return L))
      
      (setf (values S) (- M V))
      (when (<= S S_max)
        (setf (values val-S) (if (oddp S) 0 (aref D S)))
        (setf (values val-Sm1) (if (or (< S 1) (oddp (1- S))) 0 (aref D (1- S))))
        
        (setf (values term) (mod (+ (* c0 val-S) (* c1 val-Sm1)) P))
        (setf (values L) (mod (+ L (* term (aref C b))) P)))
      
      (setf (values k) b)
    L-K
      (when (< k 1) (go L-K-END))
      (setf (aref C k) (mod (+ (aref C k) (aref C (1- k))) P))
      (decf k)
      (go L-K)
    L-K-END
      
      (incf V)
      (go L-V)))

(defun solve-euler-0344 (n c P)
  "Executes the ACX Jump collapsing De Bruijn's Silver Dollar variant."
  (prog ((m (1+ c))
         (a 0) (b 0) (c0 0) (c1 0)
         (S_max 0) (M-val 0)
         (binom-arr nil)
         (D-arr nil)
         (L 0) (T-val 0) (W 0))
    
    (setf (values a) (1+ (floor c 2)))
    (setf (values b) (ceiling c 2))
    ;; Using derived combinatorial boundary coefficients
    (setf (values c0) (1+ (floor c 2)))
    (setf (values c1) (floor c 2))
    
    (setf (values S_max) (- n m))
    (setf (values M-val) (+ S_max b))
    
    (setf (values binom-arr) (compute-binom-array a P))
    (setf (values D-arr) (compute-D S_max binom-arr P))
    
    (setf (values L) (compute-L D-arr M-val S_max c0 c1 b P))
    (setf (values T-val) (mod (* m (exact-binom n m)) P))
    
    (setf (values W) (mod (- T-val L) P))
    (when (< W 0) (incf W P))
    (return W)))

(defun solve ()
  (format t "--- Mathematical Grounding Validation ---~%")
  ;; Utilizing an astronomically large P defined safely via Lisp reader macros
  (format t "Testing W(10, 2)... Expected: 324, Got: ~A~%" 
          (solve-euler-0344 10 2 #.(expt 10 30)))
  (format t "Testing W(100, 10)... Expected: 1514704946113500, Got: ~A~%" 
          (solve-euler-0344 100 10 #.(expt 10 30)))
  (format t "-----------------------------------------~%")
  (format t "Solving for W(10^6, 100)...~%")
  (let ((ans (solve-euler-0344 1000000 100 +modulo-p+)))
    (format t "Answer modulo 1000036000099: ~A~%" ans)
    ans))