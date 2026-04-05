;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3-flash-preview
(cl:in-package cl-user)
(defpackage #:project-euler-0319 (:use cl iterate alexandria) (:export #:solve))
(in-package #:project-euler-0319)

(defmacro optimized-code-p (boole)
  (typecase boole
    (null nil)
    (T `(declaim (optimize (speed 3) (safety 0) (debug 0) #+lispworks (hcl:fixnum-safety 0))))))

(optimized-code-p nil)


(defconstant $const-limit-n #.(expt 10 10))
(defconstant $const-modulo #.(expt 10 9))
(defconstant $const-limit-k 5000000)

(defun mod-expt (base-val power-val modulo-val)
  (let ((result-val 1)
        (current-base (mod base-val modulo-val))
        (current-power power-val))
    (iterate
      (while (> current-power 0))
      (when (oddp current-power)
        (setf result-val (mod (* result-val current-base) modulo-val)))
      (setf current-base (mod (* current-base current-base) modulo-val))
      (setf current-power (ash current-power -1)))
    result-val))

(defun s3-mod (left-bound right-bound modulo-val)
  (let* ((mod-double (* 2 modulo-val))
         (term-right (mod-expt 3 (1+ right-bound) mod-double))
         (term-left (mod-expt 3 left-bound mod-double))
         (diff-val (mod (- term-right term-left) mod-double)))
    (/ diff-val 2)))

(defun mod-sum-g (left-bound right-bound modulo-val)
  (let ((sum-ones (mod (+ (- right-bound left-bound) 1) modulo-val))
        (sum-twos (mod (- (mod-expt 2 (1+ right-bound) modulo-val)
                          (mod-expt 2 left-bound modulo-val))
                       modulo-val))
        (sum-threes (s3-mod left-bound right-bound modulo-val)))
    (mod (- sum-threes sum-twos sum-ones) modulo-val)))

(defun solve ()
  (format t "Starting PE 319...~%")
  (let* ((mu-array (make-array (1+ $const-limit-k) :element-type '(signed-byte 8) :initial-element 0))
         (m-pre-array (make-array (1+ $const-limit-k) :element-type '(signed-byte 32) :initial-element 0))
         (primes-array (make-array (truncate $const-limit-k 5) :element-type '(unsigned-byte 32) :fill-pointer 0))
         (is-prime-array (make-array (1+ $const-limit-k) :element-type 'bit :initial-element 1))
         (memo-array (make-array (+ (floor $const-limit-n $const-limit-k) 5) :initial-element nil)))
    
    (format t "Initializing Sieve...~%")
    (setf (sbit is-prime-array 0) 0)
    (setf (sbit is-prime-array 1) 0)
    (setf (aref mu-array 1) 1)
    
    (iterate
      (for index from 2 to $const-limit-k)
      (when (= (sbit is-prime-array index) 1)
        (vector-push index primes-array)
        (setf (aref mu-array index) -1))
      (iterate
        (for prime-val in-vector primes-array)
        (while (<= (* index prime-val) $const-limit-k))
        (setf (sbit is-prime-array (* index prime-val)) 0)
        (if (= (mod index prime-val) 0)
            (progn
              (setf (aref mu-array (* index prime-val)) 0)
              (leave))
            (setf (aref mu-array (* index prime-val)) (- (aref mu-array index))))))
            
    (format t "Computing Prefix Sums...~%")
    (setf (aref m-pre-array 0) 0)
    (iterate
      (for index from 1 to $const-limit-k)
      (setf (aref m-pre-array index) (+ (aref m-pre-array (1- index)) (aref mu-array index))))
      
    (format t "Defining recursive M function...~%")
    (labels ((get-M (val-X)
               (if (<= val-X $const-limit-k)
                   (aref m-pre-array val-X)
                   (let ((idx-memo (floor $const-limit-n val-X)))
                     (if (aref memo-array idx-memo)
                         (aref memo-array idx-memo)
                         (let ((ans-val 1))
                           (iterate
                             (with left-bound = 2)
                             (while (<= left-bound val-X))
                             (let* ((quotient-V (floor val-X left-bound))
                                    (right-bound (floor val-X quotient-V)))
                               (decf ans-val (* (+ (- right-bound left-bound) 1) (get-M quotient-V)))
                               (setf left-bound (1+ right-bound))))
                           (setf (aref memo-array idx-memo) ans-val)))))))
                           
      (format t "Computing final answer...~%")
      (let ((total-ans 1))
        (iterate
          (with left-bound = 1)
          (while (<= left-bound $const-limit-n))
          (let* ((quotient-V (floor $const-limit-n left-bound))
                 (right-bound (floor $const-limit-n quotient-V)))
            (let ((sum-g (mod-sum-g left-bound right-bound $const-modulo)))
              (setf total-ans (mod (+ total-ans (* sum-g (get-M quotient-V))) $const-modulo)))
            (setf left-bound (1+ right-bound))))
        (format t "Done. The answer is: ~A~%" (mod total-ans $const-modulo))
        (mod total-ans $const-modulo)))))


#+| Do it | (solve )
#|------------------------------------------------------------|
Timing the evaluation of (solve)
Starting PE 319...
Initializing Sieve...
Computing Prefix Sums...
Defining recursive M function...
Computing final answer...
Done. The answer is: 268457129

User time    =        1.812
System time  =        0.038
Elapsed time =        1.792
Allocation   = 80276472 bytes
22633 Page faults
GC time      =        0.040
 |------------------------------------------------------------|#
;;→ 268457129
:ok