;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3.1-pro
(cl:in-package cl-user)
(defpackage #:project-euler-0806 (:use cl series alexandria) (:export #:solve))
(in-package #:project-euler-0806)
(eval-when (:compile-toplevel :load-toplevel :execute) (series::install))

(declaim (optimize (speed 3) (safety 0) (debug 0)))

(defconstant $mod-value 1000000007)

(defun mod-add (val-a val-b)
  (declare (type fixnum val-a val-b))
  (let ((sum (+ val-a val-b)))
    (if (>= sum $mod-value)
        (- sum $mod-value)
        sum)))

(defun mod-mul (val-a val-b)
  (declare (type fixnum val-a val-b))
  (mod (* val-a val-b) $mod-value))

(defun get-hanoi-index (c0-init c1-init c2-init num-disks)
  "Deterministic finite automaton to recover the Hanoi index from peg counts.
   Runs in exactly O(N) steps. Returns the index modulo 10^9+7, or NIL if invalid."
  (declare (type fixnum c0-init c1-init c2-init num-disks))
  (let ((counts (make-array 3 :element-type 'fixnum :initial-contents (list c0-init c1-init c2-init)))
        (peg-s 0)
        (peg-d 2)
        (peg-o 1)
        (index-mod 0)
        (power-of-2 1)) ;; Actually, we need powers of 2 from top to bottom.
    (declare (type fixnum peg-s peg-d peg-o index-mod))
    
    ;; Precompute powers of 2 mod 10^9+7 to accumulate the index from top (n-1) to bottom (0).
    (let ((powers (make-array num-disks :element-type 'fixnum)))
      (setf (aref powers 0) 1)
      (do ((k 1 (1+ k)))
          ((>= k num-disks))
        (setf (aref powers k) (mod-mul (aref powers (1- k)) 2)))
        
      (do ((k (1- num-disks) (1- k)))
          ((< k 0))
        (let ((c-s (aref counts peg-s))
              (c-d (aref counts peg-d)))
          (declare (type fixnum c-s c-d))
          
          (cond
            ;; If c_d is 0, the disk MUST have been placed on peg_s. (Bit = 0)
            ((zerop c-d)
             (when (< c-s 1) (return-from get-hanoi-index nil))
             (decf (aref counts peg-s))
             ;; Next state transition for Bit 0
             (let ((next-s peg-s)
                   (next-d peg-o)
                   (next-o peg-d))
               (setf peg-s next-s peg-d next-d peg-o next-o)))
               
            ;; If c_s is 0, the disk MUST have been placed on peg_d. (Bit = 1)
            ((zerop c-s)
             (when (< c-d 1) (return-from get-hanoi-index nil))
             (decf (aref counts peg-d))
             (setf index-mod (mod-add index-mod (aref powers k)))
             ;; Next state transition for Bit 1
             (let ((next-s peg-o)
                   (next-d peg-d)
                   (next-o peg-s))
               (setf peg-s next-s peg-d next-d peg-o next-o)))
               
            ;; If BOTH > 0, this configuration NEVER appears on the shortest path.
            (t (return-from get-hanoi-index nil)))))
            
      ;; Final verification: all counts must be exactly exhausted.
      (if (and (zerop (aref counts 0)) (zerop (aref counts 1)) (zerop (aref counts 2)))
          index-mod
          nil))))

(defun generate-candidates-and-sum (num-disks)
  "Generates the 3^P valid (c0, c1, c2) Nim candidates and sums their Hanoi indices."
  (declare (type fixnum num-disks))
  (let* ((half-n (ash num-disks -1))
         (total-sum 0)
         ;; Extract positions of 1s in half-n
         (set-bits nil)
         (temp half-n)
         (bit-pos 0))
    (declare (type fixnum half-n total-sum temp bit-pos))
    
    (do () ((zerop temp))
      (when (oddp temp)
        (push bit-pos set-bits))
      (setf temp (ash temp -1))
      (incf bit-pos))
      
    (let ((num-set-bits (length set-bits)))
      ;; Generate 3^(num-set-bits) combinations using series mapping
      (iterate ((combo (scan-range :from 0 :upto (1- (expt 3 num-set-bits)))))
        (declare (type fixnum combo))
        (let ((c0 0)
              (c1 0)
              (current-combo combo))
          (declare (type fixnum c0 c1 current-combo))
          
          ;; For bits where half-n is 0, both c0 and c1 are 0.
          ;; For bits where half-n is 1, (c0,c1) can be (0,1), (1,0), or (1,1).
          (dolist (pos set-bits)
            (let ((choice (mod current-combo 3)))
              (setf current-combo (floor current-combo 3))
              (case choice
                (0 (setf c1 (logior c1 (ash 1 pos))))           ;; (0, 1)
                (1 (setf c0 (logior c0 (ash 1 pos))))           ;; (1, 0)
                (2 (progn                                       ;; (1, 1)
                     (setf c0 (logior c0 (ash 1 pos)))
                     (setf c1 (logior c1 (ash 1 pos))))))))
                     
          (let ((c2 (logxor c0 c1)))
            (declare (type fixnum c2))
            (let ((index-mod (get-hanoi-index c0 c1 c2 num-disks)))
              (when index-mod
                (setf total-sum (mod-add total-sum index-mod))))))))
    total-sum))

(defun solve ()
  (format t "--- Mathematical Grounding Validation ---~%")
  (format t "Testing f(4)... Expected: 30, Got: ~A~%" (generate-candidates-and-sum 4))
  (format t "Testing f(10)... Expected: 67518, Got: ~A~%" (generate-candidates-and-sum 10))
  (format t "-----------------------------------------~%")
  (format t "Solving for f(10^5)...~%")
  (let ((ans (generate-candidates-and-sum 100000)))
    (format t "Answer modulo 10^9+7: ~A~%" ans)
    ans))