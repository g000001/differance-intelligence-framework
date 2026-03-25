;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3.5-pro
(cl:in-package cl-user)
(defpackage #:project-euler-0941 (:use cl iterate alexandria) (:export #:solve))
(in-package #:project-euler-0941)

#||
(cl:comment "PE 941 Mathematical Constraints and Shortcuts (Optimized)")
(cl:comment "Invariant 1: The sequence C(k, n) is the lexicographically smallest de Bruijn sequence (FKM sequence), formed by concatenating all Lyndon words whose length divides n in lexicographical order.")
(cl:comment "Invariant 2: Any 12-digit word W appearing in C(10, 12) uniquely maps to a tuple (L, i), where L is a Lyndon word and i is the offset. The position of W corresponds exactly to the sorted lexicographical rank of its (L, i) pair.")
(cl:comment "Shortcut: We reverse-engineer the hidden Lyndon word L for each W in O(n) time. Since maximum length is 12, there are at most 28 combinations of (length, offset) to test via the FKM succession rules.")
(cl:comment "Optimization 1: We use a pure Duval algorithm in `generate-S` to deterministically simulate the sequence forward without infinite loops.")
(cl:comment "Optimization 2: We completely eliminate array allocations by caching L as a 64-bit 'Left-Aligned Integer' and a 'Length'. Lexicographical array comparison is mathematically identical to comparing these left-aligned integers, making the sort of 10^7 items hyper-fast with zero GC overhead.")
||#

(declaim (inline is-lyndon-word))

(defun is-lyndon-word (l-arr len)
  "Checks if the given array of digits of length 'len' is a Lyndon word."
  (iterate (for i from 1 below len)
    (let ((is-greater nil))
      (iterate (for j from 0 below len)
        (let ((c1 (aref l-arr j))
              (c2 (aref l-arr (mod (+ i j) len))))
          (cond ((< c1 c2)
                 (setf is-greater t)
                 (finish))
                ((> c1 c2)
                 (finish)))))
      (when (not is-greater)
        (return-from is-lyndon-word nil))))
  t)

(defun generate-S (l-arr res a)
  "Generates the prefix of the FKM sequence starting with L using Duval's algorithm."
  (setf (fill-pointer res) 0)
  (let ((d (length l-arr))
        (p (length l-arr)))
    (dotimes (j 12)
      (setf (aref a j) (aref l-arr (mod j d))))
    (dotimes (j d)
      (vector-push-extend (aref l-arr j) res))
    
    (iterate (while (< (fill-pointer res) 24))
      (let ((pos 11))
        (iterate (while (and (>= pos 0) (= (aref a pos) 9)))
          (decf pos))
        (if (< pos 0)
            (progn
              (dotimes (j 24)
                (vector-push-extend 0 res))
              (return res))
            (progn
              (incf (aref a pos))
              (let ((new-p (1+ pos)))
                (iterate (for j from new-p to 11)
                  (setf (aref a j) (aref a (- j new-p))))
                (setf p new-p)
                (when (zerop (mod 12 p))
                  (dotimes (j p)
                    (vector-push-extend (aref a j) res))))))))
    res))

(defun find-L-i (w-arr temp-l temp-res temp-a)
  "Finds the unique Lyndon word L and offset i, returning (Aligned-Val, Length, i)."
  (let ((divisors '(1 2 3 4 6 12)))
    (dolist (d divisors)
      (dotimes (i d)
        (labels ((check-candidate (len)
                   (when (is-lyndon-word temp-l len)
                     (generate-S temp-l temp-res temp-a)
                     (let ((match t))
                       (dotimes (j 12)
                         (when (not (= (aref temp-res (+ i j)) (aref w-arr j)))
                           (setf match nil)
                           (return)))
                       (when match
                         ;; Compress L into a left-aligned integer for fast O(1) lexicographical comparisons
                         (let ((aligned-val 0))
                           (dotimes (j len)
                             (setf aligned-val (+ (* aligned-val 10) (aref temp-l j))))
                           (dotimes (j (- 12 len))
                             (setf aligned-val (* aligned-val 10)))
                           (return-from find-L-i (values aligned-val len i))))))))
          
          (if (<= d 6)
              (progn
                (setf (fill-pointer temp-l) d)
                (dotimes (j i)
                  (setf (aref temp-l j) (aref w-arr (+ (- d i) j))))
                (dotimes (j (- d i))
                  (setf (aref temp-l (+ i j)) (aref w-arr j)))
                (check-candidate d))
              
              (progn
                (setf (fill-pointer temp-l) 12)
                (let ((all-nines t))
                  (dotimes (j (- 12 i))
                    (when (not (= (aref w-arr j) 9))
                      (setf all-nines nil)
                      (return)))
                  
                  (if (not all-nines)
                      (progn
                        (dotimes (j i)
                          (setf (aref temp-l j) (aref w-arr (+ (- 12 i) j))))
                        (dotimes (j (- 12 i))
                          (setf (aref temp-l (+ i j)) (aref w-arr j)))
                        (check-candidate 12))
                      
                      (progn
                        ;; Complex wraparound increment logic for bridging two Lyndon words
                        (iterate (for k from 0 below i)
                          (when (> (aref w-arr (+ (- 12 i) k)) 0)
                            (dotimes (j k)
                              (setf (aref temp-l j) (aref w-arr (+ (- 12 i) j))))
                            (setf (aref temp-l k) (1- (aref w-arr (+ (- 12 i) k))))
                            (iterate (for j from (1+ k) below i)
                              (setf (aref temp-l j) 9))
                            (dotimes (j (- 12 i))
                              (setf (aref temp-l (+ i j)) (aref w-arr j)))
                            (check-candidate 12)))))))))))
    (values 0 1 0)))

(defun solve ()
  (let* ((n 10000000)
         (modulo 1234567891)
         (a-vals (make-array (1+ n) :element-type '(unsigned-byte 64)))
         (l-aligned (make-array (1+ n) :element-type '(unsigned-byte 64)))
         (l-lens (make-array (1+ n) :element-type '(unsigned-byte 8)))
         (i-vals (make-array (1+ n) :element-type '(unsigned-byte 8)))
         (indices (make-array n :element-type '(unsigned-byte 32)))
         ;; Reusable buffers to completely eliminate inner-loop allocations
         (w-arr (make-array 12 :element-type 'fixnum))
         (temp-l (make-array 12 :element-type 'fixnum :adjustable t :fill-pointer 0))
         (temp-res (make-array 36 :element-type 'fixnum :adjustable t :fill-pointer 0))
         (temp-a (make-array 12 :element-type 'fixnum)))
    
    (format t "Generating the sequence a_j for N = 10^7...~%")
    (setf (aref a-vals 0) 0)
    (iterate (for j from 1 to n)
      (setf (aref a-vals j) (mod (+ (* 920461 (aref a-vals (1- j))) 800217387569) 1000000000000))
      (setf (aref indices (1- j)) j))
      
    (format t "Decoding positions (L, i) dynamically using FKM sequence rules...~%")
    (iterate (for j from 1 to n)
      (let ((val (aref a-vals j)))
        (iterate (for k from 11 downto 0)
          (multiple-value-bind (q r) (truncate val 10)
            (setf (aref w-arr k) r)
            (setf val q)))
        (multiple-value-bind (aligned len idx) (find-L-i w-arr temp-l temp-res temp-a)
          (setf (aref l-aligned j) aligned)
          (setf (aref l-lens j) len)
          (setf (aref i-vals j) idx))))
          
    (format t "Sorting 10^7 items with hyper-fast zero-allocation integer comparisons...~%")
    (setf indices (sort indices (lambda (x y)
                                  (let ((vx (aref l-aligned x))
                                        (vy (aref l-aligned y)))
                                    (if (< vx vy) t
                                        (if (> vx vy) nil
                                            (let ((lx (aref l-lens x))
                                                  (ly (aref l-lens y)))
                                              (if (< lx ly) t
                                                  (if (> lx ly) nil
                                                      (< (aref i-vals x) (aref i-vals y)))))))))))
                                     
    (format t "Calculating the final score F(N)...~%")
    (let ((f-score 0))
      (iterate (for rank from 1 to n)
        (let* ((original-idx (aref indices (1- rank)))
               (val (aref a-vals original-idx))
               (term (mod (* rank (mod val modulo)) modulo)))
          (setf f-score (mod (+ f-score term) modulo))))
          
      (format t "Final Answer F(~A) mod ~A: ~A~%" n modulo f-score)
      f-score)))

#+| Do it | (solve )
#|------------------------------------------------------------|
Timing the evaluation of (solve)
Generating the sequence a_j for N = 10^7...
Decoding positions (L, i) dynamically using FKM sequence rules...
Sorting 10^7 items with hyper-fast zero-allocation integer comparisons...
Calculating the final score F(N)...
Final Answer F(10000000) mod 1234567891: 1068765750

User time    =  0:05:32.989
System time  =        6.030
Elapsed time =  0:07:44.471
Allocation   = 225830688 bytes
88773 Page faults
GC time      =        0.007
 |------------------------------------------------------------|#
;;→ 1068765750
:ok