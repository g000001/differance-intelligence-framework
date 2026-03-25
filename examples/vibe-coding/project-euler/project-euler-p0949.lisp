;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3.5-pro
(cl:in-package cl-user)
(defpackage #:project-euler-0949 (:use cl iterate alexandria) (:export #:solve))
(in-package #:project-euler-0949)

#||
(cl:comment "PE 949 Mathematical Constraints and Shortcuts")
(cl:comment "Invariant 1: The ability to move on ANY subset of components simultaneously converts the game from a Disjunctive Sum to a Mapping Game.")
(cl:comment "Invariant 2: In this specific Mapping Game, players can unilaterally destroy opponent's safe moves. This perfectly collapses the game value of any word into a Dyadic Rational (Conway Number).")
(cl:comment "Constraint 1: Any word ending in L guarantees Left a win for that word (Value +1). Any word starting with R and ending in R guarantees Right a win (Value -1).")
(cl:comment "Constraint 2: Words of the form L...R act as Hot Potatoes, their exact fractional value can be determined by the average of their optimal Left and Right sub-options.")
(cl:comment "Shortcut: We compute the exact CGT value v(w) for all 2^n words using a polynomial DP, scale them to integers, and then use a 1D array convolution to find all combinations of k=7 words where the sum is <= 0.")
||#

(defun compute-word-values (n)
  "Computes the integer-scaled game value V(w) = v(w) * 2^(n-1) for all 2^n words."
  (let* ((num-words (ash 1 n))
         (values (make-array num-words :element-type 'integer :initial-element 0))
         (scale (ash 1 (1- n))))
    ;; Base cases: Length 1 words are implicit in the bit representation
    ;; We build up values by length
    (iterate (for len from 1 to n)
      (let ((current-scale (ash 1 (- n len))))
        (iterate (for bits from 0 below (ash 1 len))
          ;; bits: 0 represents L, 1 represents R
          (let ((starts-with-r (= (logand (ash bits (- 1 len)) 1) 1))
                (ends-with-l (= (logand bits 1) 0)))
            (cond
              (ends-with-l
               ;; Any word ending in L is exactly +1 for Left
               (setf (aref values bits) scale))
              (starts-with-r
               ;; Any word starting with R and ending with R is exactly -1 for Right
               (setf (aref values bits) (- scale)))
              (t
               ;; Word is L...R. Compute average of optimal sub-moves.
               ;; Left removes prefix (i.e., takes a suffix starting with L)
               ;; Right removes suffix (i.e., takes a prefix ending with R)
               (let ((max-l (- scale))
                     (min-r scale))
                 ;; Left options: suffixes
                 (iterate (for i from 1 to (1- len))
                   ;; Suffix starts at index i (0-indexed)
                   (let ((suffix-len (- len i)))
                     (when (= (logand (ash bits (- 1 suffix-len)) 1) 0) ; Starts with L
                       (let* ((mask (1- (ash 1 suffix-len)))
                              (suffix-bits (logand bits mask))
                              (val (aref values suffix-bits)))
                         (when (> val max-l) (setf max-l val))))))
                 ;; Right options: prefixes
                 (iterate (for j from 1 to (1- len))
                   ;; Prefix ends at index j-1
                   (let ((prefix-len j))
                     (when (= (logand (ash bits (- len prefix-len)) 1) 1) ; Ends with R
                       (let* ((prefix-bits (ash bits (- prefix-len len)))
                              (val (aref values prefix-bits)))
                         (when (< val min-r) (setf min-r val))))))
                 
                 ;; For a Hot game in this specific ruleset, value is exactly the midpoint
                 (setf (aref values bits) (ash (+ max-l min-r) -1)))))))))
    values))

(defun solve ()
  (let* ((n 20)
         (k 7)
         (modulo 1001001011)
         (values (compute-word-values n))
         (value-counts (make-hash-table :test 'eql)))
    
    (format t "Computed exact CGT fractional values for all 2^~A words.~%" n)
    
    ;; Group by exact integer value
    (iterate (for i from 0 below (length values))
      (incf (gethash (aref values i) value-counts 0)))
      
    (let* ((unique-values (hash-table-keys value-counts))
           (min-val (* k (reduce #'min unique-values)))
           (max-val (* k (reduce #'max unique-values)))
           (offset (- min-val))
           (dp-size (1+ (+ max-val offset)))
           (dp (make-array dp-size :element-type '(unsigned-byte 62) :initial-element 0))
           (next-dp (make-array dp-size :element-type '(unsigned-byte 62) :initial-element 0)))
           
    (format t "Compressed into ~A unique strategic values. Starting 1D convolution...~%" (length unique-values))
    
    (setf (aref dp offset) 1) ; Base state: sum = 0
    
    (iterate (for step from 1 to k)
      (fill next-dp 0)
      (iterate (for i from 0 below dp-size)
        (let ((count (aref dp i)))
          (when (> count 0)
            (maphash (lambda (val freq)
                       (let ((next-i (+ i val)))
                         (setf (aref next-dp next-i)
                               (mod (+ (aref next-dp next-i) (mod (* count freq) modulo)) modulo))))
                     value-counts))))
      (rotatef dp next-dp))
      
    (format t "Convolution finished. Sifting Right-winning games (Sum <= 0)...~%")
    (let ((ans 0))
      ;; Right wins if the total scaled sum is <= 0
      (iterate (for i from 0 to offset)
        (setf ans (mod (+ ans (aref dp i)) modulo)))
        
      (format t "Final Answer G(~A, ~A): ~A~%" n k ans)
      ans))))


#+| Do it | (solve )
:ng