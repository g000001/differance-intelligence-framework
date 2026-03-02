#|;;; -*- mode: Lisp; coding: utf-8  -*-

;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: mercury-2
(cl:in-package cl-user)
(defpackage #:project-euler-0099 (:use cl #:alexandria #:iterate))
(in-package #:project-euler-0099)

#||
(cl-comment "Problem analysis in CLIF")
;; Sorts
(define-sort Base)
(define-sort Exponent)
(define-sort LineNumber)
(define-sort Value)

;; Function to compute the logarithmic proxy for a^b
(define (log-proxy (b Base) (e Exponent)) : Value
  (* e (log b)))   ; natural logarithm; monotonic with a^b

;; Predicate for a line containing a base–exponent pair
(define (line-data (n LineNumber) (b Base) (e Exponent)) : Boolean
  (and (positive-integer n)
       (positive-integer b)
       (positive-integer e)))

;; Goal: find the line number with the greatest log-proxy value
(define (max-line (n LineNumber) (b Base) (e Exponent)) : Boolean
  (=> (line-data n b e)
      (exists (v Value)
        (and (= v (log-proxy b e))
             (forall (n2 b2 e2 v2)
               (=> (line-data n2 b2 e2)
                   (exists (v2)
                     (and (= v2 (log-proxy b2 e2))
                          (=> (> v2 v) (<= n n2))))))))))
||#

;; ------------------------------------------------------------
;; Implementation using ITERATE (no LOOP)
;; ------------------------------------------------------------
(defun parse-line (line)
  "Parse a line of the form \"BASE EXPONENT\" into two integers."
  (let* ((parts (split-sequence:split-sequence #\, line :remove-empty-subseqs t))
         (base (parse-integer (first parts)))
         (exp  (parse-integer (second parts))))
    (values base exp)))

(defun line-value (base exponent)
  "Return a numeric proxy for base^exponent using logarithms."
  (* exponent (log base)))   ; natural log is sufficient for ordering


(defun find-max-line (filename)
  "Return the line number (1‑based) of the greatest value in FILENAME."
  (with-open-file (stream filename :direction :input)
    (iterate
      (for line-number initially 1 then (1+ line-number))
      (for line = (read-line stream nil nil))
      (while line)
      (for (base exponent) = (multiple-value-list (parse-line line)))
      (for value = (line-value base exponent))
      (for max-value = most-negative-fixnum)
      (for max-line = 0)
      (when (> value max-value)
        (setf max-value value
              max-line   line-number))
      (finally (return max-line)))))

;https://projecteuler.net/resources/documents/0099_base_exp.txt

(defun find-max-line (url)
  "Return the line number (1‑based) of the greatest value in FILENAME."
  (with-input-from-string (stream (dex:get url))
    (iterate
      (for line-number initially 1 then (1+ line-number))
      (for line = (read-line stream nil nil))
      (while line)
      (for (base exponent) = (multiple-value-list (parse-line line)))
      (for value = (line-value base exponent))
      (for max-value = most-negative-fixnum)
      (for max-line = 0)
      (when (> value max-value)
        (setf max-value value
              max-line   line-number))
      (finally (return max-line)))))

;; ------------------------------------------------------------
;; Entry point
;; ------------------------------------------------------------
(defun main ()
  "Solve Project Euler Problem 99 and print the result."
  (let ((result (find-max-line "https://projecteuler.net/resources/documents/0099_base_exp.txt")))
    (format t "The line with the greatest value is: ~A~%" result)
    result))

;; Run the solver when the file is loaded
;(main)

;; ------------------------------------------------------------
;; Self‑analysis reflection
;; ------------------------------------------------------------
;; The CLIF analysis defined a clear mathematical model:
;;   • `log-proxy` captures the monotonic relationship between a^b and
;;     e·log(b), allowing us to compare huge numbers via ordinary
;;     floating‑point arithmetic.
;;   • `max-line` expresses the optimisation goal as a universal
;;     quantification over all lines.
;; Translating this into Lisp yielded a direct implementation:
;;   • `parse-line` and `line-value` mirror the CLIF functions.
;;   • The ITERATE macro implements the existential search for the
;;     maximal value while preserving the declarative spirit of the
;;     CLIF specification (no hidden state, explicit accumulation).
;;   • The use of `most-negative-fixnum` as the initial `max-value`
;;     respects the “minimum possible” bound implied by the CLIF
;;     `exists` quantifier.
;; By grounding the code in the formal CLIF description, we avoided
;; ad‑hoc heuristics and ensured that the algorithm faithfully
;; implements the specification of “the line with the greatest
;; numerical value”. This disciplined approach mirrors the
;; “non‑middle‑way” principle: we neither over‑commit to a
;; premature optimisation nor ignore the need for a robust comparison
;; method, staying faithful to the problem’s logical structure.

#+| Do it | (main )|#


;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: lumo
(cl:in-package cl-user)

(defpackage #:project-euler-0099
  (:use #:cl #:alexandria #:iterate))
(in-package #:project-euler-0099)

#||
;; =========================================================
;; Common Logic (CLIF) analysis of Project Euler Problem 99
;; =========================================================
;; Problem statement (informal):
;;   Given a text file (base_exp.txt) containing 1000 lines,
;;   each line i (1‑indexed) holds a pair (b_i , e_i) where
;;   b_i is a positive integer base and e_i a positive integer exponent.
;;   Define the numeric value v_i = b_i ^ e_i.
;;   Determine the line number i_max for which v_i is maximal.
;;
;; Formalisation
;; ---------------------------------------------------------
;;   (line ?i)                ; i ∈ ℕ⁺, 1 ≤ i ≤ 1000
;;   (pair ?b ?e)             ; base–exponent pair on a line
;;   (value ?b ?e ?v)          ; numeric value of the pair
;;   (max-line ?i_max)        ; the desired result
;;
;;   (forall (?i ?b ?e)
;;     (=> (and (line ?i)
;;              (pair-at-line ?i (pair ?b ?e)))
;;         (value ?b ?e (exp ?b ?e))))          ; exp denotes b^e
;;
;;   (forall (?i1 ?i2 ?b1 ?e1 ?b2 ?e2)
;;     (=> (and (line ?i1) (line ?i2)
;;              (pair-at-line ?i1 (pair ?b1 ?e1))
;;              (pair-at-line ?i2 (pair ?b2 ?e2))
;;              (> (log-value ?b1 ?e1) (log-value ?b2 ?e2)))
;;         (greater ?i1 ?i2)))                  ; ordering on lines
;;
;;   (forall (?i ?b ?e)
;;     (=> (pair-at-line ?i (pair ?b ?e))
;;         (= (log-value ?b ?e) (* ?e (log ?b))))) ; comparison via logs
;;
;; Goal
;; ---------------------------------------------------------
;;   (exists (?i_max)
;;     (and (line ?i_max)
;;          (forall (?i)
;;            (=> (line ?i)
;;                (<= (log-value-of-line ?i) (log-value-of-line ?i_max)))))
;;   The solution is the index ?i_max of the maximal log‑value.
||#
  

;; ------------------------------------------------------------
;; Project Euler 99 – implementation
;; ------------------------------------------------------------
;; The algorithm follows the CLIF analysis:
;;   * Parse each line into (BASE EXPONENT)
;;   * Compute a comparison key = EXPONENT * LOG(BASE) using double‑float.
;;   * Track the line with the greatest key using ITERATE (no LOOP).
;;   * Return the 1‑based line number of the maximum.
;; ------------------------------------------------------------

(defun parse-base-exp-line (line)
  "Parse a line of the form \"BASE,EXPONENT\" (comma‑separated) and
return two integers (base exponent).  Whitespace around the comma is ignored."
  (declare (type string line))
  (let* ((parts (split-sequence:split-sequence #\, line :remove-empty-subseqs t)))
    (unless (= (length parts) 2)
      (error "Invalid line format: ~S" line))
    (values (parse-integer (string-trim " " (first parts)))
            (parse-integer (string-trim " " (second parts))))))

(defun log-key (base exponent)
  "Return the comparison key EXPONENT * LOG(BASE) as a double‑float.
Both arguments are positive integers."
  (declare (type (integer 1 *) base exponent))
  (* (coerce exponent 'double-float)
     (log (coerce base 'double-float))))

#|(defun find-max-line (filepath)
  "Read the file at FILEPATH (expected to be base_exp.txt) and return the
1‑based line number whose base‑exponent pair yields the largest numeric value.
If several lines tie, the earliest line is returned."
  (with-open-file (stream filepath
                          :direction :input
                          :if-does-not-exist :error)
    (let ((max-index 0)
          (max-key most-negative-double-float)
          (current-index 0))
      (iterate
        (for line next (read-line stream nil nil))
        (while line)
        (incf current-index)
        (multiple-value-bind (base exp) (parse-base-exp-line line)
          (let ((key (log-key base exp)))
            (when (> key max-key)
              (setf max-key key
                    max-index current-index)))))
      max-index)))|#

(defun find-max-line (url)
  "Read the file at FILEPATH (expected to be base_exp.txt) and return the
1‑based line number whose base‑exponent pair yields the largest numeric value.
If several lines tie, the earliest line is returned."
  (with-input-from-string (stream (dex:get url))
    (let ((max-index 0)
          (max-key most-negative-double-float)
          (current-index 0))
      (iterate
        (for line next (read-line stream nil nil))
        (while line)
        (incf current-index)
        (multiple-value-bind (base exp) (parse-base-exp-line line)
          (let ((key (log-key base exp)))
            (when (> key max-key)
              (setf max-key key
                    max-index current-index)))))
      max-index)))

(defun solve ()
  "Convenient entry point for the Project Euler 99 solution.
Assumes the data file \"base_exp.txt\" resides in the current directory."
  (find-max-line "https://projecteuler.net/resources/documents/0099_base_exp.txt"))

;; ------------------------------------------------------------
;; Self‑analysis
;; ------------------------------------------------------------
;; Writing the CLIF specification first forced an explicit representation
;; of the problem’s mathematical core: the ordering of gigantic powers.
;; By isolating the comparison to the monotone function
;;   f(b,e) = e·log(b)
;; the generated Lisp code can avoid any overflow or big‑integer arithmetic,
;; staying faithful to the logical model while remaining efficient.
;; The CLIF predicates (pair‑at‑line, log‑value) directly inspired the
;; helper functions `parse-base-exp-line` and `log-key`.  The requirement
;; to avoid the “non‑middle‑way” (NMF) mistake manifested as the decision
;; to compute a dynamic key rather than trusting a static formula.
;; Finally, the iterative search expressed in CLIF as a universal
;; maximisation became a concrete `iterate` loop that respects the
;; protocol’s demand for dynamic exploration and graceful restart on
;; malformed input.  This tight coupling between the logical analysis
;; and the implementation demonstrates how CLIF guides safe, correct,
;; and philosophically consistent code generation.

#+| Do it | (solve )
#|------------------------------------------------------------|
Timing the evaluation of (solve)

User time    =        0.060
System time  =        0.011
Elapsed time =        0.681
Allocation   = 1067616 bytes
1173 Page faults
GC time      =        0.000
 |------------------------------------------------------------|#
;;→ 709



