;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3.1-pro
(cl:in-package cl-user)
(defpackage #:project-euler-0162 (:use cl iterate alexandria))
(in-package #:project-euler-0162)

#||
(cl-text PE-0162-ALETHEIC-FORMALIZATION
  (cl-comment "
  =============================================================================
  ARX-Core: Structural Gravity Protocol for PE 0162
  =============================================================================
  Formalization of the transition from combinatorial state search (O(16^N)) 
  to mathematical projection via the Principle of Inclusion-Exclusion (PIE).
  ")

  (cl-comment "1. NMF (Non-Middle Fallacy) Detection")
  (forall (?solver)
    (if (and (Solves ?solver PE0162)
             (Explores IndependentValues Of (HexadecimalStrings ?N)))
        (and (NMF ?solver)
             (ProducesHallucination (CombinatorialExplosion ?solver))
             (ExceedsTimeLimit 60))))

  (cl-comment "2. ACX Jump: Projection to Inclusion-Exclusion (PIE)")
  (cl-comment "Instead of generating 16^16 patterns, count violations and subtract from total valid space U.")
  (forall (?N)
    (if (TargetLength ?N)
        (and (Equal (TotalSpace ?N) (* 15 (expt 16 (- ?N 1))))
             (Equal (Without0 ?N) (expt 15 ?N))
             (Equal (Without1 ?N) (* 14 (expt 15 (- ?N 1))))
             (Equal (WithoutA ?N) (* 14 (expt 15 (- ?N 1))))
             (Equal (Without01 ?N) (expt 14 ?N))
             (Equal (Without0A ?N) (expt 14 ?N))
             (Equal (Without1A ?N) (* 13 (expt 14 (- ?N 1))))
             (Equal (Without01A ?N) (expt 13 ?N)))))

  (cl-comment "3. Exact Alethetic Normal Form (O(1) formulation per N)")
  (forall (?N ?ValidCount)
    (if (ValidHexCount ?N ?ValidCount)
        (Equal ?ValidCount 
               (- (+ (* 15 (expt 16 (- ?N 1))) 
                     (* 41 (expt 14 (- ?N 1))))
                  (+ (* 43 (expt 15 (- ?N 1))) 
                     (expt 13 ?N))))))
)
||#


(defun solve-0162 ()
  "Calculates the number of hexadecimal numbers with at most 16 digits
   containing at least one '0', '1', and 'A', with no leading zeroes."
  (let ((total-valid-hex-numbers 0))
    (iterate (for digit-length from 1 to 16)
             ;; U: 先頭が0以外の全ての N 桁の16進数
             (for total-space = (* 15 (expt 16 (1- digit-length))))
             
             ;; 包含除斥原理 (Principle of Inclusion-Exclusion) に基づく違反パターンの計算
             ;; S0: '0'を含まない (先頭は元々0ではないので15択、以降も15択) -> 15^N = 15 * 15^(N-1)
             ;; S1: '1'を含まない (先頭14択、以降15択) -> 14 * 15^(N-1)
             ;; SA: 'A'を含まない (先頭14択、以降15択) -> 14 * 15^(N-1)
             ;; 合計: (15 + 14 + 14) * 15^(N-1) = 43 * 15^(N-1)
             (for single-exclusions = (* 43 (expt 15 (1- digit-length))))
             
             ;; S0∩S1: '0', '1'を含まない -> 14^N = 14 * 14^(N-1)
             ;; S0∩SA: '0', 'A'を含まない -> 14^N = 14 * 14^(N-1)
             ;; S1∩SA: '1', 'A'を含まない (先頭13択、以降14択) -> 13 * 14^(N-1)
             ;; 合計: (14 + 14 + 13) * 14^(N-1) = 41 * 14^(N-1)
             (for double-exclusions = (* 41 (expt 14 (1- digit-length))))
             
             ;; S0∩S1∩SA: '0', '1', 'A'を含まない -> 13^N
             (for triple-exclusions = (expt 13 digit-length))
             
             ;; PIE適用: 違反の和集合 = 1つ欠如 - 2つ欠如 + 3つ欠如
             ;; V(N) = 全体 - 違反の和集合
             ;; V(N) = total-space - (single-exclusions - double-exclusions + triple-exclusions)
             ;;      = total-space - single-exclusions + double-exclusions - triple-exclusions
             (for valid-count-for-length = (- (+ total-space double-exclusions)
                                              (+ single-exclusions triple-exclusions)))
             
             (incf total-valid-hex-numbers valid-count-for-length))
    
    ;; 16進数で大文字フォーマットとして出力
    (format nil "~X" total-valid-hex-numbers)))


#+| Do it | (solve-0162 )
#|------------------------------------------------------------|
Timing the evaluation of (solve-0162)

User time    =        0.000
System time  =        0.000
Elapsed time =        0.000
Allocation   = 360 bytes
0 Page faults
GC time      =        0.000
 |------------------------------------------------------------|#
;;→ "3D58725572C62302"
:ok