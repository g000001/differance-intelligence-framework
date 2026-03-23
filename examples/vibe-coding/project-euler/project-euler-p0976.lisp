;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3-flash-preview
(cl:in-package cl-user)
(defpackage #:project-euler-0976 (:use cl iterate alexandria) (:export #:solve))
(in-package #:project-euler-0976)

#||
Project Euler 976: Game of Strips
[Mathematical Autopsy & Correction]
The previous assumption that this was an impartial game (Nim) solvable by FWT was fundamentally flawed.
Due to the strict coloring and symbol rules, X can only ever play on X-designated squares and O on O-designated squares.
The game is strictly a Partisan Cold Game (a Number Game) where the only thing that matters is the total number of valid moves each player can secure.
- Even-length strips always yield an equal number of moves for X and O. (Value = 0)
- Odd-length strips yield 1 extra move for the player who plays on them first.
Since playing on a blank strip is compulsory, the first phase is a draft where players greedily pick odd-length strips.
X wins if and only if X secures strictly more moves than O (T_X > T_O).
Because X plays first, this happens if and only if there is an ODD number of odd-length strips in the chosen tuple.
The problem collapses into pure combinatorics: How many multisets of size k <= N contain an odd number of odd elements?
Ans = \sum_{c=1, c is odd}^N \binom{M+c-1}{c} \binom{Z+N-c}{N-c}
where M = ceil(N/2) is the number of odd strip lengths, and Z = floor(N/2) is the number of even strip lengths.
The complexity instantly drops from O(N log N) FWT to a single O(N) summation.
||#

(defconstant $mod 1234567891)
(defconstant $n #.(expt 10 7))

(defparameter *fact* (make-array (1+ (* 2 $n)) :element-type 'fixnum))
(defparameter *inv-fact* (make-array (1+ (* 2 $n)) :element-type 'fixnum))

(defmacro mod+ (val-a val-b) `(mod (+ ,val-a ,val-b) $mod))
(defmacro mod* (val-a val-b) `(mod (* ,val-a ,val-b) $mod))

(defun mod-pow (base-val exp-val)
  (let ((result 1)
        (curr-base (mod base-val $mod))
        (curr-exp exp-val))
    (iterate (while (> curr-exp 0))
      (when (oddp curr-exp)
        (setf result (mod* result curr-base)))
      (setf curr-base (mod* curr-base curr-base)
            curr-exp (ash curr-exp -1)))
    result))

(defun mod-inv (val)
  (mod-pow val (- $mod 2)))

(defun precompute-factorials ()
  (setf (aref *fact* 0) 1)
  (iterate (for idx from 1 to (* 2 $n))
    (setf (aref *fact* idx) (mod* (aref *fact* (1- idx)) idx)))
  (setf (aref *inv-fact* (* 2 $n)) (mod-inv (aref *fact* (* 2 $n))))
  (iterate (for idx from (1- (* 2 $n)) downto 0)
    (setf (aref *inv-fact* idx) (mod* (aref *inv-fact* (1+ idx)) (1+ idx)))))

(defun ncr (n-val r-val)
  (if (or (< r-val 0) (> r-val n-val))
      0
      (mod* (aref *fact* n-val)
            (mod* (aref *inv-fact* r-val) (aref *inv-fact* (- n-val r-val))))))

(defun solve ()
  (precompute-factorials)
  (let* ((m-odds (ceiling $n 2))
         (z-evens (floor $n 2))
         (total-winning-tuples 0))
    
    ;; Sum over all odd counts 'c' of odd-length strips
    ;; \sum_{c is odd} \binom{M+c-1}{c} \binom{Z+N-c}{N-c}
    (iterate (for c-idx from 1 to $n by 2)
      (let* ((term-odds (ncr (+ m-odds c-idx -1) c-idx))
             (term-evens (ncr (+ z-evens $n c-idx) (- $n c-idx))) ; Hockey-stick simplified: \binom{Z+N-c}{N-c}
             (ways (mod* term-odds term-evens)))
        (setf total-winning-tuples (mod+ total-winning-tuples ways))))
        
    (format t "Log: Final Answer Modulo 1234567891 = ~A~%" total-winning-tuples)
    total-winning-tuples))


#+| Do it | (solve )