;;; -*- mode: Lisp; coding: utf-8  -*-
>;;; llm-model: claude-sonnet-4-6
(cl:in-package cl-user)
(defpackage #:project-euler-0091 (:use cl iterate alexandria) (:export #:solve))
(in-package #:project-euler-0091)

(defmacro optimized-code-p (boole)
  (typecase boole
    (null nil)
    (T `(declaim (optimize (speed 3) (safety 0) (debug 0) #+lispworks (hcl:fixnum-safety 0))))))

(optimized-code-p nil)


(defun right-angle-at-vertex-p (ax ay bx by cx cy)
  "頂点A(ax,ay)において直角かどうか。AB・AC=0を判定する。"
  (let ((dot (+ (* (- bx ax) (- cx ax))
                (* (- by ay) (- cy ay)))))
    (zerop dot)))

(defun collinear-p (x1 y1 x2 y2)
  "O(0,0), P(x1,y1), Q(x2,y2) が同一直線上にある（または点が重複）かどうか。"
  (zerop (- (* x1 y2) (* x2 y1))))

(defun solve ()
  (let* ((limit-n 50)
         (count-total 0)
         (count-right-at-origin 0)
         (count-right-at-p 0)
         (count-right-at-q 0))
    ;; P=(x1,y1), Q=(x2,y2) を全列挙。ただし重複カウントを避けるため
    ;; (x1,y1) < (x2,y2) の辞書順で列挙する。
    ;; O,P,Qのいずれかが同一、または三点が同一直線上なら三角形不成立。
    (iterate
      (for x1 from 0 to limit-n)
      (iterate
        (for y1 from 0 to limit-n)
        (when (and (zerop x1) (zerop y1)) (next-iteration)) ; P≠O
        (iterate
          (for x2 from 0 to limit-n)
          (iterate
            (for y2 from 0 to limit-n)
            (when (and (zerop x2) (zerop y2)) (next-iteration)) ; Q≠O
            (when (and (= x1 x2) (= y1 y2)) (next-iteration))  ; P≠Q
            ;; 順序条件: 辞書順で (x1,y1) < (x2,y2)
            (when (or (> x1 x2)
                      (and (= x1 x2) (>= y1 y2)))
              (next-iteration))
            ;; 同一直線上チェック
            (when (collinear-p x1 y1 x2 y2) (next-iteration))
            ;; 直角判定
            (let ((right-o (right-angle-at-vertex-p 0 0 x1 y1 x2 y2))
                  (right-p (right-angle-at-vertex-p x1 y1 0 0 x2 y2))
                  (right-q (right-angle-at-vertex-p x2 y2 0 0 x1 y1)))
              (when right-o (incf count-right-at-origin))
              (when right-p (incf count-right-at-p))
              (when right-q (incf count-right-at-q))
              (when (or right-o right-p right-q)
                (incf count-total)))))))
    (format t "~&[DEBUG] N=~a~%" limit-n)
    (format t "~&[DEBUG] 直角@O: ~a~%" count-right-at-origin)
    (format t "~&[DEBUG] 直角@P: ~a~%" count-right-at-p)
    (format t "~&[DEBUG] 直角@Q: ~a~%" count-right-at-q)
    (format t "~&[DEBUG] 合計: ~a~%" count-total)
    count-total))


#+| Do it | (solve )
#|------------------------------------------------------------|
Timing the evaluation of (solve)
[DEBUG] N=50
[DEBUG] 直角@O: 2500
[DEBUG] 直角@P: 8367
[DEBUG] 直角@Q: 3367
[DEBUG] 合計: 14234

User time    =        0.172
System time  =        0.009
Elapsed time =        0.126
Allocation   = 102376 bytes
308 Page faults
GC time      =        0.000
 |------------------------------------------------------------|#
;;→ 14234
:ok