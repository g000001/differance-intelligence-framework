;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3.1-pro
(cl:in-package cl-user)
(defpackage #:project-euler-secret (:use cl iterate alexandria) (:export #:solve))
(in-package #:project-euler-secret)

(defmacro optimized-code-p (boole)
  (typecase boole
    (null nil)
    (T `(declaim (optimize (speed 3) (safety 0) (debug 0) #+lispworks (hcl:fixnum-safety 0))))))

(optimized-code-p T)

;; P2形式(アスキーPGM)の単純な読み込み
(defun load-pgm (filepath)
  (with-open-file (stream filepath :direction :input)
    (let* ((magic (read stream))
           (width (read stream))
           (height (read stream))
           (maxval (read stream))
           (grid (make-array (list height width) :element-type 'fixnum)))
      (declare (ignore magic maxval))
      (iterate (for y from 0 below height)
        (iterate (for x from 0 below width)
          (setf (aref grid y x) (read stream))))
      grid)))

;; P2形式での保存（結果の可視化用）
(defun save-pgm (grid filepath)
  (let* ((height (array-dimension grid 0))
         (width (array-dimension grid 1)))
    (with-open-file (stream filepath :direction :output :if-exists :supersede)
      (format stream "P2~%~D ~D~%6~%" width height) ; 最大値はmod 7なので6
      (iterate (for y from 0 below height)
        (iterate (for x from 0 below width)
          (format stream "~D " (aref grid y x)))
        (format stream "~%")))))

(defun apply-step-mod7 (current-grid limit-w limit-h shift-x shift-y)
  (declare (type fixnum limit-w limit-h shift-x shift-y)
           (type (simple-array fixnum (* *)) current-grid))
  (let ((next-grid (make-array (list limit-h limit-w) :element-type 'fixnum)))
    (iterate (for pos-y from 0 below limit-h)
      (iterate (for pos-x from 0 below limit-w)
        (let ((val-up    (aref current-grid (mod (- pos-y shift-y) limit-h) pos-x))
              (val-down  (aref current-grid (mod (+ pos-y shift-y) limit-h) pos-x))
              (val-left  (aref current-grid pos-y (mod (- pos-x shift-x) limit-w)))
              (val-right (aref current-grid pos-y (mod (+ pos-x shift-x) limit-w))))
          (declare (type fixnum val-up val-down val-left val-right))
          ;; 4近傍の和を法7で計算
          (setf (aref next-grid pos-y pos-x) (mod (+ val-up val-down val-left val-right) 7)))))
    next-grid))

(defun solve-image-automaton (initial-grid target-steps)
  (let* ((limit-h (array-dimension initial-grid 0))
         (limit-w (array-dimension initial-grid 1))
         (current-grid (make-array (list limit-h limit-w) :element-type 'fixnum)))
    
    ;; 初期状態を mod 7 でコピー
    (iterate (for pos-y from 0 below limit-h)
      (iterate (for pos-x from 0 below limit-w)
        (setf (aref current-grid pos-y pos-x) (mod (aref initial-grid pos-y pos-x) 7))))
    
    (let ((remaining-steps target-steps)
          (shift-x 1)
          (shift-y 1))
      
      ;; 10^12 の 7進数展開に基づく計算量の次元崩壊
      (iterate (while (> remaining-steps 0))
        (let ((digit (mod remaining-steps 7)))
          (iterate (for iter-count from 1 to digit)
            (setf current-grid (apply-step-mod7 current-grid limit-w limit-h shift-x shift-y)))
          
          (setf remaining-steps (floor remaining-steps 7))
          (setf shift-x (mod (* shift-x 7) limit-w))
          (setf shift-y (mod (* shift-y 7) limit-h))))
      
      current-grid)))

(defun solve (&optional (input-file "/tmp/input.pgm") (output-file "/tmp/output.pgm"))
  (format t "Reading image from ~A...~%" input-file)
  (let* ((initial-grid (load-pgm input-file))
         (steps #.(expt 10 12)))
    
    (format t "Image loaded. Size: ~Dx~D~%" (array-dimension initial-grid 1) (array-dimension initial-grid 0))
    (format t "Applying Frobenius Endomorphism shortcut for ~D steps...~%" steps)
    
    (let ((result-grid (solve-image-automaton initial-grid steps)))
      
      (format t "Computation complete.~%")
      (format t "Saving result to ~A...~%" output-file)
      (save-pgm result-grid output-file)
      
      (format t "Done. Open ~A to reveal the secret word.~%" output-file)
      t)))


#+| Do it | (solve )
#|------------------------------------------------------------|
Timing the evaluation of (solve)
Reading image from /tmp/input.pgm...
Image loaded. Size: 975x585
Applying Frobenius Endomorphism shortcut for 1000000000000 steps...
Computation complete.
Saving result to /tmp/output.pgm...
Done. Open /tmp/output.pgm to reveal the secret word.

User time    =        2.533
System time  =        0.052
Elapsed time =        2.528
Allocation   = 201047224 bytes
19538 Page faults
GC time      =        0.040
 |------------------------------------------------------------|#
;;→ t
;;Leonhard
:ok