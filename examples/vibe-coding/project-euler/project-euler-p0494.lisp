;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3-flash-preview
(cl:in-package cl-user)
(defpackage #:project-euler-0494 (:use cl iterate alexandria) (:export #:solve))
(in-package #:project-euler-0494)

(defun solve ()
  (let ((m 90)
        (F (make-array 91 :element-type 'integer))
        (memo (make-hash-table :test 'equal)))
    
    (format t "Step 1: Calculating base asymptotic families (Fibonacci F_90)...~%")
    (setf (aref F 1) 1)
    (setf (aref F 2) 1)
    (iterate (for i from 3 to 90)
             (setf (aref F i) (+ (aref F (1- i)) (aref F (- i 2)))))
    
    (let ((base-families (aref F 90)))
      (format t "F_90 = ~A~%" base-families)
      
      (format t "Step 2: Calculating exception families via DP from Seeds 9 and 19...~%")
      
      ;; DP: count_paths (steps_remaining, current_value)
      (labels ((count-paths (steps val)
                 (if (= steps 0)
                     1
                     (let ((key (cons steps val)))
                       (multiple-value-bind (res exists) (gethash key memo)
                         (if exists
                             res
                             (let ((ways (count-paths (1- steps) (* val 2))))
                               ;; 逆算でO（(x-1)/3）が有効なのは、val ≡ 4 (mod 6) の時のみ
                               (when (and (= (mod val 6) 4) (> val 4))
                                 (incf ways (count-paths (1- steps) (truncate (1- val) 3))))
                               (setf (gethash key memo) ways)
                               ways)))))))
        
        ;; Seed 9 の例外パス数: D(9) = 15 なので、残りの長さは m - 15
        ;; 数学的証明通り、9からのO分岐は永遠に発生しないため、結果は常に 1 になる
        (let ((paths-from-9 (count-paths (- m 15) 9)))
          (format t "Exceptions from Seed 9  = ~A~%" paths-from-9)
          
          ;; Seed 19 の例外パス数: D(19) = 17 なので、残りの長さは m - 17
          (let ((paths-from-19 (count-paths (- m 17) 19)))
            (format t "Exceptions from Seed 19 = ~A~%" paths-from-19)
            
            (let ((total-exceptions (+ paths-from-9 paths-from-19)))
              (format t "Total exception families = ~A~%" total-exceptions)
              
              (let ((ans (+ base-families total-exceptions)))
                (format t "Final Answer: ~A~%" ans)
                ans))))))))


