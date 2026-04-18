;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3-flash-preview
(cl:in-package cl-user)
(defpackage #:project-euler-0446 (:use cl iterate alexandria) (:export #:solve))
(in-package #:project-euler-0446)

(defmacro optimized-code-p (boole)
  (typecase boole
    (null nil)
    (T `(declaim (optimize (speed 3) (safety 0) (debug 0) #+lispworks (hcl:fixnum-safety 0))))))

(optimized-code-p T)

(defconstant +mod+ 1000000007)

(declaim (inline power))
(defun power (base exp mod-m)
  (declare (type fixnum base exp mod-m))
  (let ((res 1)
        (b base)
        (e exp))
    (declare (type fixnum res b e))
    (iterate (while (> e 0))
             (when (oddp e)
               (setf res (mod (* res b) mod-m)))
             (setf b (mod (* b b) mod-m))
             (setf e (ash e -1)))
    res))

(defun solve ()
  (let* (($n 10000000)
         (max-k (1+ $n))
         (val (make-array (1+ max-k) :element-type 'fixnum))
         (g-val (make-array (1+ max-k) :element-type 'fixnum :initial-element 1))
         (sieve (make-array (1+ max-k) :element-type 'bit :initial-element 0))
         (isqrt-max-k (isqrt max-k)))
    (declare (type fixnum $n max-k isqrt-max-k))
    
    (format t "Step 1: P(k) = k^2+1 の配列初期化中...~%")
    (iterate (for k from 0 to max-k)
             (declare (type fixnum k))
             (let ((v (1+ (* k k))))
               (declare (type fixnum v))
               ;; P(k) は k が奇数のとき偶数になる。2の寄与を予め除外する。
               (when (oddp k)
                 (setf v (ash v -1)))
               (setf (aref val k) v)))
               
    (format t "Step 2: 素数 p = 1 mod 4 による O(N log log N) の篩を実行中...~%")
    (iterate (for p from 2 to max-k)
             (declare (type fixnum p))
             (when (zerop (sbit sieve p))
               ;; エラトステネスの篩のマーキング
               (when (<= p isqrt-max-k)
                 (iterate (for i from (* p p) to max-k by p)
                          (declare (type fixnum i))
                          (setf (sbit sieve i) 1)))
               
               ;; p = 1 mod 4 の素数に対する k^2+1 の因数分解
               (when (= 1 (mod p 4))
                 (let ((a 2)
                       (p-minus-1 (1- p)))
                   (declare (type fixnum a p-minus-1))
                   ;; r^2 = -1 mod p の解を求める
                   (iterate (while (/= (power a (ash p-minus-1 -1) p) p-minus-1))
                            (incf a))
                   (let* ((r1 (power a (ash p-minus-1 -2) p))
                          (r2 (- p r1)))
                     (declare (type fixnum r1 r2))
                     
                     ;; 解 r1 と r2 から始まる等差数列を篩う
                     (dolist (r (list r1 r2))
                       (declare (type fixnum r))
                       (iterate (for k from r to max-k by p)
                                (declare (type fixnum k))
                                (multiple-value-bind (q rem) (floor (aref val k) p)
                                  (when (zerop rem)
                                    (let ((v q)
                                          (e 1))
                                      (declare (type fixnum v e))
                                      (iterate (while t)
                                               (multiple-value-bind (q2 rem2) (floor v p)
                                                 (if (zerop rem2)
                                                     (progn (setf v q2) (incf e))
                                                     (leave))))
                                      (setf (aref val k) v)
                                      ;; g(k) の更新: (p^e + 1) を掛ける
                                      (let ((pe p))
                                        (declare (type fixnum pe))
                                        (iterate (for i from 2 to e) (setf pe (* pe p)))
                                        (setf (aref g-val k) 
                                              (mod (* (aref g-val k) (mod (1+ pe) +mod+)) +mod+)))))))))))))
    
    (format t "Step 3: 残存した巨大素数の寄与を反映中...~%")
    (iterate (for k from 0 to max-k)
             (declare (type fixnum k))
             (let ((v (aref val k)))
               (declare (type fixnum v))
               (when (> v 1)
                 (let ((v-mod (mod (1+ v) +mod+)))
                   (setf (aref g-val k) (mod (* (aref g-val k) v-mod) +mod+))))))
                   
    (format t "Step 4: F(N) = Σ R(n^4+4) の最終集計...~%")
    (let ((ans 0))
      (declare (type fixnum ans))
      (iterate (for n from 1 to $n)
               (declare (type fixnum n))
               (let* ((g-odd (mod (* (aref g-val (1- n)) (aref g-val (1+ n))) +mod+))
                      ;; nが偶数の場合、(n^4+4) は4でちょうど割り切れるため、2の寄与 (2^2+1)=5 を掛ける
                      (g-tot (if (evenp n)
                                 (mod (* 5 g-odd) +mod+)
                                 g-odd))
                      (n-mod (mod n +mod+))
                      (n2 (mod (* n-mod n-mod) +mod+))
                      (n4 (mod (* n2 n2) +mod+))
                      (n4-plus-4 (mod (+ n4 4) +mod+))
                      (r-n (- g-tot n4-plus-4)))
                 (declare (type fixnum g-odd g-tot n-mod n2 n4 n4-plus-4 r-n))
                 (when (< r-n 0) (incf r-n +mod+))
                 (setf ans (mod (+ ans r-n) +mod+))))
                 
      (format t "Final Result F(~A): ~A~%" $n ans)
      ans)))


#+| Do it | (solve )
#|------------------------------------------------------------|
Timing the evaluation of (solve)
Step 1: P(k) = k^2+1 の配列初期化中...
Step 2: 素数 p = 1 mod 4 による O(N log log N) の篩を実行中...
Step 3: 残存した巨大素数の寄与を反映中...
Step 4: F(N) = Σ R(n^4+4) の最終集計...
Final Result F(10000000): 907803852

User time    =        4.614
System time  =        0.092
Elapsed time =        4.608
Allocation   = 173022784 bytes
45131 Page faults
GC time      =        0.001
 |------------------------------------------------------------|#
;;→ 907803852
:ok