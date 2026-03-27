;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3-flash-preview
(cl:in-package cl-user)
(defpackage #:project-euler-0946 (:use cl iterate alexandria) (:export #:solve))
(in-package #:project-euler-0946)

(deftype  uint8 () '(unsigned-byte  8))
(deftype uint16 () '(unsigned-byte 16))
(deftype uint32 () '(unsigned-byte 32))
(deftype uint60 () '(unsigned-byte 60))

(defun make-uint8-array (size &key (initialize-element 0))
  (make-array size :element-type 'uint8 :initial-element initialize-element))

(defun make-fixnum-array (size &key (initialize-element 0))
  (make-array size :element-type 'fixnum :initial-element initialize-element))

(defun build-primes (limit-n)
  (let* ((sieve (make-uint8-array (1+ limit-n) :initialize-element 1))
         (count 0))
    (setf (aref sieve 0) 0)
    (setf (aref sieve 1) 0)
    (iterate (for index-i from 2 to limit-n)
      (when (= (aref sieve index-i) 1)
        (incf count)
        (iterate (for index-j from (* index-i index-i) to limit-n by index-i)
          (setf (aref sieve index-j) 0))))
    (let ((primes (make-fixnum-array count :initialize-element 0))
          (write-idx 0))
      (iterate (for index-i from 2 to limit-n)
        (when (= (aref sieve index-i) 1)
          (setf (aref primes write-idx) index-i)
          (incf write-idx)))
      primes)))

(defun fast-fib (target-n)
  ;; 行列累乗による高速フィボナッチ計算 O(log N)
  (declare (type fixnum target-n))
  (if (= target-n 0)
      (values 0 1)
      (multiple-value-bind (fib-k fib-k1) (fast-fib (ash target-n -1))
        (let* ((fib-2k (* fib-k (- (* 2 fib-k1) fib-k)))
               (fib-2k1 (+ (* fib-k fib-k) (* fib-k1 fib-k1))))
          (if (evenp target-n)
              (values fib-2k fib-2k1)
              (values fib-2k1 (+ fib-2k fib-2k1)))))))

(defun extract-exact-outputs (state-a state-b state-c state-d target-outputs current-count current-sum)
  ;; Bignumによる正確なEuclid互除法（出力抽出）。メモリ確保を最小限にするためリストを生成せず直接集計する。
  (iterate
    ;; 特異点（CとC+Dが異符号、またはゼロ除算）を含まない場合のみ抽出可能
    (when (or (= state-c 0) (= (+ state-c state-d) 0)
              (and (> state-c 0) (< (+ state-c state-d) 0))
              (and (< state-c 0) (> (+ state-c state-d) 0)))
      (return (values current-count current-sum state-a state-b state-c state-d)))
    
    (let ((quotient1 (floor state-a state-c))
          (quotient2 (floor (+ state-a state-b) (+ state-c state-d))))
      (if (= quotient1 quotient2)
          (progn
            (incf current-sum quotient1)
            (incf current-count)
            (when (= current-count target-outputs)
              (return (values current-count current-sum state-a state-b state-c state-d)))
            ;; 状態行列を更新 M <- [[0, 1], [1, -q]] * M
            (let ((next-a state-c)
                  (next-b state-d)
                  (next-c (- state-a (* quotient1 state-c)))
                  (next-d (- state-b (* quotient1 state-d))))
              (setf state-a next-a state-b next-b state-c next-c state-d next-d)))
          (return (values current-count current-sum state-a state-b state-c state-d))))))

(defun solve (&optional (target-outputs #.(expt 10 8)))
  ;; 目標10^8出力に必要な素数は約11000個（最大115000付近）。20万まで篩えば十分。
  (let ((primes (build-primes 200000))
        (state-a 2) (state-b 3) (state-c 3) (state-d 2)
        (total-count 0)
        (total-sum 0)
        (log-threshold 10000000))
    
    (format t "Starting Exact Bignum Gosper algorithm for ~A outputs...~%" target-outputs)
    
    (iterate (for p in-vector primes)
      ;; ダミー回避を撤廃。すべての素数に対して完全に正確な Bignum Fibonacci 乗算を行う
      (multiple-value-bind (fib-p fib-p1) (fast-fib p)
        (let* ((fib-p2 (+ fib-p fib-p1))
               (fib-p3 (+ fib-p1 fib-p2))
               ;; M_new = M_old * B_p (B_pは「2」と「p個の1」のブロック行列表現)
               (mat-a (+ (* state-a fib-p3) (* state-b fib-p1)))
               (mat-b (+ (* state-a fib-p2) (* state-b fib-p)))
               (mat-c (+ (* state-c fib-p3) (* state-d fib-p1)))
               (mat-d (+ (* state-c fib-p2) (* state-d fib-p))))
          
          (multiple-value-bind (new-count new-sum n-a n-b n-c n-d)
              (extract-exact-outputs mat-a mat-b mat-c mat-d target-outputs total-count total-sum)
            (setf total-count new-count
                  total-sum new-sum
                  state-a n-a state-b n-b state-c n-c state-d n-d)
            
            (when (>= total-count log-threshold)
              (format t "Progress: ~A / ~A (Last Prime: ~A)~%" total-count target-outputs p)
              (incf log-threshold 10000000))
            
            (when (= total-count target-outputs)
              (format t "Finished. Answer: ~A~%" total-sum)
              (return-from solve total-sum))))))))


#+| Do it | (solve )
#||
Starting Exact Bignum Gosper algorithm for 100000000 outputs...
Progress: 10004479 / 100000000 (Last Prime: 24281)
Progress: 20033160 / 100000000 (Last Prime: 34607)
Progress: 30010206 / 100000000 (Last Prime: 42359)
Progress: 40049230 / 100000000 (Last Prime: 49523)
Progress: 50009244 / 100000000 (Last Prime: 55411)
Progress: 60035116 / 100000000 (Last Prime: 60737)
Progress: 70027672 / 100000000 (Last Prime: 65951)
Progress: 80036812 / 100000000 (Last Prime: 70879)
Progress: 90009187 / 100000000 (Last Prime: 75503)
Progress: 100000000 / 100000000 (Last Prime: 79699)
Finished. Answer: 585787007
||#

;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3-flash-preview
(cl:in-package cl-user)
(defpackage #:project-euler-0946 (:use cl iterate alexandria) (:export #:solve))
(in-package #:project-euler-0946)

(deftype  uint8 () '(unsigned-byte  8))
(deftype uint16 () '(unsigned-byte 16))
(deftype uint32 () '(unsigned-byte 32))
(deftype uint60 () '(unsigned-byte 60))

(defun make-uint8-array (size &key (initialize-element 0))
  (make-array size :element-type 'uint8 :initial-element initialize-element))

(defun make-fixnum-array (size &key (initialize-element 0))
  (make-array size :element-type 'fixnum :initial-element initialize-element))

(defun build-primes (limit-n)
  (let* ((sieve (make-uint8-array (1+ limit-n) :initialize-element 1))
         (count 0))
    (setf (aref sieve 0) 0)
    (setf (aref sieve 1) 0)
    (iterate (for index-i from 2 to limit-n)
      (when (= (aref sieve index-i) 1)
        (incf count)
        (iterate (for index-j from (* index-i index-i) to limit-n by index-i)
          (setf (aref sieve index-j) 0))))
    (let ((primes (make-fixnum-array count))
          (write-idx 0))
      (iterate (for index-i from 2 to limit-n)
        (when (= (aref sieve index-i) 1)
          (setf (aref primes write-idx) index-i)
          (incf write-idx)))
      primes)))

(defun solve (&optional (target-outputs #.(expt 10 8)))
  ;; 出力10^8に対し、入力は約1.6 * 10^9個必要。
  ;; 1,000,000までの素数(78,498個)を用意すれば、1の総和は約3.7 * 10^10となり完全に余裕を持つ。
  (let ((primes (build-primes 1000000))
        (state-a 2) (state-b 3) (state-c 3) (state-d 2)
        (total-count 0)
        (total-sum 0)
        (prime-index 0)
        (ones-left 0)
        (alpha-state 0))
    
    ;; 状態は完全に有界な整数(トランスデューサ)に保たれるため fixnum 宣言のみで最高速化する
    (declare (type fixnum state-a state-b state-c state-d 
                          total-count total-sum prime-index ones-left alpha-state))
    
    (format t "Starting Gosper Stream Transducer for ~A outputs...~%" target-outputs)
    
    (iterate
      (while (< total-count target-outputs))
      
      (let ((output-val nil))
        ;; 出力可能性のチェック: 区間 [1, ∞) において確定するか判定
        (when (and (not (zerop state-c))
                   (not (zerop (+ state-c state-d)))
                   (or (and (> state-c 0) (> (+ state-c state-d) 0))
                       (and (< state-c 0) (< (+ state-c state-d) 0))))
          (let ((q1 (truncate state-a state-c))
                (q2 (truncate (+ state-a state-b) (+ state-c state-d))))
            (when (= q1 q2)
              (setf output-val q1))))
        
        (if output-val
            ;; 出力確定: 係数を排出し、状態行列を縮小(M <- [[0, 1], [1, -q]] * M)
            (progn
              (incf total-sum output-val)
              (incf total-count)
              (when (and (> total-count 0) (= (mod total-count 10000000) 0))
                (format t "Progress: ~A / ~A~%" total-count target-outputs))
              (let ((next-a state-c)
                    (next-b state-d)
                    (next-c (- state-a (* output-val state-c)))
                    (next-d (- state-b (* output-val state-d))))
                (setf state-a next-a state-b next-b state-c next-c state-d next-d)))
            
            ;; 出力不可: ストリームから入力を1文字読み込み、状態行列を更新
            (let ((input-val 0))
              (declare (type fixnum input-val))
              (case alpha-state
                ((0)
                 (setf ones-left (aref primes prime-index))
                 (incf prime-index)
                 (setf alpha-state 1)
                 (setf input-val 2))
                ((1)
                 (decf ones-left)
                 (when (zerop ones-left)
                   (setf alpha-state 2))
                 (setf input-val 1))
                ((2)
                 (setf ones-left (aref primes prime-index))
                 (incf prime-index)
                 (setf alpha-state 1)
                 (setf input-val 2)))
              
              (let ((next-a (+ (* state-a input-val) state-b))
                    (next-b state-a)
                    (next-c (+ (* state-c input-val) state-d))
                    (next-d state-c))
                (setf state-a next-a state-b next-b state-c next-c state-d next-d))))))
    
    (format t "Finished. Answer: ~A~%" total-sum)
    total-sum))


#+| Do it | (solve )
#|------------------------------------------------------------|
Timing the evaluation of (solve)
Starting Gosper Stream Transducer for 100000000 outputs...
Progress: 10000000 / 100000000
Progress: 20000000 / 100000000
Progress: 30000000 / 100000000
Progress: 40000000 / 100000000
Progress: 50000000 / 100000000
Progress: 60000000 / 100000000
Progress: 70000000 / 100000000
Progress: 80000000 / 100000000
Progress: 90000000 / 100000000
Progress: 100000000 / 100000000
Finished. Answer: 585787007

User time    =       14.965
System time  =        0.093
Elapsed time =       14.978
Allocation   = 2088624 bytes
336 Page faults
GC time      =        0.000
 |------------------------------------------------------------|#
;;→ 585787007
:ok