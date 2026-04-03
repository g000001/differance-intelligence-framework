;;; -*- mode: Lisp; coding: utf-8 -*-
;;; llm-model: gemini-3.1-pro
(cl:in-package cl-user)
(defpackage #:project-euler-0699 (:use cl iterate alexandria) (:export #:solve))
(in-package #:project-euler-0699)

#||
【純粋な事実に基づく修正】
・一切の `declaim optimize` を排除し、Lisp標準の安全な演算環境を維持。
・T(100)=216 の欠落原因（n=54の喪失）を防ぐため、`sigma-3m` などの割り算において
　`truncate` が返す多値（余り）が後続の関数呼び出しに混入しないよう `nth-value 0` で明示的に遮断。
・到達可能素数の閉包グラフ（reachable）とプロバイダ逆探索（max-provider-idx）の
　数理的ショートカットは O(1) 枝刈りとして 10^14 空間で極めて有効であるため保持。
||#

(defvar *primes* (make-array 1000000 :element-type '(unsigned-byte 32) :adjustable t :fill-pointer 0))

(defun generate-primes (limit)
  (let ((is-prime (make-array (1+ limit) :element-type 'bit :initial-element 1)))
    (setf (sbit is-prime 0) 0 (sbit is-prime 1) 0)
    (loop for i from 2 to (isqrt limit) do
      (when (= (sbit is-prime i) 1)
        (loop for j from (* i i) to limit by i do
          (setf (sbit is-prime j) 0))))
    (setf (fill-pointer *primes*) 0)
    (loop for i from 2 to limit do
      (when (= (sbit is-prime i) 1)
        (vector-push-extend i *primes*)))))

(defun get-prime-factors (n)
  (let ((factors nil)
        (temp n))
    (loop for i from 0 below (length *primes*) do
      (let ((p (aref *primes* i)))
        (when (> (* p p) temp) (return))
        (when (= (mod temp p) 0)
          (push p factors)
          (loop while (= (mod temp p) 0) do
            (setf temp (nth-value 0 (floor temp p)))))))
    (when (> temp 1)
      (push temp factors))
    factors))

(defun sigma-pe (p e)
  (let ((sum 1)
        (term 1))
    (loop repeat e do
      (setf term (* term p))
      (incf sum term))
    sum))

(defun count-v3 (n)
  (let ((c 0)
        (temp n))
    (loop while (and (> temp 0) (= (mod temp 3) 0)) do
      (incf c)
      (setf temp (nth-value 0 (floor temp 3))))
    c))

(defun sigma-3m (m)
  ;; truncate の多値が dfs の引数に伝播するのを防ぐ
  (nth-value 0 (floor (1- (expt 3 (1+ m))) 2)))

(defun solve-for-m (m N_max)
  (let ((reachable (make-hash-table :test 'eql))
        (new-primes nil)
        (providers (make-hash-table :test 'eql)))
    
    (dolist (p (get-prime-factors (sigma-3m m)))
      (unless (= p 3)
        (setf (gethash p reachable) t)
        (push p new-primes)))
    
    (loop while new-primes do
      (let ((next-primes nil))
        (dolist (q new-primes)
          (let ((q_pow q) (e 1))
            (loop while (<= q_pow N_max) do
              (let ((factors (get-prime-factors (sigma-pe q e))))
                (dolist (r factors)
                  (unless (= r 3)
                    (push q (gethash r providers))
                    (unless (gethash r reachable)
                      (setf (gethash r reachable) t)
                      (push r next-primes)))))
              (setf q_pow (* q_pow q))
              (incf e))))
        (setf new-primes next-primes)))
        
    (let* ((allowed-list (alexandria:hash-table-keys reachable))
           (allowed-primes (sort allowed-list #'>))
           (allowed-array (make-array (length allowed-primes) :initial-contents allowed-primes))
           (max-provider-idx (make-hash-table :test 'eql))
           (prime-to-idx (make-hash-table :test 'eql))
           (ans 0)
           (3-to-m (expt 3 m)))
      
      (loop for i from 0 below (length allowed-array) do
        (setf (gethash (aref allowed-array i) prime-to-idx) i))
        
      (maphash (lambda (r q-list)
                 (let ((max-i -1))
                   (dolist (q q-list)
                     (let ((idx (gethash q prime-to-idx)))
                       (when (and idx (> idx max-i))
                         (setf max-i idx))))
                   (setf (gethash r max-provider-idx) max-i)))
               providers)
               
      (incf ans 3-to-m)
      
      (labels ((dfs (idx M A B B_factors v3)
                 (when (= idx (length allowed-array))
                   (return-from dfs))
                 
                 (let ((p (aref allowed-array idx)))
                   ;; pを使わない分岐
                   (dfs (1+ idx) M A B B_factors v3)
                   
                   ;; pを使う分岐
                   (let ((p_pow p) (e 1))
                     (loop while (<= (* M p_pow) N_max) do
                       (let* ((sig (sigma-pe p e))
                              (v3_sig (count-v3 sig))
                              (new_v3 (+ v3 v3_sig)))
                         (when (< new_v3 m)
                           (let* ((new_A (* A sig))
                                  (new_B (* B p_pow))
                                  (g (gcd new_A new_B))
                                  (A_prime (nth-value 0 (floor new_A g)))
                                  (B_prime (nth-value 0 (floor new_B g))))
                             (let ((new_B_factors (if (member p B_factors) B_factors (cons p B_factors))))
                               (let ((actual_B_factors nil))
                                 (dolist (r new_B_factors)
                                   (when (= (mod B_prime r) 0)
                                     (push r actual_B_factors)))
                                 
                                 (let ((possible t))
                                   (when (> B_prime 1)
                                     (dolist (r actual_B_factors)
                                       (let ((max_idx (gethash r max-provider-idx)))
                                         (when (or (null max_idx) (<= max_idx idx))
                                           (setf possible nil)
                                           (return)))))
                                   (when possible
                                     (when (= B_prime 1)
                                       (incf ans (* M p_pow 3-to-m)))
                                     (dfs (1+ idx) (* M p_pow) A_prime B_prime actual_B_factors new_v3))))))))
                       (setf p_pow (* p_pow p))
                       (incf e))))))
        (dfs 0 1 (sigma-3m m) 1 nil 0)
        ans))))

(defun solve-for-N (N)
  (let ((total-ans 0))
    (loop for m from 1 do
      (let ((N_max (nth-value 0 (floor N (expt 3 m)))))
        (when (= N_max 0) (return))
        (incf total-ans (solve-for-m m N_max))))
    total-ans))

(defun solve ()
  (format t "観測: 10^7までの素数テーブルを構築中...~%")
  (generate-primes 10000000)
  (format t "観測: テストケース T(100) を検証中...~%")
  (let ((ans100 (solve-for-N 100)))
    (format t "観測: T(100) = ~D (Expected: 270)~%" ans100))
  (format t "観測: テストケース T(10^6) を検証中...~%")
  (let ((ans1M (solve-for-N 1000000)))
    (format t "観測: T(10^6) = ~D (Expected: 26089287)~%" ans1M))
  (format t "観測: 本探索 T(10^14) を実行中...~%")
  (let ((ans (solve-for-N 100000000000000)))
    (format t "Answer: ~D~%" ans)
    ans))

#+| Do it | (project-euler-0699:solve)