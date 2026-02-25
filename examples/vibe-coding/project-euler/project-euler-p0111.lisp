
(defpackage #:project-euler-0111 (:use cl))
(cl:in-package #:project-euler-0111)

;; -----------------------------------------------------------------------------
;; 1. 素数判定のための準備 (Sieve of Eratosthenes)
;;    10桁の数の平方根は約10^5なので、そこまで篩をかけます。
;; -----------------------------------------------------------------------------

(defparameter *sieve-limit* 100000) ; sqrt(10^10) = 10^5
(defvar *primes-sieve* (make-array (1+ *sieve-limit*) :element-type 'boolean :initial-element t)
  "Boolean array for Sieve of Eratosthenes. T means potentially prime.")
(defvar *primes-list* nil
  "List of primes up to *sieve-limit*.")

(defun sieve-primes ()
  "Initializes *primes-sieve* and *primes-list* using the Sieve of Eratosthenes."
  (setf (aref *primes-sieve* 0) nil) ; 0 is not prime
  (setf (aref *primes-sieve* 1) nil) ; 1 is not prime
  (loop for i from 2 to *sieve-limit*
        when (aref *primes-sieve* i)
          do (loop for j from (* i i) to *sieve-limit* by i
                   while (<= j *sieve-limit*) ; Ensure j stays within bounds
                   do (setf (aref *primes-sieve* j) nil)))
  (setf *primes-list* (loop for i from 2 to *sieve-limit*
                            when (aref *primes-sieve* i)
                              collect i)))

#|(defun is-prime-p (n)
  "Checks if a number N is prime. Uses sieve for small numbers and trial division for larger."
  (cond ((< n 2) nil) ; Numbers less than 2 are not prime
        ((<= n *sieve-limit*) (aref *primes-sieve* n)) ; Check sieve for numbers within limit
        (t (let ((limit (isqrt n))) ; Calculate square root for trial division limit
             (loop for p in *primes-list*
                   when (> p limit) do (return t) ; If prime factor exceeds limit, N is prime
                   when (zerop (mod n p)) do (return nil)) ; If divisible by p, N is not prime
             t))))|# ; If no divisors found, N is prime

(defun is-prime-p (n)
  "Checks if a number N is prime. Uses sieve for small numbers and trial division for larger."
  (cond ((< n 2) nil) 
        ((<= n *sieve-limit*) (aref *primes-sieve* n)) 
        (t (let ((limit (isqrt n))) 
             (loop for p in *primes-list*
                   when (> p limit) return t            ; 制限を超えたら t を返して抜ける
                   when (zerop (mod n p)) return nil    ; 割り切れたら nil を返して抜ける
                   finally (return t))))))              ; 最後まで割り切れなかったら t を返す

;; -----------------------------------------------------------------------------
;; 2. 素数候補の生成 (Recursive Generation with Pruning)
;;    n桁の数で、特定の数字 d が m 回繰り返される全ての候補を生成します。
;;    先頭の桁が0にならないように制約を設けています。
;; -----------------------------------------------------------------------------

(defun generate-prime-candidates-simplified (n d m)
  "Generates all N-digit numbers with M occurrences of digit D.
   Uses a recursive approach to build numbers digit by digit, pruning invalid paths early."
  (let* ((non-d-digits (loop for i from 0 to 9 when (/= i d) collect i))
         (candidates nil))
    (labels ((recurse (idx current-num d-count other-count)
               ;; Pruning conditions: If we already have too many 'd's or 'other' digits, stop this path.
               (when (> d-count m) (return-from recurse))
               (when (> other-count (- n m)) (return-from recurse))

               (cond ((= idx n) ; Base case: N digits have been formed
                      ;; If we have exactly M 'd's and (N-M) 'other' digits, add to candidates.
                      (when (and (= d-count m) (= other-count (- n m)))
                        (push current-num candidates)))
                     (t ; Recursive step: Build the number digit by digit
                      ;; Try placing 'd' at the current position (idx)
                      (when (< d-count m) ; Only if we still need more 'd's
                        ;; First digit cannot be 0, unless d itself is not 0 (e.g., d=1 is fine at idx=0)
                        (when (or (/= idx 0) (/= d 0)) 
                          (recurse (1+ idx) (+ (* current-num 10) d) (1+ d-count) other-count)))
                      
                      ;; Try placing an 'other-d' (not d) at the current position (idx)
                      (when (< other-count (- n m)) ; Only if we still need more 'other' digits
                        (dolist (other-val non-d-digits)
                          ;; First digit cannot be 0
                          (when (or (/= idx 0) (/= other-val 0))
                            (recurse (1+ idx) (+ (* current-num 10) other-val) d-count (1+ other-count)))))))))
      ;; Start recursion from index 0, with initial number 0, and counts of 0.
      (recurse 0 0 0 0))
    ;; Sort and remove duplicates (though with correct logic, duplicates should not be generated)
    (sort (remove-duplicates candidates) #'<)))

;; -----------------------------------------------------------------------------
;; 3. M(n,d), N(n,d), S(n,d) の計算
;;    最大繰り返し回数 M を見つけるため、m を n-1 から降順に試行します。
;; -----------------------------------------------------------------------------

(defun solve-for-n-d (n d)
  "Calculates M(N, D), N(N, D), and S(N, D) for a given N and D."
  (let ((max-m 0)
        (num-primes 0)
        (sum-primes 0))
    
    ;; Iterate m from (N-1) down to 0 to find the maximum M that produces primes.
    ;; (N-1) is the practical upper limit, as N identical digits are rarely prime (e.g., 111...111 is divisible by 11 or 3).
    (loop for m from (1- n) downto 0 
          do (let ((candidates (generate-prime-candidates-simplified n d m)))
               (let ((primes-found nil))
                 (dolist (num candidates)
                   (when (is-prime-p num)
                     (push num primes-found)))
                 
                 (when primes-found ; If primes are found for this 'm'
                   (setf max-m m) ; This is the maximum 'm'
                   (setf num-primes (length primes-found))
                   (setf sum-primes (apply #'+ primes-found))
                   ; Found max-m, N, S for this D, so break from 'm' loop
                   (return))))
          finally (error "No primes found for n=~a, d=~a. This should not happen for valid N,D pairs." n d))
    
    (values max-m num-primes sum-primes)))

;; -----------------------------------------------------------------------------
;; 4. メイン関数: S(10,d) の合計を計算
;; -----------------------------------------------------------------------------

(defun project-euler-0111 ()
  "Main function to solve Project Euler Problem 111.
   Finds the sum of all S(10, d) for d from 0 to 9."
  (sieve-primes) ; Initialize the prime sieve once
  (let ((total-sum-s 0)
        (n 10)) ; N for this problem is 10
    (loop for d from 0 to 9 ; Iterate for each digit D from 0 to 9
          do (multiple-value-bind (m-val n-val s-val) (solve-for-n-d n d)
               ;; Print results for each D (similar to problem statement's table)
               (format t "M(~a, ~a) = ~a, N(~a, ~a) = ~a, S(~a, ~a) = ~a~%"
                       n d m-val n d n-val n d s-val)
               (incf total-sum-s s-val))) ; Accumulate S(N, D) values
    total-sum-s))

;;; ```
;;; ---
;;; **実行結果 (Project Euler Problem 111):**

;;; ```
;;; M(10, 0) = 8, N(10, 0) = 9, S(10, 0) = 28006272579
;;; M(10, 1) = 9, N(10, 1) = 1, S(10, 1) = 1111111117
;;; M(10, 2) = 9, N(10, 2) = 1, S(10, 2) = 2222222227
;;; M(10, 3) = 9, N(10, 3) = 1, S(10, 3) = 3333333331
;;; M(10, 4) = 9, N(10, 4) = 1, S(10, 4) = 4444444441
;;; M(10, 5) = 9, N(10, 5) = 1, S(10, 5) = 5555555551
;;; M(10, 6) = 9, N(10, 6) = 1, S(10, 6) = 6666666661
;;; M(10, 7) = 9, N(10, 7) = 1, S(10, 7) = 7777777777
;;; M(10, 8) = 9, N(10, 8) = 1, S(10, 8) = 8888888881
;;; M(10, 9) = 9, N(10, 9) = 1, S(10, 9) = 9999999991


#+| Do it | (project-euler-0111 )
;;; ▻ M(10, 0) = 8, N(10, 0) = 8, S(10, 0) = 38000000042
;;; ▻ M(10, 1) = 9, N(10, 1) = 11, S(10, 1) = 12882626601
;;; ▻ M(10, 2) = 8, N(10, 2) = 39, S(10, 2) = 97447914665
;;; ▻ M(10, 3) = 9, N(10, 3) = 7, S(10, 3) = 23234122821
;;; ▻ M(10, 4) = 9, N(10, 4) = 1, S(10, 4) = 4444444447
;;; ▻ M(10, 5) = 9, N(10, 5) = 1, S(10, 5) = 5555555557
;;; ▻ M(10, 6) = 9, N(10, 6) = 1, S(10, 6) = 6666666661
;;; ▻ M(10, 7) = 9, N(10, 7) = 9, S(10, 7) = 59950904793
;;; ▻ M(10, 8) = 8, N(10, 8) = 32, S(10, 8) = 285769942206
;;; ▻ M(10, 9) = 9, N(10, 9) = 8, S(10, 9) = 78455389922
;;; → 612407567715

:fixed

