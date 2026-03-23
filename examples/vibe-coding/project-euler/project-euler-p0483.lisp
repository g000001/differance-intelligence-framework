;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3-flash-preview
(cl:in-package cl-user)
(defpackage #:project-euler-0483 (:use cl iterate alexandria) (:export #:solve))
(in-package #:project-euler-0483)


#||
(cl:comment "CLIF representation of PE 483 invariants and number-theoretic DP state reduction")
(cl:text
  ;; 1. The expected value of f(P)^2 over S_n is related to the LCM of cycle lengths.
  (forall (n)
    (= (g n)
       (sum (P (permutations n))
            (* (/ 1 (factorial n))
               (expt (lcm (cycles P)) 2)))))

  ;; 2. Separation of Primes: Primes >= 19 can appear at most once in any cycle length k <= 350.
  (forall (k p1 p2)
    (implies (and (<= k 350) (prime p1) (prime p2) (>= p1 19) (>= p2 19))
             (not (and (divides p1 k) (divides p2 k)))))

  ;; 3. Smooth Numbers: Cycle lengths are factored into a smooth part (primes < 19) and at most one large prime.
  (forall (k)
    (implies (<= k 350)
             (or (smooth k 17)
                 (exists (p m)
                   (and (prime p) (>= p 19)
                        (smooth m 17)
                        (= k (* m p)))))))

  ;; 4. Independent EGFs: The choice of cycles containing a large prime p is independent of other large primes.
  (forall (p1 p2)
    (implies (and (prime p1) (prime p2) (/= p1 p2) (>= p1 19) (>= p2 19))
             (independent (cycle-selection p1) (cycle-selection p2))))
)
||#


(defconstant +max-n+ 350)
(defconstant +v-limit+ 131072) ; 2^17 (Bit-packed state size for primes < 19)

(declaim (inline fast-merge-v))
(defun fast-merge-v (v1-val v2-val)
  (let ((res 0))
    (declare (type fixnum v1-val v2-val res))
    (setf res (logior res (ash (max (logand v1-val 15) (logand v2-val 15)) 0)))
    (setf res (logior res (ash (max (logand (ash v1-val -4) 7) (logand (ash v2-val -4) 7)) 4)))
    (setf res (logior res (ash (max (logand (ash v1-val -7) 3) (logand (ash v2-val -7) 3)) 7)))
    (setf res (logior res (ash (max (logand (ash v1-val -9) 3) (logand (ash v2-val -9) 3)) 9)))
    (setf res (logior res (ash (max (logand (ash v1-val -11) 3) (logand (ash v2-val -11) 3)) 11)))
    (setf res (logior res (ash (max (logand (ash v1-val -13) 3) (logand (ash v2-val -13) 3)) 13)))
    (setf res (logior res (ash (max (logand (ash v1-val -15) 3) (logand (ash v2-val -15) 3)) 15)))
    res))

(defun get-small-factors-pack (n)
  (let ((v2 0) (v3 0) (v5 0) (v7 0) (v11 0) (v13 0) (v17 0)
        (temp n))
    (iterate (while (evenp temp)) (incf v2) (setf temp (ash temp -1)))
    (iterate (while (= (mod temp 3) 0)) (incf v3) (setf temp (floor temp 3)))
    (iterate (while (= (mod temp 5) 0)) (incf v5) (setf temp (floor temp 5)))
    (iterate (while (= (mod temp 7) 0)) (incf v7) (setf temp (floor temp 7)))
    (iterate (while (= (mod temp 11) 0)) (incf v11) (setf temp (floor temp 11)))
    (iterate (while (= (mod temp 13) 0)) (incf v13) (setf temp (floor temp 13)))
    (iterate (while (= (mod temp 17) 0)) (incf v17) (setf temp (floor temp 17)))
    (let ((res 0))
      (setf res (logior res v2))
      (setf res (logior res (ash v3 4)))
      (setf res (logior res (ash v5 7)))
      (setf res (logior res (ash v7 9)))
      (setf res (logior res (ash v11 11)))
      (setf res (logior res (ash v13 13)))
      (setf res (logior res (ash v17 15)))
      (values res temp))))

(defun is-prime (n)
  (if (< n 2) nil
      (iterate (for i from 2 to (isqrt n))
               (when (= (mod n i) 0) (leave nil))
               (finally (return t)))))

(defun format-ratio-to-scientific (num den)
  "巨大なBignumの分数を安全に科学的記数法(有効数字10桁)にフォーマット"
  (if (= num 0)
      "0.000000000e0"
      (let ((exp 0))
        ;; 1 <= num/den < 10 となるようにスケール調整
        (iterate (while (>= num (* den 10)))
                 (setf den (* den 10))
                 (incf exp))
        (iterate (while (< num den))
                 (setf num (* num 10))
                 (decf exp))
        ;; 有効数字10桁のため、10^10倍して四捨五入
        (let* ((scaled-num (* num 10000000000))
               (val (round scaled-num den))
               (float-val (/ val 10000000000.0d0)))
          ;; もし繰り上がりで 10.0 になったら調整
          (when (>= float-val 10.0d0)
            (setf float-val (/ float-val 10.0d0))
            (incf exp))
          (format nil "~,9Fe~D" float-val exp)))))

(defun solve (&optional (target-n +max-n+))
  (format t "--- Project Euler 483 Execution Started (N=~D) ---~%" target-n)
  
  (let ((fact-array (make-array (1+ target-n) :initial-element 1)))
    (iterate (for i from 1 to target-n)
             (setf (aref fact-array i) (* i (aref fact-array (1- i)))))

    ;; Bignum/Ratio を許容する配列に変更。初期値は整数の 0。
    (let ((dp-1 (make-array (* (1+ target-n) +v-limit+) :initial-element 0))
          (dp-2 (make-array (* (1+ target-n) +v-limit+) :initial-element 0))
          (active-1 (make-array (1+ target-n) :initial-element '()))
          (active-2 (make-array (1+ target-n) :initial-element '())))
      
      (setf (aref dp-1 0) 1)
      (push 0 (aref active-1 0))

      (labels ((clear-buffer (dp active)
                 (iterate (for n from 0 to target-n)
                          (iterate (for v in (aref active n))
                                   (setf (aref dp (+ (* n +v-limit+) v)) 0))
                          (setf (aref active n) '())))
               
               (copy-buffer (dp-src active-src dp-dst active-dst)
                 (iterate (for n from 0 to target-n)
                          (iterate (for v in (aref active-src n))
                                   (let ((val (aref dp-src (+ (* n +v-limit+) v))))
                                     (setf (aref dp-dst (+ (* n +v-limit+) v)) val)
                                     (push v (aref active-dst n)))))))

        ;; 段階 1: Smooth numbers (<19) の有理数DP遷移
        (iterate (for i from 1 to target-n)
                 (multiple-value-bind (pack rem) (get-small-factors-pack i)
                   (when (= rem 1)
                     (clear-buffer dp-2 active-2)
                     (copy-buffer dp-1 active-1 dp-2 active-2)
                     (iterate (for n from 0 to (- target-n i))
                              (iterate (for v in (aref active-1 n))
                                       (let ((val (aref dp-1 (+ (* n +v-limit+) v))))
                                         (iterate (for c from 1 to (floor (- target-n n) i))
                                                  (let* ((next-n (+ n (* c i)))
                                                         (next-v (fast-merge-v v pack))
                                                         ;; 完全な有理数（Ratio）として重みを計算
                                                         (weight (/ 1 (* (expt i c) (aref fact-array c))))
                                                         (idx (+ (* next-n +v-limit+) next-v)))
                                                    (when (= (aref dp-2 idx) 0)
                                                      (push next-v (aref active-2 next-n)))
                                                    (incf (aref dp-2 idx) (* val weight)))))))
                     (rotatef dp-1 dp-2)
                     (rotatef active-1 active-2))))

        ;; 段階 2: 大きな素数 (>= 19) のDFS遷移
        (let ((large-primes '()))
          (iterate (for i from 19 to target-n)
                   (when (is-prime i) (push i large-primes)))
          
          (iterate (for p in (reverse large-primes))
                   (let ((k-max (floor target-n p))
                         (transitions '()))
                     (labels ((dfs (m current-n current-v current-w)
                                (if (> m k-max)
                                    (when (> current-n 0)
                                      ;; 有理数による p^2 の掛算
                                      (push (list current-n current-v (* current-w (* p p))) transitions))
                                    (progn
                                      (dfs (1+ m) current-n current-v current-w)
                                      (let ((m-pack (get-small-factors-pack m)))
                                        (iterate (for c from 1 to (floor (- target-n current-n) (* m p)))
                                                 (dfs (1+ m)
                                                      (+ current-n (* c m p))
                                                      (fast-merge-v current-v m-pack)
                                                      (/ current-w (* (expt (* m p) c) (aref fact-array c))))))))))
                       (dfs 1 0 0 1)) ; 初期重みは整数の 1
                     
                     (when transitions
                       (clear-buffer dp-2 active-2)
                       (copy-buffer dp-1 active-1 dp-2 active-2)
                       (iterate (for n from 0 to target-n)
                                (iterate (for v in (aref active-1 n))
                                         (let ((val (aref dp-1 (+ (* n +v-limit+) v))))
                                           (iterate (for trans in transitions)
                                                    (let ((delta-n (first trans))
                                                          (delta-v (second trans))
                                                          (weight (third trans)))
                                                      (when (<= (+ n delta-n) target-n)
                                                        (let* ((next-n (+ n delta-n))
                                                               (next-v (fast-merge-v v delta-v))
                                                               (idx (+ (* next-n +v-limit+) next-v)))
                                                          (when (= (aref dp-2 idx) 0)
                                                            (push next-v (aref active-2 next-n)))
                                                          (incf (aref dp-2 idx) (* val weight)))))))))
                       (rotatef dp-1 dp-2)
                       (rotatef active-1 active-2)))))
        
        ;; 最終集計
        (let ((ans 0))
          (iterate (for v in (aref active-1 target-n))
                   (let ((val (aref dp-1 (+ (* target-n +v-limit+) v)))
                         (v2 (logand v 15))
                         (v3 (logand (ash v -4) 7))
                         (v5 (logand (ash v -7) 3))
                         (v7 (logand (ash v -9) 3))
                         (v11 (logand (ash v -11) 3))
                         (v13 (logand (ash v -13) 3))
                         (v17 (logand (ash v -15) 3)))
                     (let ((lcm-sq 1))
                       (setf lcm-sq (* lcm-sq (expt 2 (* 2 v2))))
                       (setf lcm-sq (* lcm-sq (expt 3 (* 2 v3))))
                       (setf lcm-sq (* lcm-sq (expt 5 (* 2 v5))))
                       (setf lcm-sq (* lcm-sq (expt 7 (* 2 v7))))
                       (setf lcm-sq (* lcm-sq (expt 11 (* 2 v11))))
                       (setf lcm-sq (* lcm-sq (expt 13 (* 2 v13))))
                       (setf lcm-sq (* lcm-sq (expt 17 (* 2 v17))))
                       (incf ans (* val lcm-sq)))))
          
          (let ((formatted-ans (format-ratio-to-scientific (numerator ans) (denominator ans))))
            (format t "Result: ~A~%" formatted-ans)
            formatted-ans))))))


;;(time (print (solve )))