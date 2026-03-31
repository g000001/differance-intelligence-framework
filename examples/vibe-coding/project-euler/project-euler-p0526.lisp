;;; -*- mode: Lisp; coding: utf-8 -*-
;;; llm-model: claude-sonnet-4-6
;;; PythonコードのCommon Lispへの忠実な変換
(cl:in-package cl-user)
(defpackage #:project-euler-0526 (:use cl iterate alexandria) (:export #:solve))
(in-package #:project-euler-0526)


;; ---------------------------
;; Basic number theory helpers
;; ---------------------------

(defun egcd (a b)
  "Extended GCD: returns (values g x y) with a*x + b*y = g = gcd(a,b)."
  (let ((x0 1) (x1 0)
        (y0 0) (y1 1))
    (iterate (while (> b 0))
      (let ((q (floor a b)))
        (psetf a b
               b (- a (* q b)))
        (psetf x0 x1
               x1 (- x0 (* q x1)))
        (psetf y0 y1
               y1 (- y0 (* q y1)))))
    (values a x0 y0)))

(defun inv-mod (a m)
  "Modular inverse of a modulo m (assuming gcd(a,m)=1)."
  (multiple-value-bind (g x y) (egcd (mod a m) m)
    (declare (ignore y))
    (if (/= g 1)
        (error "inverse does not exist")
        (mod x m))))

(defun sieve-primes (limit)
  "Sieve of Eratosthenes up to limit (inclusive)."
  (when (< limit 2) (return-from sieve-primes nil))
  (let ((bs (make-array (1+ limit) :element-type 'bit :initial-element 1)))
    (setf (sbit bs 0) 0
          (sbit bs 1) 0)
    (let ((i 2))
      (iterate (while (<= (* i i) limit))
        (when (= (sbit bs i) 1)
          (iterate (for j from (* i i) to limit by i)
            (setf (sbit bs j) 0)))
        (incf i)))
    (iterate (for i from 2 to limit)
      (collect i into result)
      (when (= (sbit bs i) 0) (next-iteration))
      (finally (return result)))))

;; ---------------------------
;; Deterministic Miller-Rabin (64-bit)
;; ---------------------------

(defparameter *mr-small-primes* '(2 3 5 7 11 13 17 19 23 29 31 37))
(defparameter *mr-bases-64* '(2 325 9375 28178 450775 9780504 1795265022))

(defun is-prime (n)
  "Deterministic Miller-Rabin valid for all n < 2^64."
  (when (< n 2) (return-from is-prime nil))
  (iterate (for p in *mr-small-primes*)
    (when (= n p) (return-from is-prime t))
    (when (zerop (mod n p)) (return-from is-prime nil)))

  ;; write n-1 = d * 2^s
  (let ((d (1- n))
        (s 0))
    (iterate (while (evenp d))
      (incf s)
      (setf d (ash d -1)))

    (iterate (for a in *mr-bases-64*)
      (let ((a-mod (mod a n)))
        (when (zerop a-mod) (next-iteration))
        (let ((x (expt-mod a-mod d n)))
          (unless (or (= x 1) (= x (1- n)))
            (let ((found nil))
              (iterate (for r from 1 below s)
                (setf x (mod (* x x) n))
                (when (= x (1- n))
                  (setf found t)
                  (finish)))
              (unless found (return-from is-prime nil)))))))
    t))

(defun expt-mod (base exp m)
  "base^exp mod m"
  (let ((res 1)
        (b (mod base m)))
    (iterate (while (> exp 0))
      (when (oddp exp)
        (setf res (mod (* res b) m)))
      (setf b (mod (* b b) m))
      (setf exp (ash exp -1)))
    res))


;; ---------------------------
;; Wheel construction for t
;; ---------------------------

(defun forbidden-residues (polys p)
  "For each polynomial (a b) meaning a*t+b, find t ≡ -b*a^{-1} (mod p).
   Returns the set of forbidden residues modulo p, or nil if any poly is always 0 mod p."
  (let ((forb nil))
    (iterate (for poly in polys)
      (let ((a (first poly))
            (b (second poly)))
        (if (zerop (mod a p))
            ;; a ≡ 0 (mod p): poly ≡ b (mod p)
            (when (zerop (mod b p))
              ;; always divisible by p → impossible for primes
              (return-from forbidden-residues nil))
            ;; normal case
            (let ((inva (inv-mod a p)))
              (pushnew (mod (* (- b) inva) p) forb)))))
    forb))

(defun build-wheel-residues (polys wheel-primes)
  "Build all residues r (mod M) that avoid divisibility by wheel-primes for ALL polynomials.
   Returns (values residues M)."
  (let ((residues (list 0))
        (mod-val 1))
    (iterate (for p in wheel-primes)
      (let ((forb (forbidden-residues polys p)))
        (when (null forb)
          ;; forbidden-residues returned nil → no valid residues
          (return-from build-wheel-residues (values nil 0)))
        (let ((allowed (iterate (for x from 0 below p)
                         (unless (member x forb)
                           (collect x)))))
          (let ((inv (inv-mod mod-val p))
                (new-residues nil))
            (iterate (for r in residues)
              (let ((r-mod-p (mod r p)))
                (iterate (for a in allowed)
                  (let ((k (mod (* (- a r-mod-p) inv) p)))
                    (push (+ r (* mod-val k)) new-residues)))))
            (setf mod-val (* mod-val p))
            (setf residues (sort new-residues #'<))))))
    (values residues mod-val)))


;; ---------------------------
;; Main search
;; ---------------------------

(defun search-best (n)
  "Find h(N) for large N."
  ;; Two optimal residue classes mod 2520
  (let ((classes
         (list
          ;; class A: k ≡ 311 (mod 2520)
          (list 311
                '((2520 311) (2520 313) (2520 317) (2520 319)
                  (105 13) (1260 157) (8 1) (630 79) (420 53)))
          ;; class B: k ≡ 2201 (mod 2520)
          (list 2201
                '((2520 2201) (2520 2203) (2520 2207) (2520 2209)
                  (420 367) (630 551) (1260 1103) (105 92) (8 7)))))
        (wheel-primes '(11 13 17 19 23))
        (small-checks '(29 31 37 41 43 47)))

    ;; max-heap: list of (-k base-r t0 rr m-val polys)
    ;; Pythonのheapqは最小ヒープなのでkを負にして最大ヒープとして使う
    ;; ここではCommon Lispのリストで単純に実装
    (let ((heap nil))

      ;; heap操作（Pythonのheapqに対応する単純実装）
      (flet ((heap-push (item)
               (push item heap)
               (setf heap (sort heap #'> :key #'first)))
             (heap-pop ()
               (let ((top (first heap)))
                 (setf heap (rest heap))
                 top)))

        ;; 各クラスの残差を計算してヒープに積む
        (iterate (for cls in classes)
          (let* ((base-r (first cls))
                 (polys (second cls))
                 (t-max (floor (- n base-r) 2520)))
            (multiple-value-bind (residues m-val)
                (build-wheel-residues polys wheel-primes)
              (when residues
                (iterate (for rr in residues)
                  (let* ((t0 (- t-max (mod (- t-max rr) m-val))))
                    (when (>= t0 0)
                      (let ((k0 (+ (* 2520 t0) base-r)))
                        (heap-push (list k0 base-r t0 rr m-val polys))))))))))

        ;; ヒープから降順に取り出して評価
        (iterate (while heap)
          (let* ((item (heap-pop))
                 (k      (first item))
                 (base-r (second item))
                 (t-val  (third item))
                 (rr     (fourth item))
                 (m-val  (fifth item))
                 (polys  (sixth item)))

            ;; 次の候補をヒープに戻す
            (let ((t-next (- t-val m-val)))
              (when (>= t-next 0)
                (let ((k-next (+ (* 2520 t-next) base-r)))
                  (heap-push (list k-next base-r t-next rr m-val polys)))))

            ;; 9個の線形式を計算
            (let ((vals (mapcar (lambda (poly)
                                  (+ (* (first poly) t-val) (second poly)))
                                polys)))

              ;; 四つ子部分に対して小素数チェック
              (let ((quad (subseq vals 0 4))
                    (composite nil))
                (iterate (for p in small-checks)
                  (when composite (finish))
                  (iterate (for x in quad)
                    (when (and (/= x p) (zerop (mod x p)))
                      (setf composite t)
                      (finish))))

                (unless composite
                  ;; Miller-Rabin for quad
                  (iterate (for x in quad)
                    (unless (is-prime x)
                      (setf composite t)
                      (finish)))

                  (unless composite
                    ;; 残り5個
                    (iterate (for x in (subseq vals 4 9))
                      (unless (is-prime x)
                        (setf composite t)
                        (finish)))

                    (unless composite
                      ;; 発見
                      (let ((ans (reduce #'+ vals)))
                        (format t "Found! k = ~D, g(k) = ~D~%" k ans)
                        (return-from search-best ans))))))))))))
  (error "No solution found."))


(defun solve ()
  (let ((ans (search-best (expt 10 16))))
    (format t "h(10^16) = ~D~%" ans)
    ans))