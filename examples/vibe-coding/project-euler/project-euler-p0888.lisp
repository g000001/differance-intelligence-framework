;;; ;;; -*- mode: Lisp; coding: utf-8  -*-

;;; (cl:in-package cl-user)
;;; (defpackage #:project-euler-0888 (:use cl iterate))
;;; (in-package #:project-euler-0888)

;;; ;; ============================================================================
;;; ;; [1. ユーティリティ: 勝義的演算の定礎]
;;; ;; ============================================================================

;;; (defun extended-gcd (a b)
;;;   (let ((x0 1) (x1 0) (y0 0) (y1 1))
;;;     (iterate (while (not (zerop b)))
;;;              (let ((q (floor a b)))
;;;                (psetf a b
;;;                       b (mod a b)
;;;                       x0 x1
;;;                       x1 (- x0 (* q x1))
;;;                       y0 y1
;;;                       y1 (- y0 (* q y1)))))
;;;     (values a x0 y0)))

;;; (defun inverse-mod (n m)
;;;   (if (null m) 
;;;       (/ 1 n) ; modがnilなら有理数演算
;;;       (multiple-value-bind (g x y) (extended-gcd n m)
;;;         (declare (ignore y))
;;;         (if (= g 1) (mod x m) (error "No inverse for ~A mod ~A" n m)))))

;;; ;; ============================================================================
;;; ;; [2. Grundy数: ゲーム構造の還元]
;;; ;; ============================================================================

;;; (defun compute-grundy (limit)
;;;   "MEXを用いたGrundy数の計算。小さなlimitならO(N^2)で十分間に合う。"
;;;   (let ((g (make-array (1+ limit) :element-type 'fixnum :initial-element 0)))
;;;     (iterate (for n from 1 to limit)
;;;              (let ((seen (make-array 128 :element-type 'bit :initial-element 0)))
;;;                ;; 減算: 1, 2, 4, 9
;;;                (iterate (for s in '(1 2 4 9))
;;;                         (when (>= n s) (setf (bit seen (aref g (- n s))) 1)))
;;;                ;; 分割: G(a) ^ G(b) where a + b = n
;;;                (iterate (for a from 1 to (floor n 2))
;;;                         (let ((val (logxor (aref g a) (aref g (- n a)))))
;;;                           (when (< val 128) (setf (bit seen val) 1))))
;;;                ;; mex
;;;                (setf (aref g n) (iterate (for i from 0) (while (= (bit seen i) 1)) (finally (return i))))))
;;;     g))

;;; ;; ============================================================================
;;; ;; [3. WHT係数抽出: 中道の現成]
;;; ;; ============================================================================

;;; (defun solve-g-chi-m (m c-plus c-minus mod)
;;;   "係数 [z^m] (1-z)^{-C+} (1+z)^{-C-} の計算"
;;;   (let ((a (make-array (1+ m) :initial-element 0))
;;;         (b (make-array (1+ m) :initial-element 0)))
;;;     (setf (aref a 0) 1 (aref b 0) 1)
;;;     ;; 負の二項定理: [z^k] (1-z)^{-C} = binom(k+C-1, k)
;;;     (iterate (for k from 1 to m)
;;;              (let ((num (+ c-plus k -1)) (den k))
;;;                (setf (aref a k) (if mod 
;;;                                     (mod (* (aref a (1- k)) num (inverse-mod den mod)) mod)
;;;                                     (/ (* (aref a (1- k)) num) den)))))
;;;     ;; [z^k] (1+z)^{-C} = (-1)^k * binom(k+C-1, k)
;;;     (iterate (for k from 1 to m)
;;;              (let ((num (+ c-minus k -1)) (den k))
;;;                (setf (aref b k) (if mod
;;;                                     (mod (* (aref b (1- k)) num (inverse-mod den mod)) mod)
;;;                                     (/ (* (aref b (1- k)) num) den)))))
;;;     ;; 畳み込み
;;;     (iterate (for k from 0 to m)
;;;              (let* ((term-a (aref a k))
;;;                     (term-b (aref b (- m k)))
;;;                     (sign (if (evenp (- m k)) 1 -1))
;;;                     (product (* term-a term-b sign)))
;;;                (sum product into total))
;;;              (finally (return (if mod (mod total mod) total))))))

;;; (defun solve-euler-888-test ()
;;;   "S(12, 4) = 204 を検証する一発勝負の関数"
;;;   (let* ((n 12) (m 4) (mod nil)
;;;          (g (compute-grundy n))
;;;          (max-g (iterate (for v in-vector g) (maximize v)))
;;;          (dim (ash 1 (integer-length max-g))) ; WHTの次元
;;;          (counts (make-array dim :initial-element 0)))
;;;     ;; Grundy数の出現頻度をカウント
;;;     (iterate (for i from 1 to n)
;;;              (incf (aref counts (aref g i))))
;;;     ;; WHT空間での集計
;;;     (let ((sum-g 0))
;;;       (iterate (for chi from 0 below dim)
;;;                (let ((c-plus 0) (c-minus 0))
;;;                  (iterate (for v from 0 below dim)
;;;                           (if (evenp (logcount (logand v chi)))
;;;                               (incf c-plus (aref counts v))
;;;                               (incf c-minus (aref counts v))))
;;;                  ;; 各χ成分の係数を加算
;;;                  (incf sum-g (solve-g-chi-m m c-plus c-minus mod))))
;;;       ;; 最後に次元数で割る (WHTの逆変換)
;;;       (/ sum-g dim))))

;;; ;; 実行: (solve-euler-888-test)

;;; #+| Do it | (solve-euler-888-test )
;;; → 204
;;; 
;;; ;;; -*- mode: Lisp; coding: utf-8  -*-
;;; ;;; llm-model: gemini-3-flash-preview

;;; (cl:in-package cl-user)
;;; (defpackage #:project-euler-0888 (:use cl iterate))
;;; (in-package #:project-euler-0888)

;;; (declaim (optimize (speed 3) (safety 0) (debug 0)))

;;; ;; [1. ユーティリティ]
;;; (defun extended-gcd (a b)
;;;   (let ((x0 1) (x1 0) (y0 0) (y1 1))
;;;     (iterate (while (not (zerop b)))
;;;              (let ((q (floor a b)))
;;;                (psetf a b b (mod a b)
;;;                       x0 x1 x1 (- x0 (* q x1))
;;;                       y0 y1 y1 (- y0 (* q y1)))))
;;;     (values a x0 y0)))

;;; (defun inverse-mod (n m)
;;;   (multiple-value-bind (g x y) (extended-gcd n m)
;;;     (declare (ignore y))
;;;     (if (= g 1) (mod x m) (error "No inverse"))))

;;; ;; [2. 高速 Grundy 計算 (MEXのビット演算化)]
;;; (defun compute-grundy-fast (limit)
;;;   (let ((g (make-array (1+ limit) :element-type 'fixnum :initial-element 0)))
;;;     (declare (type (simple-array fixnum (*)) g))
;;;     (iterate (for n from 1 to limit)
;;;              (let ((bits 0))
;;;                (declare (type (unsigned-byte 128) bits))
;;;                ;; 減算
;;;                (iterate (for s in '(1 2 4 9))
;;;                         (when (>= n s) (setf bits (logior bits (ash 1 (aref g (- n s)))))))
;;;                ;; 分割 (対称性を利用)
;;;                (iterate (for a from 1 to (ash n -1))
;;;                         (setf bits (logior bits (ash 1 (logxor (aref g a) (aref g (- n a)))))))
;;;                ;; MEX
;;;                (setf (aref g n) (iterate (for i from 0) (while (logbitp i bits)) (finally (return i))))))
;;;     g))

;;; ;; [3. 周期検出]
;;; (defun find-structure (g limit)
;;;   (let ((window 400))
;;;     (iterate (for p from 1 to (floor (- limit window) 2))
;;;              (when (iterate (for i from (- limit window) below limit)
;;;                             (always (= (aref g i) (aref g (- i p)))))
;;;                (let ((transient (iterate (for i from (- limit p) downto 1)
;;;                                          (while (= (aref g i) (aref g (+ i p))))
;;;                                          (finally (return i)))))
;;;                  (return (values p transient)))))))

;;; ;; [4. 巨大Nのカウント]
;;; (defun get-counts (n g period transient max-g)
;;;   (let* ((dim (ash 1 (integer-length max-g)))
;;;          (counts (make-array dim :initial-element 0)))
;;;     (iterate (for i from 1 to (min n transient))
;;;              (incf (aref counts (aref g i))))
;;;     (when (> n transient)
;;;       (multiple-value-bind (q r) (floor (- n transient) period)
;;;         (iterate (for i from (1+ transient) to (+ transient period))
;;;                  (incf (aref counts (aref g i)) q))
;;;         (iterate (for i from 1 to r)
;;;                  (incf (aref counts (aref g (+ transient i)))))))
;;;     counts))

;;; ;; [5. 母関数係数抽出]
;;; (defun solve-g-chi-m (m c-plus c-minus mod)
;;;   (let ((a (make-array (1+ m))) (b (make-array (1+ m))))
;;;     (setf (aref a 0) 1 (aref b 0) 1)
;;;     (iterate (for k from 1 to m)
;;;              (let ((inv (inverse-mod k mod)))
;;;                (setf (aref a k) (mod (* (aref a (1- k)) (+ c-plus k -1) inv) mod)
;;;                      (aref b k) (mod (* (aref b (1- k)) (+ c-minus k -1) inv) mod))))
;;;     (iterate (for k from 0 to m)
;;;              (let ((term (mod (* (aref a k) (aref b (- m k))) mod)))
;;;                (sum (if (evenp (- m k)) term (- mod term)) into result))
;;;              (finally (return (mod result mod))))))

;;; ;; [6. 最終現成]
;;; (defun solve-euler-888 ()
;;;   (let* ((n 12491249) (m 1249) (mod 912491249)
;;;          (search-limit 5000) ;; 周期を見つけるのに十分な長さ
;;;          (g (compute-grundy-fast search-limit)))
;;;     (multiple-value-bind (period transient) (find-structure g search-limit)
;;;       (unless period (error "No period found"))
;;;       (format t "ACX Jump Confirmed: Period ~A, Transient ~A~%" period transient)
;;;       (let* ((max-g (iterate (for i from 1 to search-limit) (maximize (aref g i))))
;;;              (dim (ash 1 (integer-length max-g)))
;;;              (counts (get-counts n g period transient max-g))
;;;              (sum-g (iterate (for chi from 0 below dim)
;;;                              (let ((c-plus 0) (c-minus 0))
;;;                                (iterate (for v from 0 below dim)
;;;                                         (if (evenp (logcount (logand v chi)))
;;;                                             (incf c-plus (aref counts v))
;;;                                             (incf c-minus (aref counts v))))
;;;                                (sum (solve-g-chi-m m c-plus c-minus mod) into total))
;;;                              (finally (return (mod total mod))))))
;;;         (mod (* sum-g (inverse-mod dim mod)) mod)))))

;;; ;; (solve-euler-888)

;;; #+| Do it | (solve-euler-888 )


;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3-flash-preview

(cl:in-package cl-user)
(defpackage #:project-euler-0888 (:use cl alexandria iterate))
(in-package #:project-euler-0888)

(declaim (optimize (speed 3) (safety 0) (debug 0)))

;; [1. ユーティリティ]
(defun extended-gcd (a b)
  (let ((x0 1) (x1 0) (y0 0) (y1 1))
    (iterate (while (not (zerop b)))
             (let ((q (floor a b)))
               (psetf a b b (mod a b)
                      x0 x1 x1 (- x0 (* q x1))
                      y0 y1 y1 (- y0 (* q y1)))))
    (values a x0 y0)))

(defun inverse-mod (n m)
  (multiple-value-bind (g x y) (extended-gcd n m)
    (declare (ignore y))
    (if (= g 1) (mod x m) (error "No inverse"))))

;; [2. 極限まで削ぎ落とした Grundy 計算機]
(declaim (ftype (function (fixnum) (simple-array fixnum (*))) compute-grundy-ultra))
(defun compute-grundy-ultra (limit)
  (let ((g (make-array (1+ limit) :element-type 'fixnum :initial-element 0)))
    (declare (type (simple-array fixnum (*)) g))
    (format t "Starting Ultra-Fast Grundy calculation up to ~D...~%" limit)
    (iterate (for n from 1 to limit)
             (let ((bits 0))
               (declare (type (unsigned-byte 256) bits))
               ;; 減算
               (when (>= n 1) (setf bits (logior bits (ash 1 (aref g (- n 1))))))
               (when (>= n 2) (setf bits (logior bits (ash 1 (aref g (- n 2))))))
               (when (>= n 4) (setf bits (logior bits (ash 1 (aref g (- n 4))))))
               (when (>= n 9) (setf bits (logior bits (ash 1 (aref g (- n 9))))))
               ;; 分割 (ここを極限まで速くする)
               (iterate (for a from 1 to (ash n -1))
                        (declare (type fixnum a))
                        (setf bits (logior bits (ash 1 (logxor (aref g a) (aref g (- n a)))))))
               ;; MEX: 0となっている最小のビット位置
               (setf (aref g n) (integer-length (logand (lognot bits) (1+ bits))))
               (setf (aref g n) (1- (integer-length (logand (lognot bits) (logand (+ bits 1) (lognot bits)))))) ;; 代わりの高速MEX
               ;; シンプルに:
               (setf (aref g n) (iterate (for i from 0) (while (logbitp i bits)) (finally (return i))))))
    g))

;; [3. 構造検出 (より広い視野で)]
(defun find-structure-wide (g limit)
  (let ((window 1000)) ;; 周期判定の信頼性を高める
    (iterate (for p from 1 to (floor (- limit window) 2))
             (when (iterate (for i from (- limit window) below limit)
                            (always (= (aref g i) (aref g (- i p)))))
               (let ((transient (iterate (for i from (- limit p) downto 1)
                                         (while (= (aref g i) (aref g (+ i p))))
                                         (finally (return i)))))
                 (return (values p transient)))))))

;; [4. 係数抽出とWHT]
(defun solve-g-chi-m (m c-plus c-minus mod)
  (let ((a (make-array (1+ m))) (b (make-array (1+ m))))
    (setf (aref a 0) 1 (aref b 0) 1)
    (iterate (for k from 1 to m)
             (let ((inv (inverse-mod k mod)))
               (setf (aref a k) (mod (* (aref a (1- k)) (mod (+ c-plus k -1) mod) inv) mod)
                     (aref b k) (mod (* (aref b (1- k)) (mod (+ c-minus k -1) mod) inv) mod))))
    (iterate (for k from 0 to m)
             (let ((term (mod (* (aref a k) (aref b (- m k))) mod)))
               (sum (if (evenp (- m k)) term (- mod term)) into result))
             (finally (return (mod result mod))))))

(defun solve-euler-888 ()
  (let* ((n 12491249) (m 1249) (mod 912491249)
         (search-limit 60000) ;; 探索空間をさらに拡大
         (g (compute-grundy-ultra search-limit)))
    (multiple-value-bind (period transient) (find-structure-wide g search-limit)
      (unless period (error "それでも周期が見つかりませんでした。数学的な再設計が必要です。"))
      (format t "ACX Jump Confirmed! Period ~A, Transient ~A~%" period transient)
      (let* ((max-g (iterate (for i from 1 to search-limit) (maximize (aref g i))))
             (dim (ash 1 (integer-length max-g)))
             (counts (let ((c (make-array dim :initial-element 0)))
                       (iterate (for i from 1 to (min n transient)) (incf (aref c (aref g i))))
                       (when (> n transient)
                         (multiple-value-bind (q r) (floor (- n transient) period)
                           (iterate (for i from (1+ transient) to (+ transient period)) (incf (aref c (aref g i)) q))
                           (iterate (for i from 1 to r) (incf (aref c (aref g (+ transient i)))))))
                       c))
             (sum-g (iterate (for chi from 0 below dim)
                             (let ((c-plus 0) (c-minus 0))
                               (iterate (for v from 0 below dim)
                                        (if (evenp (logcount (logand v chi)))
                                            (incf c-plus (aref counts v))
                                            (incf c-minus (aref counts v))))
                               (sum (solve-g-chi-m m c-plus c-minus mod) into total))
                             (finally (return (mod total mod))))))
        (mod (* sum-g (inverse-mod dim mod)) mod)))))

;; (solve-euler-888)

#+| Do it | (time (print (solve-euler-888 )))
#||
227429102 
User time    =       23.731
System time  =        0.172
Elapsed time =       23.934
Allocation   = 4288312 bytes
3179 Page faults
GC time      =        0.000

||#


:gemini-3-1-pro