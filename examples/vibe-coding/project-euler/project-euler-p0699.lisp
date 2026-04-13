#|;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3-flash-preview
(cl:in-package cl-user)
(defpackage #:project-euler-0699 (:use cl) (:export #:solve))
(in-package #:project-euler-0699)

(defmacro source-pathname ()
  "Compute source pathname"
  `(load-time-value ,(or *compile-file-truename* *load-truename* (uiop:getcwd))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :cffi)
  (ql:quickload :uiop))

;;; ----------------------------------------------------------------------
;;; Julia C API バインディングと共有ライブラリのロード
;;; ----------------------------------------------------------------------
(cffi:define-foreign-library libjulia
  (:darwin (:or "libjulia.dylib" "libjulia.1.dylib"))
  (:unix (:or "libjulia.so" "libjulia.so.1"))
  (:windows "libjulia.dll")
  (t (:default "libjulia")))

(handler-case 
    (cffi:use-foreign-library libjulia)
  (error (e)
    (format t "WARNING: Failed to load libjulia: ~A~%" e)
    (format t "Please ensure Julia is installed and its lib path is exposed.~%")))

(cffi:defcfun ("jl_init" %jl-init) :void)
(cffi:defcfun ("jl_eval_string" %jl-eval-string) :pointer (str :string))

;;; ----------------------------------------------------------------------
;;; Julia JIT Code
;;; ----------------------------------------------------------------------
(defparameter *julia-code-699* "
module Euler699
export solve699

function get_sigma128(p::Int64, a::Int)
    return (Int128(p)^(a+1) - 1) ÷ (p - 1)
end

function v3(n::Union{Int64, Int128})
    c = 0
    while n > 0 && n % 3 == 0
        c += 1
        n ÷= 3
    end
    return c
end

function get_prime_factors(n::Int128)
    factors = Int64[]
    p = Int128(2)
    while p * p <= n
        if n % p == 0
            push!(factors, Int64(p))
            while n % p == 0
                n ÷= p
            end
        end
        p += 1
    end
    if n > 1
        push!(factors, Int64(n))
    end
    return factors
end

function max_p_factor(n::Int128)
    n == 1 && return Int64(1)
    p = Int128(2)
    max_p = Int64(1)
    while p * p <= n
        if n % p == 0
            max_p = Int64(p)
            while n % p == 0
                n ÷= p
            end
        end
        p += 1
    end
    if n > 1
        max_p = Int64(n)
    end
    return max_p
end

function solve699(limit_pow::Int, out_ptr::Ptr{UInt64})
    limit_N = Int64(10)^limit_pow
    
    # 素数篩（約 5.77 * 10^6 まで必要なので余裕をもって 10^7）
    sieve_max = 10000000
    is_prime = trues(sieve_max)
    is_prime[1] = false
    for p = 2:isqrt(sieve_max)
        if is_prime[p]
            for i = p*p:p:sieve_max
                is_prime[i] = false
            end
        end
    end
    primes = Int64[]
    for p = 2:sieve_max
        if p != 3 && is_prime[p]
            push!(primes, p)
        end
    end

    total_sum = Ref{Int128}(0)

    function get_max_ratio(p_idx::Int, L_rem::Int64)
        ratio = 1.0
        for i in p_idx:length(primes)
            p = primes[i]
            if p > L_rem
                break
            end
            ratio *= p / (p - 1)
            L_rem ÷= p
        end
        return ratio
    end

    function dfs(p_idx::Int, current_r::Int64, num::Int128, den::Int128, v3_sigma::Int, m::Int, L::Int64)
        # 現在の r が条件を満たす場合（分母が完全にキャンセルされた）
        if den == 1
            total_sum[] += Int128(3)^m * current_r
        end

        if p_idx > length(primes)
            return
        end

        p = primes[p_idx]
        if current_r * p > L
            return
        end

        # Prune 1: Max Ratio Pruning (Float誤差回避のため 1.000001 を乗算)
        if den > num
            max_rat = get_max_ratio(p_idx, L ÷ current_r)
            if Float64(num) * max_rat * 1.000001 < Float64(den)
                return
            end
        end

        # Prune 2: 必要なキャンセル素数が未来の範囲を超えている場合
        if den > 1
            P = max_p_factor(den)
            if P - 1 > L ÷ current_r
                return
            end
        end

        limit_for_p = isqrt(L ÷ current_r)

        # Prune 3: Tail Mode (残り1つの素数しか追加できない領域への次元崩壊)
        if p > limit_for_p
            factors = get_prime_factors(num)
            for f in factors
                if f >= p && f != 3
                    fa = f
                    a = 1
                    while num % fa == 0
                        if current_r * fa <= L
                            sigma_fa = get_sigma128(f, a)
                            new_v3 = v3_sigma + v3(sigma_fa)
                            if new_v3 < m
                                if ((num ÷ fa) * sigma_fa) % den == 0
                                    total_sum[] += Int128(3)^m * current_r * fa
                                end
                            end
                        end
                        fa *= f
                        a += 1
                    end
                end
            end
            return
        end

        # 分岐1: p を追加しない
        dfs(p_idx + 1, current_r, num, den, v3_sigma, m, L)

        # 分岐2: p^a を追加する
        pa = p
        a = 1
        while current_r * pa <= L
            sigma_pa = get_sigma128(p, a)
            new_v3 = v3_sigma + v3(sigma_pa)
            
            if new_v3 < m
                new_num = num * sigma_pa
                new_den = den * pa
                g = gcd(new_num, new_den)
                dfs(p_idx + 1, current_r * pa, new_num ÷ g, new_den ÷ g, new_v3, m, L)
            end
            
            pa *= p
            a += 1
        end
    end

    # 3^m の探索
    for m = 1:30
        L = limit_N ÷ (Int64(3)^m)
        if L == 0
            break
        end
        C = get_sigma128(Int64(3), m)
        dfs(1, Int64(1), C, Int128(1), 0, m, L)
    end

    # 128bit メモリへの書き込み
    ts = total_sum[]
    unsafe_store!(out_ptr, UInt64(ts & 0xFFFFFFFFFFFFFFFF), 1)
    unsafe_store!(out_ptr, UInt64((ts >> 64) & 0xFFFFFFFFFFFFFFFF), 2)
end
end # module
")

;;; ----------------------------------------------------------------------
;;; Lisp 実行関数
;;; ----------------------------------------------------------------------
(defun solve ()
  "Find T(10^14)."
  (format t "Initializing Julia Runtime...~%")
  (%jl-init)
  
  (format t "Loading JIT code into Julia...~%")
  (%jl-eval-string *julia-code-699*)
  
  (let* ((limit-pow 14)
         ;; 128bit (16bytes) のメモリ領域を確保
         (out-ptr (cffi:foreign-alloc :uint64 :count 2))
         (result 0))
    
    (setf (cffi:mem-aref out-ptr :uint64 0) 0)
    (setf (cffi:mem-aref out-ptr :uint64 1) 0)
    
    (format t "Executing Julia JIT function via CFFI Zero-Allocation...~%")
    (unwind-protect
         (progn
           (time
            (let ((call-code (format nil "Euler699.solve699(~D, Ptr{UInt64}(~D))" 
                                     limit-pow 
                                     (cffi:pointer-address out-ptr))))
              (%jl-eval-string call-code)))
           
           ;; Lisp側で 128bit のデータを再構築
           (let ((lo (cffi:mem-aref out-ptr :uint64 0))
                 (hi (cffi:mem-aref out-ptr :uint64 1)))
             (setf result (+ lo (ash hi 64)))))
      
      (cffi:foreign-free out-ptr))
    
    (format t "T(10^~D) = ~D~%" limit-pow result)
    result))|#


;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3-flash-preview
(cl:in-package cl-user)
(defpackage #:project-euler-0699 (:use cl iterate alexandria) (:export #:solve))
(in-package #:project-euler-0699)

(defmacro optimized-code-p (boole)
  (typecase boole
    (null nil)
    (T `(declaim (optimize (speed 3) (safety 0) (debug 0) #+lispworks (hcl:fixnum-safety 0))))))

(optimized-code-p T)

(defun get-prime-factors (n)
  (declare (type integer n))
  (let ((factors nil))
    (when (evenp n)
      (push 2 factors)
      (iterate (while (evenp n))
               (setf n (ash n -1))))
    (let ((d 3))
      (declare (type integer d))
      (iterate (while (<= (* d d) n))
               (when (zerop (mod n d))
                 (push d factors)
                 (iterate (while (zerop (mod n d)))
                          (setf n (truncate n d))))
               (incf d 2)))
    (when (> n 1) (push n factors))
    factors))

(defvar *prime-factors-memo* (make-hash-table :test 'eql))

(defun memo-prime-factors (n)
  (or (gethash n *prime-factors-memo*)
      (setf (gethash n *prime-factors-memo*) (get-prime-factors n))))

(defun sigma-val (p e)
  (declare (type integer p) (type fixnum e))
  (let ((sum 1) (term 1))
    (declare (type integer sum term))
    (iterate (for i from 1 to e)
             (setf term (* term p))
             (incf sum term))
    sum))

(defun v3-count (n)
  (declare (type integer n))
  (let ((count 0))
    (declare (type fixnum count))
    (iterate (while (zerop (mod n 3)))
             (incf count)
             (setf n (truncate n 3)))
    count))

(defun solve (&optional (N 100000000000000))
  (clrhash *prime-factors-memo*)
  (let ((ans 0)
        (visited (make-hash-table :test 'eql)))
    (labels ((dfs (M num avail-primes m-limit limit)
               (when (gethash M visited)
                 (return-from dfs))
               (setf (gethash M visited) t)
               
               ;; 不変量の検証：Mが分子を割り切り、かつ約分後の3の指数が正であるか
               (when (and (zerop (mod num M))
                          (< (v3-count num) m-limit))
                 (incf ans (* (expt 3 m-limit) M)))
               
               (iterate (for p in avail-primes)
                 (let ((p-pow p)
                       (e 1))
                   (iterate (while (<= (* M p-pow) limit))
                     (let* ((new-M (* M p-pow))
                            (sig (sigma-val p e))
                            (new-num (* num sig)))
                       (unless (gethash new-M visited)
                         ;; 次の素数候補を差分更新（経路非依存性の担保）
                         (let ((new-factors (memo-prime-factors sig))
                               (next-avail (copy-list avail-primes)))
                           (iterate (for nf in new-factors)
                             (unless (or (= nf 3) (member nf next-avail))
                               (push nf next-avail)))
                           ;; Mに含まれる素数は候補から除外
                           (setf next-avail (remove-if (lambda (x) (zerop (mod new-M x))) next-avail))
                           (dfs new-M new-num next-avail m-limit limit))))
                     (setf p-pow (* p-pow p))
                     (incf e))))))
      
      (let ((max-m (floor (log N 3))))
        (iterate (for m from 1 to max-m)
          (let* ((K (sigma-val 3 m))
                 ;; 初期の利用可能な素数はKの素因数（3を除く）
                 (K-factors (remove 3 (memo-prime-factors K)))
                 (limit (truncate N (expt 3 m))))
            (clrhash visited)
            (dfs 1 K K-factors m limit)
            ;; デバッグと進行状況の観測用ログ
            (format t "Finished m=~2A, limit=~15A, intermediate_ans=~A~%" m limit ans))))
      ans)))