;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3-flash-preview
(cl:in-package cl-user)
(defpackage #:project-euler-0920 (:use cl) (:export #:solve))
(in-package #:project-euler-0920)

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
;;; Julia JIT Code V2 (Int128 & m(1)=1 Fixed)
;;; ----------------------------------------------------------------------
(defparameter *julia-code-v2* "
module Euler920_V2
export solve920_v2

function safe_pow(base::Int64, exp::Int, limit::Int64)
    res = Int64(1)
    b = Int64(base)
    for i in 1:exp
        if limit ÷ b < res
            return limit + 1
        end
        res *= b
    end
    return res
end

function solve920_v2(limit_pow::Int, out_ptr::Ptr{UInt64})
    limit = Int64(10)^limit_pow
    primes = [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97, 101, 103, 107, 109, 113, 127, 131, 137, 139, 149, 151, 157, 163, 167, 173]

    # 数学的修正: m(1) = 1 をあらかじめ定義しておく
    min_x_dict = Dict{Int, Int64}(1 => 1)

    function process_multiset(E::Vector{Int})
        k = 1
        for e in E
            k *= (e + 1)
        end

        temp_k = k
        Pk = Int[]
        vk = Int[]
        d = 2
        while d * d <= temp_k
            if temp_k % d == 0
                count = 0
                while temp_k % d == 0
                    count += 1
                    temp_k ÷= d
                end
                push!(Pk, d)
                push!(vk, count)
            end
            d += 1
        end
        if temp_k > 1
            push!(Pk, temp_k)
            push!(vk, 1)
        end

        m = length(E)
        if length(Pk) > m
            return
        end

        P_used = copy(Pk)
        idx = 1
        while length(P_used) < m
            p = primes[idx]
            if !(p in P_used)
                push!(P_used, p)
            end
            idx += 1
        end
        sort!(P_used)

        counts = Dict{Int, Int}()
        for e in E
            counts[e] = get(counts, e, 0) + 1
        end
        unique_E = sort!(collect(keys(counts)), rev=true)

        best_x = limit + 1

        function search(p_idx::Int, current_x::Int64)
            if p_idx > m
                if current_x < best_x
                    best_x = current_x
                end
                return
            end

            p = P_used[p_idx]
            req_e = 0
            idx_pk = findfirst(==(p), Pk)
            if idx_pk !== nothing
                req_e = vk[idx_pk]
            end

            for e in unique_E
                if counts[e] > 0
                    if e >= req_e
                        term = safe_pow(Int64(p), e, limit)
                        if term <= limit && current_x <= limit ÷ term
                            next_x = current_x * term
                            if next_x < best_x
                                counts[e] -= 1
                                search(p_idx + 1, next_x)
                                counts[e] += 1
                            end
                        end
                    end
                end
            end
        end

        search(1, 1)

        if best_x <= limit
            if !haskey(min_x_dict, k) || best_x < min_x_dict[k]
                min_x_dict[k] = best_x
            end
        end
    end

    function generate(current_E::Vector{Int}, current_x_min::Int64, prime_idx::Int, max_e::Int)
        if !isempty(current_E)
            process_multiset(current_E)
        end

        p = primes[prime_idx]
        e = 1
        while e <= max_e
            term = safe_pow(Int64(p), e, limit)
            if term <= limit && current_x_min <= limit ÷ term
                next_x = current_x_min * term
                push!(current_E, e)
                generate(current_E, next_x, prime_idx + 1, e)
                pop!(current_E)
            else
                break
            end
            e += 1
        end
    end

    generate(Int[], Int64(1), 1, 60)

    # 集計 (Int128 による完全なオーバーフロー防御)
    total_sum = Int128(0)
    for v in values(min_x_dict)
        total_sum += Int128(v)
    end

    # CFFIポインタ(UInt64 x 2)に128bitを分割して書き込む
    unsafe_store!(out_ptr, UInt64(total_sum & 0xFFFFFFFFFFFFFFFF), 1)
    unsafe_store!(out_ptr, UInt64(total_sum >> 64), 2)
end
end # module
")

;;; ----------------------------------------------------------------------
;;; Lisp 実行関数
;;; ----------------------------------------------------------------------
(defun solve ()
  "Find M(16): the sum of all m(k) whose values do not exceed 10^16."
  (format t "Initializing Julia Runtime...~%")
  (%jl-init)
  
  (format t "Loading JIT code into Julia...~%")
  (%jl-eval-string *julia-code-v2*)
  
  (let* ((limit-pow 16)
         ;; 128bit (16bytes) のメモリ領域を2つのUInt64として確保
         (out-ptr (cffi:foreign-alloc :uint64 :count 2))
         (result 0))
    
    (setf (cffi:mem-aref out-ptr :uint64 0) 0)
    (setf (cffi:mem-aref out-ptr :uint64 1) 0)
    
    (format t "Executing Julia JIT function via CFFI Zero-Allocation...~%")
    (unwind-protect
         (progn
           (time
            (let ((call-code (format nil "Euler920_V2.solve920_v2(~D, Ptr{UInt64}(~D))" 
                                     limit-pow 
                                     (cffi:pointer-address out-ptr))))
              (%jl-eval-string call-code)))
           
           ;; Lisp側で 128bit のデータを再構築
           (let ((lo (cffi:mem-aref out-ptr :uint64 0))
                 (hi (cffi:mem-aref out-ptr :uint64 1)))
             (setf result (+ lo (ash hi 64)))))
      
      (cffi:foreign-free out-ptr))
    
    (format t "M(~D) = ~D~%" limit-pow result)
    result))

#+| Do it | (solve )
#|------------------------------------------------------------|
Timing the evaluation of (solve)
Initializing Julia Runtime...
Loading JIT code into Julia...
Executing Julia JIT function via CFFI Zero-Allocation...
Timing the evaluation of (let ((call-code
                                (format nil
                                        "Euler920_V2.solve920_v2(~D, Ptr{UInt64}(~D))"
                                        limit-pow
                                        (cffi-sys:pointer-address out-ptr))))
                           (%jl-eval-string call-code))

User time    =        0.766
System time  =        0.019
Elapsed time =        0.790
Allocation   = 24336 bytes
10591 Page faults
GC time      =        0.000
M(16) = 1154027691000533893

User time    =        1.176
System time  =        0.090
Elapsed time =        1.442
Allocation   = 287728 bytes
80497 Page faults
GC time      =        0.000
 |------------------------------------------------------------|#
;;→ 1154027691000533893
:ok
