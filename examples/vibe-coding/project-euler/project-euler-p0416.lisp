;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3-flash-preview
(cl:in-package cl-user)
(defpackage #:project-euler-0416 (:use cl) (:export #:solve))
(in-package #:project-euler-0416)

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
(defparameter *julia-code-416* "
module Euler416
export solve416

function multinomial(j1::Int, j2::Int, j3::Int)
    c0 = j1 + j2 + j3
    res = Int128(1)
    for i in 1:c0
        res *= i
    end
    fj1 = Int128(1)
    for i in 1:j1
        fj1 *= i
    end
    fj2 = Int128(1)
    for i in 1:j2
        fj2 *= i
    end
    fj3 = Int128(1)
    for i in 1:j3
        fj3 *= i
    end
    return Int64(res ÷ (fj1 * fj2 * fj3))
end

function mul(A::Matrix{Int64}, B::Matrix{Int64})
    N = size(A, 1)
    C = zeros(Int64, N, N)
    @inbounds for j in 1:N
        for i in 1:N
            s = Int128(0)
            for k in 1:N
                s += Int128(A[i,k]) * B[k,j]
            end
            C[i,j] = Int64(s % 1000000000)
        end
    end
    return C
end

function power_pair(A::Matrix{Int64}, B::Matrix{Int64}, p::Int64)
    N = size(A, 1)
    res_A = zeros(Int64, N, N)
    for i in 1:N
        res_A[i,i] = 1
    end
    res_B = zeros(Int64, N, N)
    
    base_A = copy(A)
    base_B = copy(B)
    
    while p > 0
        if p & 1 == 1
            new_res_A = mul(res_A, base_A)
            new_res_B = (mul(res_A, base_B) .+ mul(res_B, base_A)) .% 1000000000
            res_A = new_res_A
            res_B = new_res_B
        end
        if p > 1
            new_base_A = mul(base_A, base_A)
            new_base_B = (mul(base_A, base_B) .+ mul(base_B, base_A)) .% 1000000000
            base_A = new_base_A
            base_B = new_base_B
        end
        p >>= 1
    end
    return res_A, res_B
end

function solve416(m::Int, n::Int64, out_ptr::Ptr{Int64})
    k = 2 * m
    MOD = 1000000000
    
    # 状態の生成： c0 + c1 + c2 = k
    states = Tuple{Int,Int,Int}[]
    for c0 in 0:k
        for c1 in 0:(k - c0)
            c2 = k - c0 - c1
            push!(states, (c0, c1, c2))
        end
    end
    
    N = length(states)
    state_to_idx = Dict{Tuple{Int,Int,Int}, Int}()
    for (i, st) in enumerate(states)
        state_to_idx[st] = i
    end
    
    M_gt0 = zeros(Int64, N, N)
    M_eq0 = zeros(Int64, N, N)
    
    for i in 1:N
        c0, c1, c2 = states[i]
        for j in 1:N
            cp0, cp1, cp2 = states[j]
            
            j3 = cp2
            j2 = cp1 - c2
            j1 = cp0 - c1
            
            if j1 >= 0 && j2 >= 0 && j3 >= 0 && (j1 + j2 + j3 == c0)
                w = multinomial(j1, j2, j3) % MOD
                if cp0 > 0
                    M_gt0[i, j] = (M_gt0[i, j] + w) % MOD
                else
                    M_eq0[i, j] = (M_eq0[i, j] + w) % MOD
                end
            end
        end
    end
    
    # 行列ペアの累乗
    res_A, res_B = power_pair(M_gt0, M_eq0, n - 1)
    
    # 初期状態・終了状態のインデックス
    start_idx = state_to_idx[(k, 0, 0)]
    
    ans = (res_A[start_idx, start_idx] + res_B[start_idx, start_idx]) % MOD
    
    unsafe_store!(out_ptr, ans)
end
end # module
")

;;; ----------------------------------------------------------------------
;;; Lisp 実行関数
;;; ----------------------------------------------------------------------
(defun solve ()
  "Find the last 9 digits of F(10, 10^12)."
  (format t "Initializing Julia Runtime...~%")
  (%jl-init)
  
  (format t "Loading JIT code into Julia...~%")
  (%jl-eval-string *julia-code-416*)
  
  (let* ((m 10)
         (n #.(expt 10 12))
         (out-ptr (cffi:foreign-alloc :int64))
         (result 0))
    
    (setf (cffi:mem-ref out-ptr :int64) 0)
    
    (format t "Executing Julia JIT function via CFFI Zero-Allocation...~%")
    (unwind-protect
         (progn
           (let ((call-code (format nil "Euler416.solve416(~D, ~D, Ptr{Int64}(~D))" 
                                     m n 
                                     (cffi:pointer-address out-ptr))))
              (%jl-eval-string call-code))
           
           (setf result (cffi:mem-ref out-ptr :int64)))
      
      (cffi:foreign-free out-ptr))
    
    (format t "Last 9 digits of F(~D, 10^12) = ~9,'0D~%" m result)
    result))


#+| Do it | (solve )
#|------------------------------------------------------------|
Timing the evaluation of (solve)
Initializing Julia Runtime...
Loading JIT code into Julia...
Executing Julia JIT function via CFFI Zero-Allocation...
Last 9 digits of F(10, 10^12) = 898082747

User time    =        2.688
System time  =        0.039
Elapsed time =        2.677
Allocation   = 112536 bytes
2776 Page faults
GC time      =        0.000
 |------------------------------------------------------------|#
;;→ 898082747
:ok