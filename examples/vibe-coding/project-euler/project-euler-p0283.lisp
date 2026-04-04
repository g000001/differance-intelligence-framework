;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3-flash-preview
(cl:in-package cl-user)
(defpackage #:project-euler-0283 (:use cl) (:export #:solve))
(in-package #:project-euler-0283)

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
(defparameter *julia-code-283* "
module Euler283
export solve283

# MAX_VAL = 2000^2 + (2000 * sqrt(3))^2 ≈ 1.6 * 10^7
const MAX_VAL = 16000005
const spf = zeros(Int32, MAX_VAL)

const factors = zeros(Int64, 64)
const counts = zeros(Int, 64)
const factors_len = Ref{Int}(0)

const sum_p = Ref{Int128}(0)

function init_spf()
    @inbounds for i in 1:MAX_VAL
        spf[i] = i
    end
    @inbounds for i in 2:isqrt(MAX_VAL)
        if spf[i] == i
            for j in (i*i):i:MAX_VAL
                if spf[j] == j
                    spf[j] = i
                end
            end
        end
    end
end

function add_factor(p::Int64, c::Int)
    len = factors_len[]
    @inbounds for i in 1:len
        if factors[i] == p
            counts[i] += c
            return
        end
    end
    len += 1
    @inbounds factors[len] = p
    @inbounds counts[len] = c
    factors_len[] = len
end

function factorize(r::Int64, x_sq_plus_r_sq::Int64)
    factors_len[] = 0
    
    temp_r = r
    while temp_r > 1
        @inbounds p = Int64(spf[temp_r])
        c = 0
        while temp_r % p == 0
            c += 1
            temp_r ÷= p
        end
        add_factor(p, 2 * c)
    end
    
    temp_val = x_sq_plus_r_sq
    while temp_val > 1
        @inbounds p = Int64(spf[temp_val])
        c = 0
        while temp_val % p == 0
            c += 1
            temp_val ÷= p
        end
        add_factor(p, c)
    end
end

# GCを回避するため引数で引き回すクロージャフリー設計
function dfs(idx::Int, current_u::Int64, x::Int64, r_sq::Int64, x_sq_minus_r_sq::Int64, K::Int64, sqrt_K::Int64)
    if idx > factors_len[]
        if current_u <= sqrt_K
            if current_u >= x_sq_minus_r_sq
                if (current_u + r_sq) % x == 0
                    v = K ÷ current_u
                    if (v + r_sq) % x == 0
                        y = (current_u + r_sq) ÷ x
                        z = (v + r_sq) ÷ x
                        sum_p[] += Int128(2) * (x + y + z)
                    end
                end
            end
        end
        return
    end
    
    @inbounds p = factors[idx]
    @inbounds c = counts[idx]
    pk = Int64(1)
    for i in 0:c
        dfs(idx + 1, current_u * pk, x, r_sq, x_sq_minus_r_sq, K, sqrt_K)
        pk *= p
    end
end

function solve283(limit_ratio::Int, out_ptr::Ptr{UInt64})
    init_spf()
    sum_p[] = 0
    
    limit_r = 2 * limit_ratio
    for r in 2:2:limit_r
        r_sq = Int64(r) * r
        max_x = floor(Int, sqrt(3.0) * r)
        
        for x in 1:max_x
            x_sq = Int64(x) * x
            x_sq_plus_r_sq = x_sq + r_sq
            x_sq_minus_r_sq = x_sq - r_sq
            K = r_sq * x_sq_plus_r_sq
            sqrt_K = isqrt(K)
            
            factorize(Int64(r), x_sq_plus_r_sq)
            
            dfs(1, Int64(1), Int64(x), r_sq, x_sq_minus_r_sq, K, sqrt_K)
        end
    end
    
    # 128bitの総和を分解してCFFIポインタに渡す
    ts = sum_p[]
    unsafe_store!(out_ptr, UInt64(ts & 0xFFFFFFFFFFFFFFFF), 1)
    unsafe_store!(out_ptr, UInt64((ts >> 64) & 0xFFFFFFFFFFFFFFFF), 2)
end
end # module
")

;;; ----------------------------------------------------------------------
;;; Lisp 実行関数
;;; ----------------------------------------------------------------------
(defun solve ()
  "Find the sum of the perimeters of all integer sided triangles for which the area/perimeter ratios are equal to positive integers not exceeding 1000."
  (format t "Initializing Julia Runtime...~%")
  (%jl-init)
  
  (format t "Loading JIT code into Julia...~%")
  (%jl-eval-string *julia-code-283*)
  
  (let* ((limit-ratio 1000)
         ;; 128bit (16bytes) のメモリ領域を確保
         (out-ptr (cffi:foreign-alloc :uint64 :count 2))
         (result 0))
    
    (setf (cffi:mem-aref out-ptr :uint64 0) 0)
    (setf (cffi:mem-aref out-ptr :uint64 1) 0)
    
    (format t "Executing Julia JIT function via CFFI Zero-Allocation...~%")
    (unwind-protect
        (progn
          (let ((call-code (format nil "Euler283.solve283(~D, Ptr{UInt64}(~D))" 
                                   limit-ratio 
                                   (cffi:pointer-address out-ptr))))
            (%jl-eval-string call-code))
           
          ;; Lisp側で 128bit のデータを再構築
          (let ((lo (cffi:mem-aref out-ptr :uint64 0))
                (hi (cffi:mem-aref out-ptr :uint64 1)))
            (setf result (+ lo (ash hi 64)))))
      
      (cffi:foreign-free out-ptr))
    
    (format t "Sum of perimeters for ratios <= ~D = ~D~%" limit-ratio result)
    result))


#+| Do it | (solve )
#|------------------------------------------------------------|
Timing the evaluation of (solve)
Initializing Julia Runtime...
Loading JIT code into Julia...
Executing Julia JIT function via CFFI Zero-Allocation...
Sum of perimeters for ratios <= 1000 = 28038042525570324

User time    =        5.214
System time  =        0.027
Elapsed time =        5.178
Allocation   = 202240 bytes
16463 Page faults
GC time      =        0.000
 |------------------------------------------------------------|#
;;→ 28038042525570324
:ok