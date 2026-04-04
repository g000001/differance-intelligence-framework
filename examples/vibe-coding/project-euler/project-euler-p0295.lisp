;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3-flash-preview
(cl:in-package cl-user)
(defpackage #:project-euler-0295 (:use cl) (:export #:solve))
(in-package #:project-euler-0295)

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
;;; Julia JIT Code (V2: Exact Root-based Thresholds)
;;; ----------------------------------------------------------------------
(defparameter *julia-code-295-v2* "
module Euler295_V2
export solve295_v2

function is_valid_M(M::Int)
    if M == 1 return true end
    if M % 2 == 0 return false end
    temp = M
    d = 3
    while d * d <= temp
        if temp % d == 0
            if d % 4 == 3 return false end
            while temp % d == 0
                temp ÷= d
            end
        end
        d += 2
    end
    if temp > 1
        if temp % 4 == 3 return false end
    end
    return true
end

function get_n_min(M::Int)
    if M == 1 return 0 end
    best_n = 1000000000
    # K^2 ≡ -1 (mod 2M) となる全てのルートKを探索し、最も条件の緩い(最小の) n_min を探す
    for K in 1:M-1
        if (K * K + 1) % (2 * M) == 0
            L = (K * K + 1) ÷ (2 * M)
            W = K - L
            n_req = W ÷ 2
            if n_req < best_n
                best_n = n_req
            end
        end
    end
    return best_n
end

function solve295_v2(N::Int, out_ptr::Ptr{Int64})
    N2 = Int128(N) * N
    
    pairs = Tuple{Int64, Int32}[]
    sizehint!(pairs, 300000)
    
    for M in 1:5000
        if !is_valid_M(M) continue end
        
        n_min = get_n_min(M)
        n = n_min
        
        while true
            r2 = Int128(M) * (Int128(2) * n * n + Int128(2) * n + 1)
            if r2 > N2 break end
            push!(pairs, (Int64(r2), Int32(M)))
            n += 1
        end
    end
    
    sort!(pairs)
    
    freq = Dict{Vector{Int32}, Int64}()
    
    if !isempty(pairs)
        current_r2 = pairs[1][1]
        current_M_list = Int32[pairs[1][2]]
        
        for i in 2:length(pairs)
            r2 = pairs[i][1]
            M = pairs[i][2]
            if r2 == current_r2
                push!(current_M_list, M)
            else
                sort!(current_M_list)
                freq[current_M_list] = get(freq, current_M_list, 0) + 1
                current_r2 = r2
                current_M_list = Int32[M]
            end
        end
        sort!(current_M_list)
        freq[current_M_list] = get(freq, current_M_list, 0) + 1
    end
    
    ans = Int64(0)
    sets = collect(keys(freq))
    
    for i in 1:length(sets)
        S1 = sets[i]
        c1 = freq[S1]
        # 同一セット内でのペア
        ans += c1 * (c1 + 1) ÷ 2
        
        # 異なるセット間のペア (少なくとも1つの生成元 M が一致すればよい)
        for j in (i+1):length(sets)
            S2 = sets[j]
            c2 = freq[S2]
            
            intersect_found = false
            for x in S1
                for y in S2
                    if x == y
                        intersect_found = true
                        break
                    end
                end
                if intersect_found break end
            end
            
            if intersect_found
                ans += c1 * c2
            end
        end
    end
    
    unsafe_store!(out_ptr, ans)
end
end # module
")

;;; ----------------------------------------------------------------------
;;; Lisp 実行関数
;;; ----------------------------------------------------------------------
(defun solve ()
  "Find L(100000)."
  (format t "Initializing Julia Runtime...~%")
  (%jl-init)
  
  (format t "Loading JIT code into Julia (V2)...~%")
  (%jl-eval-string *julia-code-295-v2*)
  
  (let* ((n 100000)
         (out-ptr (cffi:foreign-alloc :int64))
         (result 0))
    
    (setf (cffi:mem-ref out-ptr :int64) 0)
    
    (format t "Executing Julia JIT function via CFFI Zero-Allocation...~%")
    (unwind-protect
         (progn
           (time
            (let ((call-code (format nil "Euler295_V2.solve295_v2(~D, Ptr{Int64}(~D))" 
                                     n
                                     (cffi:pointer-address out-ptr))))
              (%jl-eval-string call-code)))
           
           (setf result (cffi:mem-ref out-ptr :int64)))
      
      (cffi:foreign-free out-ptr))
    
    (format t "L(~D) = ~D~%" n result)
    result))