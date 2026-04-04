;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3-flash-preview
(cl:in-package cl-user)
(defpackage #:project-euler-0289 (:use cl) (:export #:solve))
(in-package #:project-euler-0289)

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
(defparameter *julia-code-289* "
module Euler289
export solve289

const MOD = Int64(10000000000)

function _get_matchings(pts::Vector{Int})
    if isempty(pts) return [Vector{Tuple{Int,Int}}()] end
    res = Vector{Vector{Tuple{Int,Int}}}()
    first = pts[1]
    for i in 2:2:length(pts)
        second = pts[i]
        left_pts = pts[2:i-1]
        right_pts = pts[i+1:end]
        
        m_lefts = _get_matchings(left_pts)
        m_rights = _get_matchings(right_pts)
        
        for m_l in m_lefts
            for m_r in m_rights
                push!(res, vcat([(first, second)], m_l, m_r))
            end
        end
    end
    return res
end

function decode!(val::UInt64, len::Int, state::Vector{Int})
    for i in 1:len
        state[i] = Int((val >> (4 * (i - 1))) & 0xF)
    end
end

# ゼロアロケーション用グローバルバッファ
const MAP = zeros(Int, 32)
const NEW_STATE = zeros(Int, 32)
const POINT_COMP = zeros(Int, 16)
const C_BUF = zeros(Int, 32)
const STATE_BUF = zeros(Int, 32)

function solve289(m::Int, n::Int, out_ptr::Ptr{UInt64})
    MATCHINGS = Dict{Int, Vector{Vector{Tuple{Int,Int}}}}()
    MATCHINGS[0] = [Vector{Tuple{Int,Int}}()]
    for k in 2:2:8
        MATCHINGS[k] = _get_matchings(collect(1:k))
    end
    
    dp = Dict{UInt64, Int64}()
    dp[UInt64(0)] = 1
    
    current_S = 0
    ans = Int64(0)
    
    for y in 0:n
        for x in 0:m
            L = x > 0 ? (y == 0 || y == n ? 1 : 2) : 0
            R = x < m ? (y == 0 || y == n ? 1 : 2) : 0
            D = y > 0 ? (x == 0 || x == m ? 1 : 2) : 0
            U = y < n ? (x == 0 || x == m ? 1 : 2) : 0
            
            offset = 0
            for i in 0:x-1
                offset += (y < n ? (i == 0 || i == m ? 1 : 2) : 0)
            end
            
            k = L + D + U + R
            matchings = MATCHINGS[k]
            new_dp = Dict{UInt64, Int64}()
            
            for (state_val, count) in dp
                decode!(state_val, current_S, STATE_BUF)
                
                for matching in matchings
                    for i in 1:current_S
                        C_BUF[i] = STATE_BUF[i]
                    end
                    
                    max_id = 0
                    for i in 1:current_S
                        if STATE_BUF[i] > max_id; max_id = STATE_BUF[i]; end
                    end
                    
                    for p in 1:(U+R)
                        POINT_COMP[p] = max_id + p
                    end
                    
                    for p in 1:(L+D)
                        idx_in_frontier = offset + (L + D) - p + 1
                        POINT_COMP[U+R+p] = C_BUF[idx_in_frontier]
                    end
                    
                    loops_closed = 0
                    
                    for (u, v) in matching
                        cu = POINT_COMP[u]
                        cv = POINT_COMP[v]
                        if cu == cv
                            loops_closed += 1
                        else
                            for i in 1:current_S
                                if C_BUF[i] == cv; C_BUF[i] = cu; end
                            end
                            for i in 1:k
                                if POINT_COMP[i] == cv; POINT_COMP[i] = cu; end
                            end
                        end
                    end
                    
                    if loops_closed > 0
                        # 単一のループが正確に最後の頂点で閉じた場合のみ有効
                        if loops_closed == 1 && x == m && y == n
                            ans = (ans + count) % MOD
                        end
                        continue
                    end
                    
                    if x == m && y == n
                        continue
                    end
                    
                    new_S = current_S - (L + D) + (U + R)
                    
                    for i in 1:offset
                        NEW_STATE[i] = C_BUF[i]
                    end
                    for i in 1:(U+R)
                        NEW_STATE[offset + i] = POINT_COMP[i]
                    end
                    for i in (offset + L + D + 1):current_S
                        NEW_STATE[i - (L + D) + (U + R)] = C_BUF[i]
                    end
                    
                    fill!(MAP, 0)
                    next_id = 1
                    for i in 1:new_S
                        v = NEW_STATE[i]
                        if MAP[v] == 0
                            MAP[v] = next_id
                            next_id += 1
                        end
                        NEW_STATE[i] = MAP[v]
                    end
                    
                    new_val = UInt64(0)
                    for i in 1:new_S
                        new_val |= UInt64(NEW_STATE[i]) << (4 * (i - 1))
                    end
                    
                    new_dp[new_val] = (get(new_dp, new_val, 0) + count) % MOD
                end
            end
            
            dp = new_dp
            current_S = current_S - (L + D) + (U + R)
        end
    end
    
    unsafe_store!(out_ptr, UInt64(ans))
end
end # module
")

;;; ----------------------------------------------------------------------
;;; Lisp 実行関数
;;; ----------------------------------------------------------------------
(defun solve ()
  "Find L(6,10) mod 10^10."
  (format t "Initializing Julia Runtime...~%")
  (%jl-init)
  
  (format t "Loading JIT code into Julia...~%")
  (%jl-eval-string *julia-code-289*)
  
  (let* ((m 6)
         (n 10)
         (out-ptr (cffi:foreign-alloc :uint64))
         (result 0))
    
    (setf (cffi:mem-ref out-ptr :uint64) 0)
    
    (format t "Executing Julia JIT function via CFFI Zero-Allocation...~%")
    (unwind-protect
         (progn
           (let ((call-code (format nil "Euler289.solve289(~D, ~D, Ptr{UInt64}(~D))" 
                                     m n 
                                     (cffi:pointer-address out-ptr))))
              (%jl-eval-string call-code))
           
           (setf result (cffi:mem-ref out-ptr :uint64)))
      
      (cffi:foreign-free out-ptr))
    
    (format t "L(~D, ~D) mod 10^10 = ~D~%" m n result)
    result))


#+| Do it | (solve )
#|------------------------------------------------------------|
Timing the evaluation of (solve)
Initializing Julia Runtime...
Loading JIT code into Julia...
Executing Julia JIT function via CFFI Zero-Allocation...
L(6, 10) mod 10^10 = 6567944538

User time    =        0.389
System time  =        0.013
Elapsed time =        0.351
Allocation   = 92848 bytes
2682 Page faults
GC time      =        0.000
 |------------------------------------------------------------|#
;;→ 6567944538
:ok