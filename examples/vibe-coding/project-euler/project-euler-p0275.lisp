;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3-flash-preview
(cl:in-package cl-user)
(defpackage #:project-euler-0275 (:use cl) (:export #:solve))
(in-package #:project-euler-0275)

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
(defparameter *julia-code-275* "
module Euler275
export solve275

const W = 40
const OFFSET_X = 20
const H = 25
const MAX_N = 18

const board = zeros(UInt8, W * H)
const poly = zeros(Int, MAX_N)

# 深さごとのアロケーションをゼロにするための静的バッファ
const untried_arr = [zeros(Int, 100) for _ in 1:MAX_N+1]
const new_nb_arr = [zeros(Int, 4) for _ in 1:MAX_N+1]

const DIRS = [-W, W, -1, 1]
const all_count = Ref{Int64}(0)
const sym_count = Ref{Int64}(0)

function is_symmetric(count::Int)
    @inbounds for i in 1:count
        u = poly[i]
        x = (u % W) - OFFSET_X
        y = u ÷ W
        sym_x = -x
        sym_u = y * W + (sym_x + OFFSET_X)
        if board[sym_u] != 1
            return false
        end
    end
    return true
end

function search(depth::Int, ut_len::Int, count::Int, sum_x::Int)
    if count == MAX_N
        if sum_x == 0
            all_count[] += 1
            if is_symmetric(count)
                sym_count[] += 1
            end
        end
        return
    end
    
    if ut_len == 0
        return
    end

    rem = MAX_N - count
    u_max = -100
    u_min = 100
    ut_list = untried_arr[depth]
    
    # Untried setのx座標の最小・最大を走査
    @inbounds for i in 1:ut_len
        u = ut_list[i]
        x = (u % W) - OFFSET_X
        if x > u_max; u_max = x; end
        if x < u_min; u_min = x; end
    end
    
    # 枝刈り: 残りのブロックを一直線に伸ばした場合の最大/最小x変位を計算
    max_possible = sum_x + rem * u_max + (rem * (rem - 1)) ÷ 2
    if max_possible < 0 return end
    min_possible = sum_x + rem * u_min - (rem * (rem - 1)) ÷ 2
    if min_possible > 0 return end

    next_depth = depth + 1
    
    @inbounds for i in 1:ut_len
        u = ut_list[i]
        
        poly[count + 1] = u
        board[u] = 1 # in poly
        x_u = (u % W) - OFFSET_X
        
        nb_len = 0
        for dir in DIRS
            nb = u + dir
            # nb >= W によって y > 0 の制約を透過的に保証
            if nb >= W && nb < W*H
                if board[nb] == 0
                    board[nb] = 2
                    nb_len += 1
                    new_nb_arr[depth][nb_len] = nb
                end
            end
        end
        
        # 次の深さの Untried set を準備
        next_ut_len = 0
        for j in (i+1):ut_len
            next_ut_len += 1
            untried_arr[next_depth][next_ut_len] = ut_list[j]
        end
        for j in 1:nb_len
            next_ut_len += 1
            untried_arr[next_depth][next_ut_len] = new_nb_arr[depth][j]
        end
        
        search(next_depth, next_ut_len, count + 1, sum_x + x_u)
        
        # バックトラック
        board[u] = 2 # restore untried state
        for j in 1:nb_len
            board[new_nb_arr[depth][j]] = 0
        end
    end
end

function solve275(out_ptr::Ptr{Int64})
    all_count[] = 0
    sym_count[] = 0
    fill!(board, 0)
    
    # 探索の開始セルは (0, 1) のみ
    start_u = 1 * W + OFFSET_X
    board[start_u] = 2
    untried_arr[1][1] = start_u
    
    search(1, 1, 0, 0)
    
    # y軸対称なものは1回、非対称なものは左右で2回生成されるため、この式で完全に一意な総数が求まる
    ans = (all_count[] + sym_count[]) ÷ 2
    unsafe_store!(out_ptr, ans)
end
end # module
")

;;; ----------------------------------------------------------------------
;;; Lisp 実行関数
;;; ----------------------------------------------------------------------
(defun solve ()
  "Find the number of balanced sculptures of order 18."
  (format t "Initializing Julia Runtime...~%")
  (%jl-init)
  
  (format t "Loading JIT code into Julia...~%")
  (%jl-eval-string *julia-code-275*)
  
  (let* ((out-ptr (cffi:foreign-alloc :int64))
         (result 0))
    
    (setf (cffi:mem-ref out-ptr :int64) 0)
    
    (format t "Executing Julia JIT function via CFFI Zero-Allocation...~%")
    (unwind-protect
         (progn
           (let ((call-code (format nil "Euler275.solve275(Ptr{Int64}(~D))" 
                                     (cffi:pointer-address out-ptr))))
              (%jl-eval-string call-code))
           
           (setf result (cffi:mem-ref out-ptr :int64)))
      
      (cffi:foreign-free out-ptr))
    
    (format t "Balanced sculptures of order 18 = ~D~%" result)
    result))


#+| Do it | (solve )
#|------------------------------------------------------------|
Timing the evaluation of (solve)
Initializing Julia Runtime...
Loading JIT code into Julia...
Executing Julia JIT function via CFFI Zero-Allocation...
Balanced sculptures of order 18 = 15030564

User time    =       12.998
System time  =        0.127
Elapsed time =       13.263
Allocation   = 342576 bytes
73220 Page faults
GC time      =        0.000
 |------------------------------------------------------------|#
;;→ 15030564
:ok