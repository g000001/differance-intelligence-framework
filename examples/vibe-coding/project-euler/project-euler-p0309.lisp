;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3-flash-preview
(cl:in-package cl-user)
(defpackage #:project-euler-0309 (:use cl) (:export #:solve))
(in-package #:project-euler-0309)

(defmacro source-pathname ()
  "Compute source pathname"
  `(load-time-value ,(or *compile-file-truename* *load-truename* (uiop:getcwd))))



(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :cffi)
  (ql:quickload :uiop))

;;; ----------------------------------------------------------------------
;;; Go コードのインライン生成とコンパイル
;;; ----------------------------------------------------------------------
(let* ((base-dir (uiop:pathname-directory-pathname (source-pathname)))
       (go-file (merge-pathnames "solution309.go" base-dir))
       (lib-file (merge-pathnames "libeuler309_go.so" base-dir)))
  
  ;; Goコードの書き出し
  (with-open-file (out go-file :direction :output :if-exists :supersede)
    (write-string 
"package main

import \"C\"
import \"unsafe\"

//export Solve309
func Solve309(limit C.int, headPtr *C.int, nextPtr *C.int, valPtr *C.int, maxNodes C.int) C.int {
    N := int(limit)
    // Lisp側で確保したCポインタをGoのスライスとして安全にキャスト
    head := unsafe.Slice((*int32)(unsafe.Pointer(headPtr)), N)
    next := unsafe.Slice((*int32)(unsafe.Pointer(nextPtr)), int(maxNodes))
    val  := unsafe.Slice((*int32)(unsafe.Pointer(valPtr)), int(maxNodes))

    // リンクドリストのヘッドを初期化 (-1 is null)
    for i := 0; i < N; i++ {
        head[i] = -1
    }

    nodeCount := int32(0)

    // Lispから渡されたメモリ領域にノードを追加 (Zero Allocation)
    addNode := func(w, a int) {
        if w >= N { return }
        if nodeCount >= int32(maxNodes) { return }
        idx := nodeCount
        nodeCount++
        val[idx] = int32(a)
        next[idx] = head[w]
        head[w] = idx
    }

    gcd := func(a, b int) int {
        for b != 0 {
            a, b = b, a%b
        }
        return a
    }

    // 原始ピタゴラス数の生成と倍数の展開
    for m := 2; m*m < N; m++ {
        for n := 1; n < m; n++ {
            if (m-n)%2 == 1 && gcd(m, n) == 1 {
                a0 := m*m - n*n
                b0 := 2 * m * n
                c0 := m*m + n*n
                if c0 >= N { continue }
                
                // 倍数 k を掛けて展開し、斜辺 c < N のものをすべて登録
                for k := 1; k*c0 < N; k++ {
                    addNode(k*a0, k*b0)
                    addNode(k*b0, k*a0)
                }
            }
        }
    }

    count := 0
    var as [4000]int // 各 w に対する a の値を格納するローカルバッファ
    
    // 各 w について、a の組み合わせを評価
    for w := 1; w < N; w++ {
        nA := 0
        for idx := head[w]; idx != -1; idx = next[idx] {
            if nA < 4000 {
                as[nA] = int(val[idx])
                nA++
            }
        }
        
        // ペア (a, b) の検証
        for i := 0; i < nA; i++ {
            a := as[i]
            for j := i + 1; j < nA; j++ {
                b := as[j]
                // a * b が a + b で割り切れるか
                if int64(a)*int64(b)%int64(a+b) == 0 {
                    count++
                }
            }
        }
    }
    return C.int(count)
}

func main() {}
" out))

  ;; GoコードをC共有ライブラリとしてビルド
  (unless (uiop:file-exists-p lib-file)
    (format t "Building Go shared library...~%")
    (uiop:run-program 
     (list "go" "build" "-buildmode=c-shared" "-o" (namestring lib-file) (namestring go-file))))
  
  ;; CFFIでライブラリをロード
  (cffi:load-foreign-library lib-file))

;;; ----------------------------------------------------------------------
;;; CFFI バインディングと Lisp 実行関数
;;; ----------------------------------------------------------------------
(cffi:defcfun ("Solve309" %solve-go) :int
  (limit :int)
  (head-ptr :pointer)
  (next-ptr :pointer)
  (val-ptr :pointer)
  (max-nodes :int))

(defun solve ()
  "Find the number of triplets (x, y, h) producing integer solutions for w."
  (let* ((limit #.(expt 10 6))
         ;; 斜辺 < 10^6 のピタゴラス数の辺は合計で約400万個。
         ;; 余裕を持たせて800万ノード分のメモリをLisp側で確保する。
         (max-nodes #.(* 8 (expt 10 6))) 
         (head-ptr (cffi:foreign-alloc :int32 :count limit))
         (next-ptr (cffi:foreign-alloc :int32 :count max-nodes))
         (val-ptr  (cffi:foreign-alloc :int32 :count max-nodes))
         (result 0))
    
    (format t "Allocated shared memory blocks for Lisp-Go CFFI transfer.~%")
    
    (unwind-protect
        (setf result (%solve-go limit head-ptr next-ptr val-ptr max-nodes))
      ;; メモリリーク防止のための確実な解放
      (cffi:foreign-free head-ptr)
      (cffi:foreign-free next-ptr)
      (cffi:foreign-free val-ptr))
    
    (format t "Result: ~D~%" result)
    result))


#+| Do it | (SOLVE )
#|------------------------------------------------------------|
Timing the evaluation of (SOLVE)
Allocated shared memory blocks for Lisp-Go CFFI transfer.
Result: 210139

User time    =        0.733
System time  =        0.026
Elapsed time =        0.704
Allocation   = 223600 bytes
4693 Page faults
GC time      =        0.000
 |------------------------------------------------------------|#
;;→ 210139
:ok