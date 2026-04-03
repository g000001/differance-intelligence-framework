;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3-flash-preview
(cl:in-package cl-user)
(defpackage #:project-euler-0300 (:use cl) (:export #:solve))
(in-package #:project-euler-0300)

(defmacro source-pathname ()
  "Compute source pathname"
  `(load-time-value ,(or *compile-file-truename* *load-truename* (uiop:getcwd))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :cffi)
  (ql:quickload :uiop))

;;; ----------------------------------------------------------------------
;;; Go コードのインライン生成とコンパイル (V2: キャッシュバイパス版)
;;; ----------------------------------------------------------------------
(let* ((base-dir (uiop:pathname-directory-pathname (source-pathname)))
       (go-file (merge-pathnames "solution300.go" base-dir))
       (lib-file (merge-pathnames "libeuler300.so" base-dir))) ; 拡張子は環境により .dylib でも可
  
  (with-open-file (out go-file :direction :output :if-exists :supersede)
    (write-string 
"package main

/*
#include <stdint.h>
*/
import \"C\"
import \"unsafe\"

type Pair struct { i, j int }
type Signature struct { lo, hi uint64 }

func getPairID(i, j int) int {
    id := 0
    for a := 0; a < i; a++ {
        id += 15 - 2 - a
    }
    id += j - (i + 2)
    return id
}

func getSig(pairs []Pair) Signature {
    var s Signature
    for _, p := range pairs {
        id := getPairID(p.i, p.j)
        if id < 64 {
            s.lo |= (1 << id)
        } else {
            s.hi |= (1 << (id - 64))
        }
    }
    return s
}

//export Solve300_v2
func Solve300_v2(n C.int, outPtr *C.int32_t) C.int {
    length := int(n)
    out := unsafe.Slice((*int32)(unsafe.Pointer(outPtr)), 1<<length)

    var visited [31][31]bool
    var path [15]struct{x, y int}
    var dx = []int{1, 0, -1, 0}
    var dy = []int{0, 1, 0, -1}
    
    uniqueTopos := make(map[Signature][]Pair)
    
    var dfs func(step int, x, y int)
    dfs = func(step int, x, y int) {
        path[step] = struct{x,y int}{x, y}
        visited[x][y] = true
        
        if step == length - 1 {
            var pairs []Pair
            for i := 0; i < length; i++ {
                for j := i + 3; j < length; j += 2 {
                    dx := path[i].x - path[j].x
                    dy := path[i].y - path[j].y
                    if dx*dx + dy*dy == 1 {
                        pairs = append(pairs, Pair{i, j})
                    }
                }
            }
            sig := getSig(pairs)
            if _, ok := uniqueTopos[sig]; !ok {
                uniqueTopos[sig] = pairs
            }
        } else {
            for i := 0; i < 4; i++ {
                nx, ny := x + dx[i], y + dy[i]
                if !visited[nx][ny] {
                    if step == 0 && i != 0 { continue }
                    if step == 1 && (i == 2 || i == 3) { continue }
                    dfs(step+1, nx, ny)
                }
            }
        }
        visited[x][y] = false
    }
    
    dfs(0, 15, 15)

    type TopoMask []uint16
    var topoMaskList []TopoMask
    for _, pairs := range uniqueTopos {
        var tm TopoMask
        for _, p := range pairs {
            tm = append(tm, uint16((1<<p.i) | (1<<p.j)))
        }
        topoMaskList = append(topoMaskList, tm)
    }

    // 全てのH/Pシーケンスに対して評価
    for seq := 0; seq < (1 << length); seq++ {
        maxScore := 0
        seq16 := uint16(seq)
        
        // 1. 空間的接触 (SAW由来)
        for _, tm := range topoMaskList {
            score := 0
            for _, mask := range tm {
                if (seq16 & mask) == mask {
                    score++
                }
            }
            if score > maxScore {
                maxScore = score
            }
        }
        
        // 2. 主鎖(Backbone)の接触 (シーケンス固有)
        backboneScore := 0
        for i := 0; i < length - 1; i++ {
            mask := uint16(3 << i)
            if (seq16 & mask) == mask {
                backboneScore++
            }
        }
        
        out[seq] = int32(maxScore + backboneScore)
    }
    return 0
}

func main() {}
" out))

  ;; unlessを外し、毎回強制的にビルドさせる
  (format t "Forcing build of Go shared library V2...~%")
  (uiop:run-program 
   (list "go" "build" "-buildmode=c-shared" "-o" (namestring lib-file) (namestring go-file)))
  (cffi:load-foreign-library lib-file))

;;; ----------------------------------------------------------------------
;;; CFFI バインディングと Lisp 実行関数
;;; ----------------------------------------------------------------------
;; 新しい関数名をバインド
(cffi:defcfun ("Solve300_v2" %solve-go-v2) :int
  (n :int)
  (out-ptr :pointer))

(defun exact-decimal-string (num den)
  (let* ((int-part (truncate num den))
         (rem (rem num den)))
    (if (zerop rem)
        (format nil "~D.0" int-part)
        (let ((dec-part ""))
          (loop while (not (zerop rem)) do
                (setf rem (* rem 10))
                (multiple-value-bind (q r) (truncate rem den)
                  (setf dec-part (concatenate 'string dec-part (write-to-string q)))
                  (setf rem r)))
          (format nil "~D.~A" int-part dec-part)))))

(defun solve ()
  (let* ((n 15)
         (seq-count (ash 1 n))
         (out-ptr (cffi:foreign-alloc :int32 :count seq-count))
         (sum 0))
    
    (format t "Allocated memory for ~D sequences. Starting Go simulation V2...~%" seq-count)
    
    (unwind-protect
         (progn
           (%solve-go-v2 n out-ptr)
           
           (loop for i from 0 below seq-count do
                 (incf sum (cffi:mem-aref out-ptr :int32 i))))
      
      (cffi:foreign-free out-ptr))
    
    (let ((result-string (exact-decimal-string sum seq-count)))
      (format t "Total max H-H contacts (Spatial + Backbone): ~D~%" sum)
      (format t "Average: ~A~%" result-string)
      result-string)))


#+| Do it | (SOLVE )
#|------------------------------------------------------------|
Timing the evaluation of (solve)
Allocated memory for 32768 sequences. Starting Go simulation V2...
Total max H-H contacts (Spatial + Backbone): 263916
Average: 8.0540771484375

User time    =        3.574
System time  =        0.047
Elapsed time =        3.567
Allocation   = 107320 bytes
492 Page faults
GC time      =        0.000
 |------------------------------------------------------------|#
;;→ "8.0540771484375"
:ok