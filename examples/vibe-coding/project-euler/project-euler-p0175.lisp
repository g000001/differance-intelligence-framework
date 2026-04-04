;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3-flash-preview
(cl:in-package cl-user)
(defpackage #:project-euler-0175 (:use cl) (:export #:solve))
(in-package #:project-euler-0175)

(defmacro source-pathname ()
  "Compute source pathname"
  `(load-time-value ,(or *compile-file-truename* *load-truename* (uiop:getcwd))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :cffi)
  (ql:quickload :uiop)
  (require "java-interface"))

(defvar *jvm-library-path*
  "/Library/Java/JavaVirtualMachines/jdk-20.jdk/Contents/Home/lib/server/libjvm.dylib")

(lw-ji:define-java-caller %euler-175-solve "Euler175" "solve")

;;; ----------------------------------------------------------------------
;;; Lisp 実行関数
;;; ----------------------------------------------------------------------
(defun solve ()
  "Find the Shortened Binary Expansion of the smallest n for which f(n)/f(n-1) = 123456789/987654321."
  
  (let* ((p 123456789)
         (q 987654321)
         (dir (uiop:pathname-directory-pathname (source-pathname)))
         (java-file (merge-pathnames "Euler175.java" dir)))
    
    (format t "Generating Java source code...~%")
    (with-open-file (out java-file :direction :output :if-exists :supersede)
      (write-string "
public class Euler175 {
    public static int solve(long p, long q, int[] out) {
        int idx = 0;
        // Stern-Brocot (Calkin-Wilf) 木の逆行をユークリッド互除法で高速化
        while (p > 0 && q > 0) {
            if (p > q) {
                long k = p / q;
                p = p % q;
                if (p == 0) {
                    out[idx++] = (int)(k - 1);
                    out[idx++] = 1;
                    break;
                } else {
                    out[idx++] = (int)k;
                }
            } else if (q > p) {
                long k = q / p;
                q = q % p;
                if (q == 0) {
                    out[idx++] = (int)k;
                    break;
                } else {
                    out[idx++] = (int)k;
                }
            } else {
                out[idx++] = 1;
                break;
            }
        }
        return idx;
    }
}
" out))
    
    (format t "Compiling Java code...~%")
    (uiop:run-program (list "javac" (namestring java-file)))
    
    (lw-ji:init-java-interface :jvm-library-path *jvm-library-path*)
    
    (progn
      (format t "Initializing JVM...~%")
      ;; 2. JVM初期化時にカレントディレクトリをクラスパスに設定
      ;; ※注意: REPLの同一セッションで既にJVMが起動済みの場合はクラスパスが更新されません。
      ;; ClassNotFoundException が出た場合は、一度LispWorksを再起動してください。
      
      (format t "Allocating shared memory in Common Lisp...~%")
      ;; 3. 共有メモリの確保: Lisp側でJavaのint配列をアロケート
      ;; (jobject の配列として安全にヒープ外管理されます)
      (let ((out-array (lw-ji:make-java-array :int 100)))
        
        (format t "Executing Euclidean steps in Java via define-java-caller...~%")
        ;; 4. リフレクションを使わず、定義したマクロ関数経由でネイティブライクに実行
        (let ((len (%euler-175-solve p q out-array))
              (result-list nil))
          
          ;; 5. Lisp側で共有メモリから結果を読み出す
          (dotimes (i len)
            (push (lw-ji:jvref out-array i) result-list))
          
          (let ((ans-str (format nil "~{~A~^,~}" result-list)))
            (format t "Shortened Binary Expansion: ~A~%" ans-str)
            ans-str))))))


#+| Do it | (solve )
#|
Timing the evaluation of (project-euler-0175:solve)
Generating Java source code...
Compiling Java code...
Initializing JVM...
Allocating shared memory in Common Lisp...
Executing Euclidean steps in Java via define-java-caller...
Shortened Binary Expansion: 1,13717420,8

User time    =        0.026
System time  =        0.014
Elapsed time =        0.429
Allocation   = 112520 bytes
8943 Page faults
GC time      =        0.000
;;→"1,13717420,8"
|#
:ok
