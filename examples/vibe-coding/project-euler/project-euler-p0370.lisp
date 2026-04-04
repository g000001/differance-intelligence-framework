;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3.1-pro
(cl:in-package cl-user)
(defpackage #:project-euler-0370 (:use cl) (:export #:solve))
(in-package #:project-euler-0370)

(defmacro source-pathname ()
  "Compute source pathname"
  `(load-time-value ,(or *compile-file-truename* *load-truename* (uiop:getcwd))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :cffi)
  (ql:quickload :uiop))

(setf (uiop:getenv "PATH")
      "/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin")

;;; ----------------------------------------------------------------------
;;; Rustコードのインライン生成とコンパイル (純粋なベアメタル・アーキテクチャ)
;;; ----------------------------------------------------------------------
(let* ((base-dir (uiop:pathname-directory-pathname (source-pathname)))
       (rs-file (merge-pathnames "euler370.rs" base-dir))
       (lib-name #+darwin "libeuler370.dylib"
                 #+linux "libeuler370.so"
                 #+windows "euler370.dll"
                 #-(or darwin linux windows) "libeuler370.so")
       (lib-file (merge-pathnames lib-name base-dir)))
  
  (format t "Generating Rust source code...~%")
  (with-open-file (out rs-file :direction :output :if-exists :supersede)
    (write-string 
"#![allow(non_snake_case)]

fn gcd(mut a: i64, mut b: i64) -> i64 {
    while b != 0 {
        let r = a % b;
        a = b;
        b = r;
    }
    a
}

#[inline]
fn get_v_phi(u: i64) -> i64 {
    let mut v = (u as f64 * 1.6180339887498948) as i64;
    // 浮動小数点の誤差を排除し、厳密な境界 v^2 - uv - u^2 < 0 を満たす最大の v を求める
    while v * v - u * v - u * u >= 0 { v -= 1; }
    while (v + 1) * (v + 1) - u * (v + 1) - u * u < 0 { v += 1; }
    v
}

#[inline]
fn get_v_max(u: i64, r: i64) -> i64 {
    let mut v = (((-(u as f64) + ((4.0 * r as f64 - 3.0 * (u * u) as f64).sqrt())) / 2.0)) as i64;
    // 厳密な境界 u^2 + uv + v^2 <= R を満たす最大の v を求める
    while u * u + u * (v + 1) + (v + 1) * (v + 1) <= r { v += 1; }
    while u * u + u * v + v * v > r { v -= 1; }
    v
}

// 領域 u^2+uv+v^2 <= r, u <= v < u*phi 内の全格子点数を求める
#[inline]
fn V(r: i64) -> i64 {
    if r < 3 { return 0; }
    let u_max = ((r as f64 / 3.0).sqrt()) as i64;
    let mut count = 0;
    for u in 1..=u_max {
        let v_phi = get_v_phi(u);
        let v_max = get_v_max(u, r);
        let v_limit = v_phi.min(v_max);
        if v_limit >= u {
            count += v_limit - u + 1;
        }
    }
    count
}

// 互いに素なペア (u,v) についての領域内格子点数 (メビウス反転を適用)
fn C(x: i64, mu: &[i8]) -> i64 {
    if x < 3 { return 0; }
    let d_max = ((x as f64 / 3.0).sqrt()) as i64;
    let mut count = 0;
    for d in 1..=d_max {
        let m = mu[d as usize];
        if m != 0 { // mu(d) = 0 の枝刈りによって演算回数を約40%削減
            count += (m as i64) * V(x / (d * d));
        }
    }
    count
}

#[no_mangle]
pub extern \"C\" fn solve_370(n: i64, out: *mut i64) {
    // ディリクレの双曲線法 (Dirichlet Hyperbola Method) の最適分割点 K
    let k_limit = 30000;
    let m_limit = n / k_limit;
    
    // エラトステネスの篩によるメビウス関数 mu(d) の事前計算
    let d_max_size = ((n as f64 / 3.0).sqrt()) as usize + 2;
    let mut mu = vec![1i8; d_max_size];
    let mut p_div = vec![false; d_max_size];
    for i in 2..d_max_size {
        if !p_div[i] {
            for j in (i..d_max_size).step_by(i) {
                p_div[j] = true;
                mu[j] = -mu[j];
            }
            let i2 = i as i64 * i as i64;
            if i2 < d_max_size as i64 {
                for j in (i2..d_max_size as i64).step_by(i2 as usize) {
                    mu[j as usize] = 0;
                }
            }
        }
    }
    
    // 双曲線法 第1項: K より大きな M への寄与
    let mut sum1 = 0;
    for k in 1..=k_limit {
        sum1 += C(n / k, &mu);
    }
    
    // 双曲線法 第2項: 小さな M (M <= n/K) の直接列挙
    let mut sum2 = 0;
    let u_max = ((m_limit as f64 / 3.0).sqrt()) as i64;
    for u in 1..=u_max {
        let v_phi = get_v_phi(u);
        let v_max = get_v_max(u, m_limit);
        let v_limit = v_phi.min(v_max);
        for v in u..=v_limit {
            if gcd(u, v) == 1 {
                let m = u * u + u * v + v * v;
                sum2 += n / m;
            }
        }
    }
    
    // 双曲線法 第3項: 重複部分の除去
    let sum3 = k_limit * C(m_limit, &mu);
    
    unsafe {
        *out = sum1 + sum2 - sum3;
    }
}
" out))

  (format t "Compiling Rust code into highly optimized shared library...~%")
  ;; Rustコンパイラを最大最適化(-C opt-level=3)で呼び出し
  (uiop:run-program 
   (list "rustc" "-C" "opt-level=3" "--crate-type" "cdylib"
         (namestring rs-file) "-o" (namestring lib-file)))
  
  (format t "Loading foreign library via CFFI...~%")
  (handler-case (cffi:close-foreign-library 'libeuler370) (error () nil))
  (cffi:define-foreign-library libeuler370
    (:darwin (:default "libeuler370"))
    (:unix (:default "libeuler370"))
    (:windows "euler370.dll"))
  (cffi:load-foreign-library lib-file))

;;; ----------------------------------------------------------------------
;;; CFFI バインディングと Lisp 実行関数
;;; ----------------------------------------------------------------------
(cffi:defcfun ("solve_370" %solve-370) :void
  (n :int64)
  (out :pointer))

(defun solve ()
  "Find the number of geometric triangles with perimeter <= 2.5 * 10^13."
  (let ((n 25000000000000) ; 2.5 * 10^13
        ;; Lisp側で共有メモリを確保
        (out-ptr (cffi:foreign-alloc :int64)))
    
    (format t "Executing highly optimized Dirichlet Hyperbola method in Rust...~%")
    (unwind-protect
         (progn
           (time (%solve-370 n out-ptr))
           (let ((ans (cffi:mem-ref out-ptr :int64)))
             (format t "Geometric triangles with perimeter <= ~D = ~D~%" n ans)
             ans))
      
      (cffi:foreign-free out-ptr))))


#+| Do it | (solve )
#|------------------------------------------------------------|
Timing the evaluation of (solve)
Executing highly optimized Dirichlet Hyperbola method in Rust...
Timing the evaluation of (%solve-370 n out-ptr)

User time    =  0:03:50.283
System time  =        4.501
Elapsed time =  0:04:22.332
Allocation   = 4051520 bytes
6824 Page faults
GC time      =        0.000
Geometric triangles with perimeter <= 25000000000000 = 41791929448408

User time    =  0:03:50.284
System time  =        4.501
Elapsed time =  0:04:22.333
Allocation   = 4055224 bytes
6866 Page faults
GC time      =        0.000
 |------------------------------------------------------------|#
;;→ 41791929448408
:ok