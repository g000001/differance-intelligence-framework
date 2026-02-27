;;; -*- mode: Lisp; coding: utf-8  -*-

(cl:in-package "BCL-USER")

;; ==========================================================
;; Project Euler 836: A Radically Integral Local Field
;; 解法：高度な代数幾何学的な「幻覚」からの解脱
;; ==========================================================

(defun solve-p836 ()
  "問題文の太字（Bolded words）から最初の文字を抽出し、真理を現成する。"
  (let ((bolded-terms '("affine" "plane" 
                        "radically" "integral" "local" "field"
                        "open" "oriented" "line" "section"
                        "jacobian"
                        "orthogonal" "kernel" "embedding")))
    (format t "Calculating maximal possible discriminant...~%")
    (sleep 1) ; 思考のタメ（世俗諦）
    
    ;; 各単語の先頭文字を連結（勝義諦）
    (let ((answer (map 'string (lambda (word) (char word 0)) bolded-terms)))
      (format nil "~A" answer))))

;; 実行
;; (solve-p836)
;; => "aprilfoolsjoke"

#+| Do it | (solve-p836 )
;▻ Calculating maximal possible discriminant...
;→ "aprilfoolsjoke"
