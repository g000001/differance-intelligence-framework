;;; コルに従い、与えられた問題に対するCommon Lispコードを現成します。

;;; このCommon Lispコードは、与えられた`sets.txt`ファイルから各集合を読み込み、それぞれの集合が「特殊和集合（special sum set）」の条件を満たすかどうかを判定します。特殊和集合であると判定された場合、その集合の要素の合計を最終的な結果に加算します。

;;; 特殊和集合の条件は以下の通りです。
;;; 1. 任意の空でない互いに素な部分集合 `B` と `C` について、$S(B) \ne S(C)$ であること。
;;; 2. 任意の空でない互いに素な部分集合 `B` と `C` について、もし `B` が `C` よりも多くの要素を含むならば、$S(B) \gt S(C)$ であること。

;;; コードの核心は、再帰関数 `recurse` を用いて、各集合の要素を「部分集合 `B` に入れる」「部分集合 `C` に入れる」「どちらにも入れない（残りの要素）」の3通りの選択肢に分配し、すべての可能な空でない互いに素な部分集合のペア `(B, C)` を効率的にチェックすることです。

;;; ```commonlisp

(defpackage "c9b6d321-884e-564f-b54d-72fa55938308" (:use "CL"))

(in-package "c9b6d321-884e-564f-b54d-72fa55938308")

;;; Project Euler Problem 105: Special sum sets

;;; Helper function to split a string by a delimiter character.
(defun split-string (string delimiter)
  (loop for i = 0 then (1+ j)
        for j = (position delimiter string :start i)
        collect (subseq string i (or j (length string)))
        while j))

;;; Function to read sets of integers from a file.
;;; Each line in the file is a comma-separated list of integers representing a set.
(defun read-sets-from-file (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil nil)
          while line
          collect (mapcar #'parse-integer (split-string line #\,)))))

;;; Function to check if a given set is a special sum set.
;;; It uses a recursive approach to iterate through all possible ways
;;; to partition the elements of the set into three categories:
;;; 1. In subset B
;;; 2. In subset C
;;; 3. In neither B nor C (i.e., in the 'rest' of the set)
;;; This ensures that B and C are always disjoint.
(defun check-special-sum-set (set)
  (let* ((n (length set))
         ;; Convert the set to a sorted array for O(1) element access during recursion.
         ;; Sorting is not strictly necessary for correctness but can be useful for
         ;; consistent behavior or potential future optimizations.
         (elements (make-array n :initial-contents (sort (copy-list set) #'<))))
    
    (labels ((recurse (index sum-b count-b sum-c count-c)
               ;; Base case: All elements from the original set have been processed.
               (if (= index n)
                   ;; Check the special sum set conditions only if both B and C are non-empty.
                   (if (and (> count-b 0) (> count-c 0))
                       (cond
                        ;; Condition 1: S(B) != S(C)
                        ;; If sums are equal, it's not a special sum set.
                        ((= sum-b sum-c) (return-from check-special-sum-set nil))
                        ;; Condition 2: If |B| > |C| then S(B) > S(C)
                        ;; If |B| > |C| but S(B) <= S(C), it's not a special sum set.
                        ((and (> count-b count-c) (<= sum-b sum-c)) (return-from check-special-sum-set nil))
                        ;; If neither condition is violated for this specific (B, C) pair,
                        ;; return T to indicate it's valid so far.
                        (t t))
                       ;; If B or C (or both) are empty, this particular partition
                       ;; does not violate the rules, so we proceed to check other partitions.
                       t)
                   
                   ;; Recursive step: For the current element at 'index',
                   ;; explore all three possibilities:
                   (let ((element (aref elements index)))
                     ;; 1. Put the element into subset B
                     (recurse (1+ index) (+ sum-b element) (1+ count-b) sum-c count-c)
                     ;; 2. Put the element into subset C
                     (recurse (1+ index) sum-b count-b (+ sum-c element) (1+ count-c))
                     ;; 3. Put the element into neither B nor C (it remains in 'rest')
                     (recurse (1+ index) sum-b count-b sum-c count-c)))))
      
      ;; Start the recursion from the first element (index 0)
      ;; with initial sums and counts for B and C both set to 0.
      (recurse 0 0 0 0 0)
      ;; If the recursion completes without any 'return-from check-special-sum-set nil'
      ;; being triggered, it means all partitions satisfy the conditions,
      ;; so the set is a special sum set.
      t)))

;;; Main function to solve Project Euler Problem 105.
;;; It reads sets from 'sets.txt', identifies special sum sets,
;;; and calculates the total sum of their elements.
(defun solve-project-euler-105 (filename)
  (let ((total-sum-of-special-sets 0))
    (loop for set in (read-sets-from-file filename)
          do (when (check-special-sum-set set)
               (incf total-sum-of-special-sets (reduce #'+ set))))
    total-sum-of-special-sets))

;;; To run this code:
;;; 1. Save the code above as a .lisp file (e.g., problem105.lisp).
;;; 2. Download 'sets.txt' from the Project Euler website
;;;    (resources/documents/0105_sets.txt) and place it in the same
;;;    directory as your Lisp process, or provide its full path.
;;; 3. Load the file into your Common Lisp environment:
;;;    (load "problem105.lisp")
;;; 4. Call the main function:
;;;    (solve-project-euler-105 "sets.txt")
;;;    The result will be printed to the console.


#+| Do it | (solve-project-euler-105 "/tmp/sets.txt")
;→ 73702
