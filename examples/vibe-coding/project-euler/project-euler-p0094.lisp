
(defun solve-problem ()
  (let ((max-perimeter 1000000000)
        (total-perimeter-sum 0))

    ;; Case 1: Sides are a, a, a-1
    ;; X = 3a+1, Y = k. Equation: X^2 - 3Y^2 = 4.
    ;; We need X = 3a+1, so X must be congruent to 1 mod 3.
    ;; Perimeters P = 3a - 1 = X - 2.
    ;; Triangle inequality (non-degenerate): a > 1, so a >= 2.
    ;; This implies X = 3a+1 >= 7.
    ;; The Pell solutions (X_n, Y_n) are generated from (4,2).
    ;; X_n mod 3 pattern is 1, 2, 1, 2, ...
    ;; So we need odd n for X_n mod 3 = 1.
    ;; We start from X_3 (n=3) to satisfy X >= 7.

    (let ((x 4) (y 2) (n 1))
      (loop
        (let ((next-x (+ (* 2 x) (* 3 y)))
              (next-y (+ x (* 2 y))))
          (setf x next-x
                y next-y
                n (1+ n)))
        ;; Check if n is odd and X is large enough (X >= 7 for a >= 2)
        (when (and (oddp n) (>= x 7))
          (let ((perimeter (- x 2)))
            (when (> perimeter max-perimeter)
              (return))
            (incf total-perimeter-sum perimeter)))))

    ;; Case 2: Sides are a, a, a+1
    ;; X = 3a-1, Y = k. Equation: X^2 - 3Y^2 = 4.
    ;; We need X = 3a-1, so X must be congruent to 2 mod 3.
    ;; Perimeters P = 3a + 1 = X + 2.
    ;; Triangle inequality (non-degenerate): a > 1, so a >= 2.
    ;; This implies X = 3a-1 >= 5.
    ;; X_n mod 3 pattern is 1, 2, 1, 2, ...
    ;; So we need even n for X_n mod 3 = 2.
    ;; We start from X_2 (n=2) to satisfy X >= 5.

    (let ((x 4) (y 2) (n 1))
      (loop
        (let ((next-x (+ (* 2 x) (* 3 y)))
              (next-y (+ x (* 2 y))))
          (setf x next-x
                y next-y
                n (1+ n)))
        ;; Check if n is even and X is large enough (X >= 5 for a >= 2)
        (when (and (evenp n) (>= x 5))
          (let ((perimeter (+ x 2)))
            (when (> perimeter max-perimeter)
              (return))
            (incf total-perimeter-sum perimeter)))))
    
    total-perimeter-sum))

;(print (solve-problem))

#+| Do it | (solve-problem )
;→ 518408346
