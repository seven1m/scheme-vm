(import (scheme base)
        (assert))

(assert (equal?
          '(1 4 9)
          (map (lambda (n) (* n n)) '(1 2 3))))

(assert (equal?
          '((1 4) (2 5) (3 6))
          (map (lambda (x y) (list x y)) '(1 2 3) '(4 5 6))))

(assert (equal?
          '((1 4) (2 5) (3 6))
          (map (lambda (x y) (list x y)) '(1 2 3 7) '(4 5 6))))
