(import (scheme base)
        (assert))

(assert (equal?
          '(1 4 9)
          (map (lambda (n) (* n n)) '(1 2 3))))
