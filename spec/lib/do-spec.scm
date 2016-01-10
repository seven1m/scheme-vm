(import (scheme base)
        (assert))

(define i 0)

(define result '())

(do ((i 0 (+ i 1)))
    ((= i 5) i)
  (set! result (cons i result)))

(assert (eqv? result '(4 3 2 1 0)))
