(import (scheme base))

(define (fib n)
  (if (< n 2)
      n
      (+
        (fib (- n 1))
        (fib (- n 2)))))
(write-string (fib 8))
(newline)
