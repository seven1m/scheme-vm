(import (scheme base))

(define (fib n)
  (if (< n 2)
      n
      (+
        (fib (- n 1))
        (fib (- n 2)))))

(define (fib2 n)
  (letrec ((f (lambda (n1 n2 c)
                (if (= c n)
                  n2
                  (f n2 (+ n1 n2) (+ c 1))))))
    (f 0 1 1)))

(write-string (fib 8))
(newline)
(write-string (fib2 8))
(newline)
