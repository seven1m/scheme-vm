(define-library (range)
  (import (only (scheme base) define letrec >=))
  (export range)
  (begin
    (define (range first last)
      (letrec ((r (lambda (n1 n2 l)
                    (if (>= n1 n2)
                      l
                      (r n1 (- n2 1) (cons (- n2 1) l))))))
        (r first last '())))))
