(include "assert")

(define fixed-args
  (lambda (x y z)
    (list x y z)))

(define variable-args
  (lambda args
    args))

(define rest-args
  (lambda (x y . rest)
    (list x y rest)))

(assert (eqv? (list 1 2 3)          (fixed-args 1 2 3)))
(assert (eqv? (list 1 2 3)          (variable-args 1 2 3)))
(assert (eqv? (list 1 2 (list 3 4)) (rest-args 1 2 3 4)))
