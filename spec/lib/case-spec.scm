(import (scheme base)
        (assert))

(define (compare x)
  (case x
    ((1) "one")
    ((2) "two")
    (else "other")))

(assert (equal? "one" (compare 1)))
(assert (equal? "two" (compare 2)))
(assert (equal? "other" (compare 3)))
