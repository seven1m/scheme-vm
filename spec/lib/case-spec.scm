(import (scheme base)
        (assert))

(define (compare x)
  (case x
    ((1) "one")
    ((2) "two")
    (else "other")))

(assert (eq? "one" (compare 1)))
(assert (eq? "two" (compare 2)))
(assert (eq? "other" (compare 3)))
