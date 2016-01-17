(import (scheme base)
        (assert))

(assert (equal? 7 (+ 4 3)))
(assert (equal? 1 (- 4 3)))
(assert (equal? 12 (* 4 3)))
(assert (equal? 2 (/ 4 2)))
(assert (equal? 2 (/ 5 2))) ; no support for inexact numbers

(assert (equal? 2 (modulo 8 3)))

(assert (equal? #t (even? 2)))
(assert (equal? #f (even? 3)))

(assert (equal? #f (odd? 2)))
(assert (equal? #t (odd? 3)))
