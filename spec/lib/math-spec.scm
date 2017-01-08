(import (scheme base)
        (assert))

(assert (= 0 (+)))
(assert (= 4 (+ 4)))
(assert (= 7 (+ 4 3)))
(assert (= 10 (+ 4 3 2 1)))

; (-) is an error
(assert (= -4 (- 4)))
(assert (= 1 (- 4 3)))
(assert (= -2 (- 4 3 2 1)))

(assert (= 1 (*)))
(assert (= 4 (* 4)))
(assert (= 12 (* 4 3)))
(assert (= 24 (* 4 3 2 1)))

; (/) is an error
(assert (= 1 (/ 1)))
(assert (= 2 (/ 4 2)))
(assert (= 2 (/ 4 2 1)))
(assert (= 2 (/ 5 2))) ; no support for inexact numbers

(assert (= 2 (modulo 8 3)))

(assert (equal? #t (even? 2)))
(assert (equal? #f (even? 3)))

(assert (equal? #f (odd? 2)))
(assert (equal? #t (odd? 3)))

(assert (= 3 (max 1 3 2)))
(assert (= 1 (min 1 3 2)))
