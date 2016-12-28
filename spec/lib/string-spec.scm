(import (scheme base)
        (assert))

(assert (string? "foo"))
(assert (not (string? 1)))

(assert (eq? 0 (string-length "")))
(assert (eq? 1 (string-length "x")))
(assert (eq? 3 (string-length "foo")))

(assert (eqv? #\y (string-ref "xyz" 1)))

(assert (equal? '(#\f #\o #\o #\b #\a #\r #\b #\a #\z) (string->list "foobarbaz")))
