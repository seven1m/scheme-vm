(import (scheme base)
        (assert))

(assert (string? "foo"))
(assert (not (string? 1)))

(assert (eq? 0 (string-length "")))
(assert (eq? 1 (string-length "x")))
(assert (eq? 3 (string-length "foo")))

(assert (eqv? #\y (string-ref "xyz" 1)))

(assert (equal? '(#\f #\o #\o #\b #\a #\r #\b #\a #\z) (string->list "foobarbaz")))

(assert (string=? "foo" "foo"))
(assert (not (string=? "foo" "bar")))
(assert (not (string=? "foo" 1)))

(assert (equal? "" (string)))
(assert (equal? "a" (string #\a)))
(assert (equal? "foo" (string #\f #\o #\o)))

(assert (equal? 123 (string->number "123")))
(assert (equal? 0 (string->number "0")))
(assert (equal? -123 (string->number "-123")))
