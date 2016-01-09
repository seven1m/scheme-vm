(import (scheme base))

(include "assert")

(assert (string? "foo"))
(assert (not (string? 1)))

(assert (eq? 0 (string-length "")))
(assert (eq? 1 (string-length "x")))
(assert (eq? 3 (string-length "foo")))

(assert (eqv? #\y (string-ref "xyz" 1)))

; later
;(assert (equal? "foo" (list->string (list #\f #\o #\o))))
