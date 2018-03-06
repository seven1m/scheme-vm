(import (scheme base)
        (assert))

(assert (symbol? 'a))
(assert (symbol? (car '(a b))))
(assert (not (symbol? "a")))

(assert (equal? (symbol->string 'a) "a"))
(assert (not (equal? (symbol->string 'b) "a")))

(assert (symbol=?))
(assert (symbol=? 'a))
(assert (symbol=? 'a 'a 'a))
(assert (not (symbol=? 'a 'a 'b)))
(assert (not (symbol=? 'a "a")))
(assert (not (symbol=? 1 1)))
