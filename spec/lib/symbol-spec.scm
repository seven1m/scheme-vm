(import (scheme base)
        (assert))

(assert (symbol? 'a))
(assert (symbol? (car '(a b))))
(assert (not (symbol? "a")))
