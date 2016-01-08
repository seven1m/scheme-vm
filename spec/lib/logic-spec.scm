(import (scheme base))

(include "assert")

(assert (eq? #t (and)))
(assert (eq? #t (and #t)))
(assert (eq? 1 (and 1)))
(assert (eq? 2 (and 1 2)))
(assert (eq? #f (and #f 2)))

(assert (eq? #f (or)))
(assert (eq? #t (or #t)))
(assert (eq? 1 (or 1)))
(assert (eq? 1 (or 1 2)))
(assert (eq? 2 (or #f 2)))
