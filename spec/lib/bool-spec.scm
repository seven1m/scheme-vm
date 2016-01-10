(import (scheme base)
        (assert))

(assert (eq? #t (boolean? #t)))
(assert (eq? #t (boolean? #f)))
(assert (eq? #f (boolean? (list))))
(assert (eq? #t #t))
(assert (eq? #t (not #f)))
