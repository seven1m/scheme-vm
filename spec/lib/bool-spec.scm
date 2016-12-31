(import (scheme base)
        (assert))

(assert (eq? #t (boolean? #t)))
(assert (eq? #t (boolean? #f)))
(assert (eq? #f (boolean? (list))))
(assert (eq? #t #t))
(assert (eq? #t (not #f)))

(assert (boolean=? #t #t))
(assert (boolean=? #f #f))
(assert (not (boolean=? #t #f)))
(assert (not (boolean=? #f #t)))
(assert (not (boolean=? 1 #t)))
