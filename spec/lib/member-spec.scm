(import (scheme base)
        (assert))

;; memq
(assert (equal? '(a b c) (memq 'a '(a b c))))
(assert (equal? '(b c) (memq 'b '(a b c))))
(assert (equal? #f (memq 'a '(b c d))))
(assert (equal? #f (memq (list 'a) '(b (a) c))))

;; memv
(assert (equal? '(101 102) (memv 101 '(100 101 102))))

;; TODO: member
