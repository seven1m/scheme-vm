(import (scheme base)
        (assert))

(assert (eq? #t (list? '())))
(assert (eq? #t (list? (list))))
(assert (eq? #t (list? '(1 2 3))))
(assert (eq? #t (list? (list 1 2 3))))
(assert (eq? #f (list? 1)))
(assert (eq? #f (list? "string")))

(assert (eq? #t (empty? (list))))
(assert (eq? #f (empty? (list 1 2 3))))

(assert (eq? 0 (length (list))))
(assert (eq? 3 (length (list 1 2 3))))

(assert (eq? 1 (last (list 1))))
(assert (eq? 3 (last (list 1 2 3))))

(assert (equal? (list 3 2 1) (reverse '(1 2 3))))

(define l1 '(1 2 3))
(define l2 l1)
(define l3 '(1 2 3))

(assert (eqv? l1 l2))
(assert (not (eqv? l1 l3)))
(assert (not (eqv? l2 l3)))
