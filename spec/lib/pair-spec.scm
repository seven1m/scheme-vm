(import (scheme base)
        (assert))

(assert (not (pair? 1)))
(assert (not (pair? "string")))
(assert (not (pair? '())))
(assert (not (pair? (list))))
(assert (pair? '(1)))

(assert (= 1 (car '(1 . 2))))
(assert (= 1 (car '(1 2))))

(assert (= 2 (cdr '(1 . 2))))
(assert (equal? '(2) (cdr '(1 2))))

(define nested-list '((1 2) (3 4) (5 6)))

(assert (equal?        1 (caar nested-list)))
(assert (equal?   '(3 4) (cadr nested-list)))
(assert (equal?     '(2) (cdar nested-list)))
(assert (equal? '((5 6)) (cddr nested-list)))

(define p1 '(1))
(define p2 p1)
(define p3 '(1))

(assert (eqv? p1 p2))
(assert (not (eqv? p1 p3)))
(assert (not (eqv? p2 p3)))
