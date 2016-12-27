(import (scheme base)
        (assert))

;; atoms
(assert (equal? 'a 'a))
(assert (not (equal? 'a 'b)))
(assert (eqv? 'a 'a))
(assert (not (eqv? 'a 'b)))
(assert (not (eqv? 'a "a")))
(assert (eq? 'a 'a))
(assert (not (eq? 'a 'b)))
(assert (not (eq? 'a "a")))

;; booleans
(assert (equal? #t #t))
(assert (equal? #f #f))
(assert (not (equal? #t #f)))
(assert (eqv? #t #t))
(assert (eqv? #f #f))
(assert (not (eqv? #t #f)))
(assert (eq? #t #t))
(assert (eq? #f #f))
(assert (not (eq? #t #f)))

;; characters
(assert (equal? #\a #\a))
(assert (not (equal? #\a #\b)))
(assert (not (equal? #\a "a")))
(assert (eqv? #\a #\a))
(assert (not (eqv? #\a #\b)))
(assert (not (eqv? #\a "a")))
(assert (eq? #\a #\a))
(assert (not (eq? #\a #\b)))
(assert (not (eq? #\a "a")))

;; lists
(assert (equal? (list 1 2) '(1 2)))
(assert (equal? (list "foo" "bar") '("foo" "bar")))
(assert (equal? (list) '()))
(assert (not (equal? (list "foo" "bar") '("foo" "baz"))))
(assert (not (equal? (list 1 2) '(1 2 3))))
(assert (not (equal? (list 1 2 3) '(1 2))))
(assert (not (equal? (list 1 2) '(2 1))))
(assert (not (equal? (list) '(1))))
(assert (not (eqv? '(1 2) '(1 2))))
(assert (not (eq? '(1 2) '(1 2))))
(let* ((x '(1 2)) (y x))
  (assert (eq? x y))
  (assert (eqv? x y)))

;; numbers
(assert (equal? 1 1))
(assert (not (equal? 1 2)))
(assert (eqv? 1 1))
(assert (not (eqv? 1 2)))
(assert (eq? 1 1))
(assert (not (eq? 1 2)))
(assert (= 1 1))
(assert (not (= 1 2)))

;; strings
(assert (equal? "foo" "foo"))
(assert (not (equal? "foo" "bar")))
(assert (not (eqv? "foo" "foo")))
(assert (not (eqv? "foo" "bar")))
(assert (not (eq? "foo" "foo")))
(assert (not (eq? "foo" "bar")))
(let* ((x "foo") (y x))
  (assert (eq? x y))
  (assert (eqv? x y)))

;;
(define l1 '(1 2 3))
(define l2 l1)
(define l3 '(1 2 3))

(assert (eqv? l1 l2))
(assert (not (eqv? l1 l3)))
(assert (not (eqv? l2 l3)))

(define p1 '(1))
(define p2 p1)
(define p3 '(1))

(assert (eqv? p1 p2))
(assert (not (eqv? p1 p3)))
(assert (not (eqv? p2 p3)))
