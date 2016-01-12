(import (scheme base)
        (assert))

;; atoms
(assert (equal? 'a 'a))

;; booleans
(assert (equal? #t #t))
(assert (equal? #f #f))
(assert (not (equal? #t #f)))

;; characters
(assert (equal? #\a #\a))
(assert (not (equal? #\a #\b)))

;; lists
(assert (equal? (list 1 2) '(1 2)))
(assert (equal? (list "foo" "bar") '("foo" "bar")))
(assert (equal? (list) '()))
(assert (not (equal? (list "foo" "bar") '("foo" "baz"))))
(assert (not (equal? (list 1 2) '(1 2 3))))
(assert (not (equal? (list 1 2 3) '(1 2))))
(assert (not (equal? (list 1 2) '(2 1))))
(assert (not (equal? (list) '(1))))

;; numbers
(assert (equal? 1 1))
(assert (not (equal? 1 2)))

;; strings
(assert (equal? "foo" "foo"))
(assert (not (equal? "foo" "bar")))
