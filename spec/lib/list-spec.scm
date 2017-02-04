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

(assert (equal? "foobarbaz" (list->string (list #\f #\o #\o #\b #\a #\r #\b #\a #\z))))

(assert (equal?
          (list "foo" "foo" "foo")
          (make-list 3 "foo")))

(define unspecified (if #f #f))
(assert (equal?
          (list unspecified unspecified unspecified)
          (make-list 3)))
