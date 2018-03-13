(import (scheme base)
        (assert))

(define (fn1 x) x)
(define fn2 fn1)

(define-library (define-in-lib-test)
  (import (only (scheme base) define))
  (export fn4)
  (begin
    (define (fn3 x) x)
    (define fn4 fn3)))

(import (define-in-lib-test))

(assert (= 'a (fn1 'a)))
(assert (= 'a (fn2 'a)))
(assert (= 'a (fn4 'a)))
