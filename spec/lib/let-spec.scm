(import (scheme base)
        (assert))

(define (foo)
  "old-foo")

(let-syntax
    ((foo (syntax-rules ()
            ((foo) "foo")))
     (bar (syntax-rules ()
            ((bar) "bar"))))
  (assert (eq? "foobar" (string-append (foo) (bar)))))

(letrec-syntax
    ((foo (syntax-rules ()
            ((foo) (bar))))
     (bar (syntax-rules ()
            ((bar) "bar"))))
  (assert (eq? "barbar" (string-append (foo) (bar)))))

(assert (eq? "old-foo" (foo)))

(letrec ((even?
          (lambda (n)
            (if (= n 0)
                #t
                (odd? (- n 1)))))
         (odd?
          (lambda (n)
            (if (= n 0)
                #f
                (even? (- n 1))))))
  (assert (even? 8))
  (assert (even? 2))
  (assert (not (even? 3)))
  (assert (odd? 9))
  (assert (not (odd? 8))))
