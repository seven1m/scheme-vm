(define-library (fixtures library-test)
  (export foo)
  (begin
    (define (foo)
      12)
    (define (bar)
      13))
  (export bar (rename bar baz)))
