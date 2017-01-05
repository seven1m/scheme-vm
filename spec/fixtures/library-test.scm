(define-library (fixtures library-test)
  (import (only (scheme base) define define-syntax))
  (export foo macro)
  (begin
    (define (foo)
      12)
    (define (bar)
      13)
    (define-syntax macro
      (syntax-rules ()
        ((macro) 14))))
  (export bar (rename bar baz)))
