(define-syntax bad-macro
  (syntax-rules ()
    ((bad-macro) (foo))))

