(import (only (scheme base) define-syntax))

(define-syntax bad-macro
  (syntax-rules ()
    ((bad-macro) (foo))))

