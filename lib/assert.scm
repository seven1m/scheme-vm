(define assert-eq
  (lambda (expected actual)
    (if (not (eq? expected actual))
        (begin
          (print "assert-eq failed:")
          (write "  expected: ")
          (print expected)
          (write "  actual:   ")
          (print actual)))))
