(define pair?
  (lambda (l)
    (if (empty? l)
        #f
        (if (car l)
            #t
            #f))))
