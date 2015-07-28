(define boolean?
  (lambda (b)
    (if (eq? b #t)
        #t
        (if (eq? b #f)
            #t
            #f))))
