(define and
  (lambda conditions
    (if (eq 0 (length conditions))
        #t
        (if (eq 1 (length conditions))
            (car conditions)
            (if (car conditions)
                (apply and (cdr conditions))
                #f)))))

(define not
  (lambda (condition)
    (if condition
        #f
        #t)))
