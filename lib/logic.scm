(define and
  (lambda conditions
    (if (= 0 (length conditions))
        #t
        (if (= 1 (length conditions))
            (car conditions)
            (if (car conditions)
                (apply and (cdr conditions))
                #f)))))

(define or
  (lambda conditions
    (if (= 0 (length conditions))
        #f
        (if (= 1 (length conditions))
            (car conditions)
            (if (car conditions)
                (car conditions)
                (apply or (cdr conditions)))))))

(define not
  (lambda (condition)
    (if condition
        #f
        #t)))
