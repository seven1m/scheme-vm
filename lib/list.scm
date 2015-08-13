(define empty?
  (lambda (l)
    (null? l)))

(define length
  (lambda (l)
    (if (empty? l)
        0
        (+ 1 (length (cdr l))))))
