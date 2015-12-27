(define empty?
  (lambda (l)
    (null? l)))

(define length
  (lambda (l)
    (if (empty? l)
        0
        (+ 1 (length (cdr l))))))

(define last
  (lambda (l)
    (if (empty? l)
        (list)
        (if (= 1 (length l))
            (car l)
            (last (cdr l))))))
