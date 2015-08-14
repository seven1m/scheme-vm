(define string->list
  (lambda (str)
    (define s->l
      (lambda (s i)
        (if (>= i (string-length s))
            (list)
            (cons
              (string-ref str i)
              (s->l s (+ i 1))))))
    (s->l str 0)))

(define string-append
  (lambda strings1
    (define s->l
      (lambda strings2
        (if (= 0 (length strings2))
            (list)
            (append (string->list (car strings2))
                    (apply s->l (cdr strings2))))))
    (list->string (apply s->l strings1))))

(define print
  (lambda args
    (if (= 0 (length args))
        (write #\newline)
        (begin
          (write (car args))
          (apply print (cdr args))))))
