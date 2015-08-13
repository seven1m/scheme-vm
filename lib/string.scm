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
    (define s-a
      (lambda strings2
        (if (= 0 (length strings2))
            (list)
            (append (string->list (car strings2))
                    (apply string-append (cdr strings2))))))
    (s-a strings1)))
