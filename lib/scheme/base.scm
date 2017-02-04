(define-library (scheme base)
  (export
   +
   -
   *
   /
   >
   >=
   <
   <=
   =
   abs
   and
   append
   apply
   begin
   boolean?
   boolean=?
   call-with-current-continuation
   call/cc
   car
   caar cadr cdar cddr
   caaar caadr cadar caddr cdaar cdadr cddar cdddr
   caaaar caaadr caadar caaddr cadaar cadadr caddar cadddr cdaaar cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr
   case
   cdr
   char?
   char=?
   char<?
   char<=?
   char>?
   char>=?
   char->integer
   char-downcase
   char-upcase
   cond
   cons
   define
   define-syntax
   do
   empty?
   eq?
   equal?
   eqv?
   even?
   if
   integer?
   integer->char
   lambda
   last
   length
   let
   let*
   let-syntax
   letrec-syntax
   letrec
   letrec*
   list
   list?
   list->string
   make-list
   map
   max
   memq
   memv
   min
   modulo
   negative?
   newline
   not
   null?
   number?
   number->string
   odd?
   or
   pair?
   positive?
   quasiquote
   quote
   reverse
   set!
   set-car!
   set-cdr!
   string
   string=?
   string?
   string-length
   string-ref
   string->list
   string->number
   string-append
   symbol?
   unless
   when
   write-string
   zero?)

  (begin
    (--define-native + base_+)
    (--define-native - base_-)
    (--define-native * base_*)
    (--define-native / base_/)
    (--define-native > base_>)
    (--define-native < base_<)
    (--define-native append base_append)
    (--define-native apply base_apply)
    (--define-native call-with-current-continuation base_call_cc)
    (--define-native call/cc base_call_cc)
    (--define-native car base_car)
    (--define-native cdr base_cdr)
    (--define-native char? base_char?)
    (--define-native char->integer base_char_to_integer)
    (--define-native cons base_cons)
    (--define-native define base_define)
    (--define-native define-syntax base_define_syntax)
    (--define-native empty? base_null?)
    (--define-native eq? base_eq?)
    (--define-native if base_if)
    (--define-native integer? base_integer?)
    (--define-native integer->char base_integer_to_char)
    (--define-native lambda base_lambda)
    (--define-native let-syntax base_let_syntax)
    (--define-native letrec-syntax base_letrec_syntax)
    (--define-native list base_list)
    (--define-native list->string base_list_to_string)
    (--define-native modulo base_modulo)
    (--define-native null? base_null?)
    (--define-native pair? base_pair?)
    (--define-native quasiquote base_quasiquote)
    (--define-native quote base_quote)
    (--define-native set! base_set!)
    (--define-native set-car! base_set_car!)
    (--define-native set-cdr! base_set_cdr!)
    (--define-native string base_string)
    (--define-native string? base_string?)
    (--define-native string-length base_string_length)
    (--define-native string-ref base_string_ref)
    (--define-native symbol? base_symbol?)

    (define-syntax begin
      (syntax-rules ()
        ((begin exp ...)
        ((lambda () exp ...)))))

    (define-syntax let
      (syntax-rules ()
        ((let ((name val) ...) body1 body2 ...)
        ((lambda (name ...) body1 body2 ...)
          val ...))
        ((let tag ((name val) ...) body1 body2 ...)
        ((letrec ((tag (lambda (name ...)
                          body1 body2 ...)))
            tag)
          val ...))))

    (define-syntax let*
      (syntax-rules ()
        ((let* () body1 body2 ...)
         (let () body1 body2 ...))
        ((let* ((name1 val1) (name2 val2) ...)
           body1 body2 ...)
        (let ((name1 val1))
          (let* ((name2 val2) ...)
            body1 body2 ...)))))

    (define-syntax letrec
      (syntax-rules ()
        ((letrec ((var1 init1) ...) body ...)
        (letrec "generate temp names"
          (var1 ...)
          ()
          ((var1 init1) ...)
          body ...))
        ((letrec "generate temp names"
          ()
          (temp1 ...)
          ((var1 init1) ...)
          body ...)
        (let ((var1 (if #f #f)) ...)
          (let ((temp1 init1) ...)
            (set! var1 temp1)
            ...
            body ...)))
        ((letrec "generate temp names"
          (x y ...)
          (temp ...)
          ((var1 init1) ...)
          body ...)
        (letrec "generate temp names"
          (y ...)
          (newtemp temp ...)
          ((var1 init1) ...)
          body ...))))

    (define-syntax letrec*
      (syntax-rules ()
        ((letrec* ((var1 init1) ...) body1 body2 ...)
        (let ((var1 (if #f #f)) ...)
          (set! var1 init1)
          ...
          (let () body1 body2 ...)))))

    (define-syntax do
      (syntax-rules ()
        ((do ((var init step ...) ...)
            (test expr ...)
          command ...)
        (letrec
            ((loop
              (lambda (var ...)
                (if test
                    (begin
                      (if #f #f)
                      expr ...)
                    (begin
                      command
                      ...
                      (loop (do "step" var step ...)
                            ...))))))
          (loop init ...)))
        ((do "step" x)
        x)
        ((do "step" x y)
        y)))

    (define-syntax and
      (syntax-rules ()
        ((and) #t)
        ((and test) test)
        ((and test1 test2 ...)
        (if test1 (and test2 ...) #f))))

    (define-syntax or
      (syntax-rules ()
        ((or) #f)
        ((or test) test)
        ((or test1 test2 ...)
          (let ((x test1))
            (if x x (or test2 ...))))))

    (define not
      (lambda (condition)
        (if condition
            #f
            #t)))

    (define-syntax =
      (syntax-rules ()
        ((=) #t)
        ((= n) #t)
        ((= n1 n2) (eq? n1 n2))
        ((= n1 n2 n3 ...)
         (and (eq? n1 n2)
              (= n2 n3 ...)))))


    (define (>= a b)
      (or (= a b) (> a b)))

    (define (<= a b)
      (or (= a b) (< a b)))

    (define (abs n)
      (if (< n 0)
        (* n -1)
        n))

    (define (eqv? a b)
      (eq? a b))

    (define (char-downcase c)
      (if (char? c)
        (let ((i (char->integer c)))
          (if (and (>= i 65) (<= i 90))
            (integer->char (+ i 32))
            c))))

    (define (char-upcase c)
      (if (char? c)
        (let ((i (char->integer c)))
          (if (and (>= i 97) (<= i 122))
            (integer->char (- i 32))
            c))))

    (define (memq obj list)
      (if (empty? list)
          #f
          (if (eq? obj (car list))
              list
              (memq obj (cdr list)))))

    (define (memv obj list)
      (if (empty? list)
          #f
          (if (eqv? obj (car list))
              list
              (memv obj (cdr list)))))

    (define (caar l) (car (car l)))
    (define (cadr l) (car (cdr l)))
    (define (cdar l) (cdr (car l)))
    (define (cddr l) (cdr (cdr l)))

    (define (caaar l) (car (car (car l))))
    (define (caadr l) (car (car (cdr l))))
    (define (cadar l) (car (cdr (car l))))
    (define (caddr l) (car (cdr (cdr l))))
    (define (cdaar l) (cdr (car (car l))))
    (define (cdadr l) (cdr (car (cdr l))))
    (define (cddar l) (cdr (cdr (car l))))
    (define (cdddr l) (cdr (cdr (cdr l))))

    (define (caaaar l) (car (car (car (car l)))))
    (define (caaadr l) (car (car (car (cdr l)))))
    (define (caadar l) (car (car (cdr (car l)))))
    (define (caaddr l) (car (car (cdr (cdr l)))))
    (define (cadaar l) (car (cdr (car (car l)))))
    (define (cadadr l) (car (cdr (car (cdr l)))))
    (define (caddar l) (car (cdr (cdr (car l)))))
    (define (cadddr l) (car (cdr (cdr (cdr l)))))
    (define (cdaaar l) (cdr (car (car (car l)))))
    (define (cdaadr l) (cdr (car (car (cdr l)))))
    (define (cdadar l) (cdr (car (cdr (car l)))))
    (define (cdaddr l) (cdr (car (cdr (cdr l)))))
    (define (cddaar l) (cdr (cdr (car (car l)))))
    (define (cddadr l) (cdr (cdr (car (cdr l)))))
    (define (cdddar l) (cdr (cdr (cdr (car l)))))
    (define (cddddr l) (cdr (cdr (cdr (cdr l)))))

    (define-syntax case
      (syntax-rules (else =>)
        ((case (key ...)
           clauses ...)
         (let ((atom-key (key ...)))
           (case atom-key clauses ...)))
        ((case key
           (else => result))
         (result key))
        ((case key
           (else result1 result2 ...))
         (begin result1 result2 ...))
        ((case key
           ((atoms ...) result1 result2 ...))
         (if (memv key '(atoms ...))
             (begin result1 result2 ...)))
        ((case key
           ((atoms ...) => result))
         (if (memv key '(atoms ...))
             (result key)))
        ((case key
           ((atoms ...) => result)
           clause clauses ...)
         (if (memv key '(atoms ...))
             (result key)
             (case key clause clauses ...)))
        ((case key
           ((atoms ...) result1 result2 ...)
           clause clauses ...)
         (if (memv key '(atoms ...))
             (begin result1 result2 ...)
             (case key clause clauses ...)))))

    (define-syntax cond
      (syntax-rules (else =>)
        ((cond (else result1 result2 ...))
        (begin result1 result2 ...))
        ((cond (test => result))
        (let ((temp test))
          (if temp (result temp))))
        ((cond (test => result) clause1 clause2 ...)
        (let ((temp test))
          (if temp
              (result temp)
              (cond clause1 clause2 ...))))
        ((cond (test)) test)
        ((cond (test) clause1 clause2 ...)
        (let ((temp test))
          (if temp temp
              (cond clause1 clause2 ...))))
        ((cond (test result1 result2 ...))
        (if test (begin result1 result2 ...)))
        ((cond (test result1 result2 ...)
              clause1 clause2 ...)
        (if test
            (begin result1 result2 ...)
            (cond clause1 clause2 ...)))))

    (define (list? l)
      (if (empty? l)
        #t
        (if (pair? l)
          (let ((next (cdr l)))
            (or (empty? next) (list? next)))
          #f)))

    (define (length l)
      (letrec ((f (lambda (ll count)
                    (if (empty? ll)
                      count
                      (f (cdr ll) (+ 1 count))))))
        (f l 0)))

    (define (last l)
      (if (empty? l)
          (list)
          (if (= 1 (length l))
              (car l)
              (last (cdr l)))))

    (define (boolean? b)
      (or (eq? b #t) (eq? b #f)))

    (define (boolean=? a b)
      (or
        (and (eq? a #t) (eq? b #t))
        (and (eq? a #f) (eq? b #f))))

    (define (number? n)
      (or (integer? n)))

    (define (negative? n)
      (< n 0))

    (define (positive? n)
      (>= n 0))

    (define (number->string num)
      (letrec* ((i->c (lambda (i)
                       (integer->char (+ i 48))))
                (n->s (lambda (n l)
                        (if (< n 10)
                          (cons (i->c n) l)
                          (n->s (/ n 10) (cons (i->c (modulo n 10)) l)))))
                (digits (n->s (abs num) '())))
        (if (negative? num)
          (list->string (cons #\- digits))
          (list->string digits))))

    (define (string->list str)
      (letrec ((s->l (lambda (l i)
                       (if (< i 0)
                         l
                         (s->l (cons (string-ref str i) l) (- i 1))))))
        (s->l '() (- (string-length str) 1))))

    (define (string-append . strings1)
      (letrec ((s->l (lambda strings2
                       (if (= 0 (length strings2))
                         (list)
                         (append (string->list (car strings2))
                                 (apply s->l (cdr strings2)))))))
        (list->string (apply s->l strings1))))

    (--define-native write write) ; don't export this

    (define (newline)
      (write #\newline))

    (define (write-string . args)
      (if (not (empty? args))
          (begin
            (write (car args))
            (apply write-string (cdr args)))))

    (define equal? '()) ; temporary

    (define (list=? a b) ; don't export this
      (if (= (length a) (length b))
          (if (empty? a)
              #t
              (if (equal? (car a) (car b))
                (list=? (cdr a) (cdr b))
                #f))
          #f))

    (define (string=? a b)
      (and
        (string? a)
        (string? b)
        (list=? (string->list a) (string->list b))))

    (define (pair=? a b) ; don't export this
      (if (equal? (car a) (car b))
        (equal? (cdr a) (cdr b))
        #f))

    (define (char=? a b)
      (if (and (char? a) (char? b))
        (eq? a b)
        #f))

    (define (char<? a b)
      (if (and (char? a) (char? b))
        (< (char->integer a) (char->integer b))
        #f))

    (define (char<=? a b)
      (or (char<? a b) (char=? a b)))

    (define (char>? a b)
      (if (and (char? a) (char? b))
        (> (char->integer a) (char->integer b))
        #f))

    (define (char>=? a b)
      (or (char>? a b) (char=? a b)))

    (define (equal? a b)
      (cond
       ((and (boolean? a) (boolean? b)) (eq? a b))
       ((and (char? a)    (char? b))    (eq? a b))
       ((and (number? a)  (number? b))  (eq? a b))
       ((and (list? a)    (list? b))    (list=? a b))
       ((and (pair? a)    (pair? b))    (pair=? a b))
       ((and (string? a)  (string? b))  (string=? a b))
       ((and (symbol? a)  (symbol? b))  (eq? a b))
       (else #f)))

    (define-syntax when
      (syntax-rules ()
        ((when test result1 result2 ...)
         (if test
             (begin result1 result2 ...)))))

    (define-syntax unless
      (syntax-rules ()
        ((unless test result1 result2 ...)
         (if (not test)
             (begin result1 result2 ...)))))

    (define (reverse l)
      (letrec ((rv (lambda (l1 l2)
                 (if (empty? l1)
                     l2
                     (rv (cdr l1) (cons (car l1) l2))))))
        (rv l '())))

    (define (zero? n)
      (= n 0))

    (define (even? n)
      (= 0 (modulo n 2)))

    (define (odd? n)
      (= 1 (modulo n 2)))

    (define (map fn l)
      (letrec ((m (lambda (l l2)
                    (if (empty? l)
                      l2
                      (m (cdr l) (cons (fn (car l)) l2))))))
        (reverse (m l '()))))

    (define (string->number str)
      (letrec* ((digits (map (lambda (c)
                              (- (char->integer c) 48))
                            (string->list str)))
                (d->n (lambda (d n)
                        (if (empty? d)
                          n
                          (if (and (>= (car d) 0) (<= (car d) 9))
                            (d->n (cdr d) (+ (* n 10) (car d)))
                            (d->n (cdr d) n)))))
                (result (d->n digits 0)))
               (if (= (car digits) -3)
                 (* result -1)
                 result)))

    (define (max . nums)
      (letrec ((get-max (lambda (nums biggest)
                          (if (empty? nums)
                              biggest
                              (if (> (car nums) biggest)
                                  (get-max (cdr nums) (car nums))
                                  (get-max (cdr nums) biggest))))))
        (get-max (cdr nums) (car nums))))

    (define (min . nums)
      (letrec ((get-min (lambda (nums smallest)
                          (if (empty? nums)
                              smallest
                              (if (< (car nums) smallest)
                                  (get-min (cdr nums) (car nums))
                                  (get-min (cdr nums) smallest))))))
        (get-min (cdr nums) (car nums))))

    (define (make-list count fill)
      (letrec ((m-l (lambda (c l)
                      (if (= c 0)
                        l
                        (m-l (- c 1) (cons fill l))))))
        (m-l count '())))

  ))
