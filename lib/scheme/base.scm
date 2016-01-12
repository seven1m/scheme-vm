(define-library (scheme base)
  (export
   +
   -
   >
   >=
   <
   <=
   =
   and
   append
   apply
   begin
   boolean?
   car
   case
   cdr
   char?
   cond
   cons
   define
   define-syntax
   do
   empty?
   eq?
   equal?
   eqv?
   if
   integer?
   lambda
   last
   length
   let
   let*
   letrec
   letrec*
   list
   list?
   list->string
   memq
   memv
   newline
   not
   null?
   number?
   or
   pair?
   quasiquote
   quote
   set!
   set-car!
   set-cdr!
   string?
   string-length
   string-ref
   string->list
   string-append
   symbol?
   write-string)

  (begin
    (--define-native + base_+)
    (--define-native - base_-)
    (--define-native > base_>)
    (--define-native >= base_>=)
    (--define-native < base_<)
    (--define-native <= base_<=)
    (--define-native = base_=)
    (--define-native append base_append)
    (--define-native apply base_apply)
    (--define-native car base_car)
    (--define-native cdr base_cdr)
    (--define-native char? base_char?)
    (--define-native cons base_cons)
    (--define-native define base_define)
    (--define-native define-syntax base_define_syntax)
    (--define-native eq? base_eq?)
    (--define-native eqv? base_eqv?)
    (--define-native if base_if)
    (--define-native integer? base_integer?)
    (--define-native lambda base_lambda)
    (--define-native list base_list)
    (--define-native list->string base_list_to_string)
    (--define-native null? base_null?)
    (--define-native pair? base_pair?)
    (--define-native quasiquote base_quasiquote)
    (--define-native quote base_quote)
    (--define-native set! base_set!)
    (--define-native set-car! base_set_car!)
    (--define-native set-cdr! base_set_cdr!)
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

    (define empty?
      (lambda (l)
        (null? l)))

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
              (memq obj (cdr list)))))

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

    (define list?
      (lambda (l)
        (if (empty? l)
            #t
            (if (pair? l)
                (let ((next (cdr l)))
                  (or (empty? next) (list? next)))
                #f))))

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

    (define boolean?
      (lambda (b)
        (or (eq? b #t) (eq? b #f))))

    (define (number? n)
      (or (integer? n)))

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

    (--define-native write write) ; ensure this doesn't export

    (define (newline)
      (write #\newline))

    (define write-string
      (lambda args
        (if (not (empty? args))
            (begin
              (write (car args))
              (apply write-string (cdr args))))))

    (define equal? '()) ; temporary

    (define (list-equal? a b)
      (if (= (length a) (length b))
          (if (= 0 (length a))
              #t
              (and (equal? (car a) (car b)) (list-equal? (cdr a) (cdr b))))
          #f))

    (define (equal? a b)
      (cond
       ((and (boolean? a) (boolean? b)) (eq? a b))
       ((and (char? a) (char? b)) (eq? a b))
       ((and (number? a) (number? b)) (eq? a b))
       ((and (list? a) (list? b)) (list-equal? a b))
       ((and (string? a) (string? b)) (eq? a b))
       ((and (symbol? a) (symbol? b)) (eq? a b))
       (else #f)))

  ))
