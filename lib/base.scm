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

(define boolean?
  (lambda (b)
    (or (eq? b #t) (eq? b #f))))

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
