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
