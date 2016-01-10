(import (scheme base)
        (assert))

(assert (eq? #t (cond (else #t))))
(assert (eq? #t (cond ((= 5 5) #t))))
(assert (eq? #f (cond ((= 5 6) #t) (else #f))))
(assert (eq? #f (cond ((= 5 6) #t) (else #f))))
(assert (eq? 2 (cond ((= 5 6) 1) ((= 10 10) 2) (else 3))))

;(assert (eq? '() (cond ((= 5 6) #t)))) ; this maybe should return a new "undefined" value or something?
                                        ; for now, this causes a runtime error since there is no value on the stack

