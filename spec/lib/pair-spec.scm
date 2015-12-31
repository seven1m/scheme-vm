(include "assert")

(assert (eq? #f (pair? 1)))
(assert (eq? #f (pair? "string")))
(assert (eq? #f (pair? '())))
(assert (eq? #f (pair? (list))))
(assert (eq? #t (pair? (list 1))))
