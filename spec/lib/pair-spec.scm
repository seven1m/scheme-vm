(include "assert")

(assert (not (pair? 1)))
(assert (not (pair? "string")))
(assert (not (pair? '())))
(assert (not (pair? (list))))
(assert (pair? (list 1)))
