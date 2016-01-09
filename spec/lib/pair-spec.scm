(import (scheme base))

(include "assert")

(assert (not (pair? 1)))
(assert (not (pair? "string")))
(assert (not (pair? '())))
(assert (not (pair? (list))))
(assert (pair? (list 1)))

;; (assert (eq? 1 (car (1 . 2))))
;; (assert (eq? 1 (car (list 1 2))))

;; (assert (eq? 2 (cdr (1 . 2))))
;; (assert (eq? (list 2) (cdr (list 1 2))))
