(import (scheme base)
        (assert))

;; AFAICT R7RS does not defined a void/undefined value,
;; but (if #f #f) can substitute for it
(assert (equal?
         3
         (length
          (list 1
                (if #f #f)
                3))))
