(import (scheme base)
        (assert))

;; AFAICT R7RS does not specify a void/undefined value,
;; but Chicken Scheme has #<unspecified>, which we copied.

(define unspecified (if #f #f))

(assert (eq? unspecified (cond (#f #f))))
