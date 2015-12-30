(include "assert")

(assert-eq 1 (last (list 1)))
(assert-eq 3 (last (list 1 2 3)))

(assert-eq #t (empty? (list)))
(assert-eq #f (empty? (list 1 2 3)))

(assert-eq 0 (length (list)))
(assert-eq 3 (length (list 1 2 3)))
