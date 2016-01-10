(import (scheme base)
        (assert))

(letrec ((even?
          (lambda (n)
            (if (= n 0)
                #t
                (odd? (- n 1)))))
         (odd?
          (lambda (n)
            (if (= n 0)
                #f
                (even? (- n 1))))))
  (assert (even? 8))
  (assert (even? 2))
  (assert (not (even? 3)))
  (assert (odd? 9))
  (assert (not (odd? 8))))
