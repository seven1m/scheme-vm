(import (scheme base)
        (assert))

(define when1 #f)
(define when2 #f)

(when (= 1 1)
      (set! when1 1))

(when (= 1 2)
      (set! when2 2))

(assert (eq? 1 when1))
(assert (eq? #f when2))

(define unless1 #f)
(define unless2 #f)

(unless (= 1 1)
        (set! unless1 1))

(unless (= 1 2)
        (set! unless2 2))

        (assert (eq? #f unless1))
        (assert (eq? 2 unless2))
