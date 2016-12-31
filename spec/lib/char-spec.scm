(import (scheme base)
        (assert))

(assert (= 48 (char->integer #\0)))

(assert (= #\0 (integer->char 48)))
(assert (not (= #\1 (integer->char 48))))

(assert (eq? #\a (char-downcase #\A)))
(assert (eq? #\a (char-downcase #\a)))
