(import (scheme base)
        (assert))

(assert (= 48 (char->integer #\0)))

(assert (= #\0 (integer->char 48)))
