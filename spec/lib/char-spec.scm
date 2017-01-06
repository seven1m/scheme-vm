(import (scheme base)
        (assert))

(assert (= 48 (char->integer #\0)))

(assert (= #\0 (integer->char 48)))
(assert (not (= #\1 (integer->char 48))))

(assert (eq? #\a (char-downcase #\A)))
(assert (eq? #\a (char-downcase #\a)))

(assert (eq? #\A (char-upcase #\a)))
(assert (eq? #\A (char-upcase #\A)))

(assert (char=? #\a #\a))
(assert (not (char=? #\a #\b)))

(assert (char<? #\a #\b))
(assert (not (char<? #\a #\a)))
(assert (not (char<? #\b #\a)))

(assert (char<=? #\a #\b))
(assert (char<=? #\a #\a))
(assert (not (char<=? #\b #\a)))

(assert (char>? #\b #\a))
(assert (not (char>? #\b #\b)))
(assert (not (char>? #\a #\b)))

(assert (char>=? #\b #\a))
(assert (char>=? #\b #\b))
(assert (not (char>=? #\a #\b)))
