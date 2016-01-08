(import (scheme base))

(include "assert")

; import all
(begin
  (import (fixtures library-test))
  (assert (eq? 12 (foo)))
  (assert (eq? 13 (bar)))
  (assert (eq? 13 (baz))))

; import only
(begin
  (import (only (fixtures library-test) foo))
  (assert (eq? 12 (foo))))

; import except
(begin
  (import (except (fixtures library-test) foo))
  (assert (eq? 13 (bar)))
  (assert (eq? 13 (baz))))

; import prefix
(begin
  (import (prefix (fixtures library-test) my-))
  (assert (eq? 12 (my-foo)))
  (assert (eq? 13 (my-bar)))
  (assert (eq? 13 (my-baz))))

; import rename
(begin
  (import (rename (fixtures library-test) (foo fooz) (bar barz)))
  (assert (eq? 12 (fooz)))
  (assert (eq? 13 (barz))))

; import rename + prefix + except
(begin
  (import (rename
           (prefix
            (except (fixtures library-test) bar)
            my-)
           (my-foo my-fooz)))
  (assert (eq? 12 (my-fooz)))
  (assert (eq? 13 (my-baz))))
