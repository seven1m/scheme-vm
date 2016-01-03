(include "assert")

; import all
(begin
  (import (fixtures library-test))
  (assert (eq? 12 (foo)))
  (assert (eq? 13 (bar)))
  (assert (eq? 13 (baz))))
