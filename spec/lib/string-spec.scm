(import (scheme base))

(include "assert")

(assert (string? "foo"))
(assert (not (string? 1)))
