(import (chezscheme) (parakeet))

(unless (fixnum? (abi-version)) (exit 1))
(printf "tests/consumer.sps: (parakeet) loads from ~a\n" (current-directory))
