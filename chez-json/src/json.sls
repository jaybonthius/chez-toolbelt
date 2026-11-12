(library (json)
  (export
    ;; Reading
    json-read string->json bytevector->json
    ;; Writing
    json-write json->string json->bytevector
    ;; Errors
    json-error? json-error-position
    ;; Settings
    json-max-depth json-max-number-digits json-allow-duplicate-keys?
    json-escape-html?)
  (import (chezscheme))

  (define-condition-type &json &error make-json-error json-error?
    (position json-error-position))

  (define json-max-depth
    (make-thread-parameter 1000
      (lambda (n)
        (unless (and (fixnum? n) (fx>= n 0))
          (assertion-violationf 'json-max-depth "~s is not a nonnegative fixnum" n))
        n)))

  (define json-max-number-digits
    (make-thread-parameter 4300
      (lambda (n)
        (unless (and (fixnum? n) (fx> n 0))
          (assertion-violationf 'json-max-number-digits "~s is not a positive fixnum" n))
        n)))

  (define json-allow-duplicate-keys?
    (make-thread-parameter #f (lambda (x) (and x #t))))

  (define json-escape-html?
    (make-thread-parameter #f (lambda (x) (and x #t))))

  (include "json/eisel-lemire.ss")
  (include "json/read.ss")
  (include "json/float.ss")
  (include "json/write.ss"))
