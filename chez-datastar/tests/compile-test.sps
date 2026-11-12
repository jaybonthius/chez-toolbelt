(import (chezscheme) (check) (support))

(define programs 0)

;; From a file, as a user's program is compiled.
(define (compile-error . body)
  (set! programs (+ programs 1))
  (let ([source (format "~a/program-~a.sps" scratch programs)])
    (with-output-to-file source
      (lambda () (for-each write (cons '(import (chezscheme) (datastar)) body))))
    (guard (c [(syntax-violation? c) c])
      (parameterize ([compile-file-message #f])
        (compile-program source (string-append (path-root source) ".so")))
      #f)))

(define (raised? c who message)
  (and c (eq? who (condition-who c)) (string=? message (condition-message c))))

(test "a misspelled option fails to compile, naming the form"
  (check (raised? (compile-error '(patch-elements-options [selecter "#a"]))
           'patch-elements-options "unknown option"))
  (check (raised? (compile-error '(patch-signals-options [only-if-mising #t]))
           'patch-signals-options "unknown option"))
  (check (raised? (compile-error '(execute-script-options [autoremove #f]))
           'execute-script-options "unknown option"))
  (check (raised? (compile-error '(fetch-options [contentType "form"]))
           'fetch-options "unknown option")))

(test "an option given twice fails to compile"
  (check (raised? (compile-error '(execute-script-options [auto-remove #f] [auto-remove #t]))
           'execute-script-options "option given twice")))

(test "a malformed options form fails to compile"
  (check (raised? (compile-error '(patch-elements-options selector "#a"))
           'patch-elements-options "expected (patch-elements-options [name value] ...)"))
  (check (raised? (compile-error '(fetch-options ["selector" "#a"]))
           'fetch-options "expected (fetch-options [name value] ...)")))

(test "a value outside a closed set fails to compile"
  (for-each
    (lambda (form)
      (let ([c (compile-error form)])
        (check (and c (string=? (format "universe of ~a does not include specified symbol" (car form))
                                (condition-message c))))))
    '((patch-mode sideways) (element-namespace xml) (fetch-content-type text)
      (fetch-retry sometimes) (request-cancellation never)))
  (check (compile-error '(patch-elements-options [mode (patch-mode sidways)]))))

(test "well-formed options compile"
  (check-equal? #f
    (compile-error
      '(patch-elements-options [selector "#a"] [mode (patch-mode inner)]
         [namespace (element-namespace svg)] [use-view-transition #t]
         [view-transition-selector "#b"] [event-id "1"] [retry-duration 10])
      '(patch-signals-options [only-if-missing #t] [event-id "1"] [retry-duration 10])
      '(execute-script-options [auto-remove #f] [attributes '()] [event-id "1"] [retry-duration 10])
      '(fetch-options [content-type (fetch-content-type form)] [retry (fetch-retry never)]
         [request-cancellation (request-cancellation disabled)]))))

(test-exit)
