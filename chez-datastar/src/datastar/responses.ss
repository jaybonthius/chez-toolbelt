(define (header-value who name value)
  (unless (header-text? value)
    (assertion-violationf who "~s cannot be sent in the ~a header" value name))
  (cons name value))

(define elements-response
  (case-lambda
    [(elements) (elements-response elements default-element-options)]
    [(elements options)
     (define who 'elements-response)
     (let ([body (or (elements->bytevector who elements) (bytevector))])
       (check-options who options element-options? 'patch-elements-options)
       (refuse-options who (element-options-given options)
         '(view-transition-selector event-id retry-duration))
       (let ([selector (element-options-selector options)]
             [mode (element-options-mode options)]
             [namespace (element-options-namespace options)])
         (response 200
           (append
             '(("content-type" . "text/html; charset=utf-8"))
             (if selector (list (header-value who "datastar-selector" selector)) '())
             (if (eq? mode 'outer) '() (list (cons "datastar-mode" (symbol->string mode))))
             (if (eq? namespace 'html)
                 '()
                 (list (cons "datastar-namespace" (symbol->string namespace))))
             (if (element-options-use-view-transition options)
                 '(("datastar-use-view-transition" . "true"))
                 '()))
           body)))]))

(define signals-response
  (case-lambda
    [(signals) (signals-response signals default-signal-options)]
    [(signals options)
     (define who 'signals-response)
     (let ([body (signals->bytevector who signals)])
       (check-options who options signal-options? 'patch-signals-options)
       (refuse-options who (signal-options-given options) '(event-id retry-duration))
       (response 200
         (cons '("content-type" . "application/json")
           (if (signal-options-only-if-missing options)
               '(("datastar-only-if-missing" . "true"))
               '()))
         body))]))

(define script-response
  (case-lambda
    [(script) (script-response script default-script-options)]
    [(script options)
     (define who 'script-response)
     (unless (string? script) (assertion-violationf who "~s is not a string" script))
     (check-options who options script-options? 'execute-script-options)
     (refuse-options who (script-options-given options) '(auto-remove event-id retry-duration))
     (let ([attributes (script-options-attributes options)])
       (response 200
         (cons '("content-type" . "text/javascript; charset=utf-8")
           (if (null? attributes)
               '()
               (list (header-value who "datastar-script-attributes" (json->string attributes)))))
         script))]))
