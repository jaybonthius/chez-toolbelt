(library (conformance)
  (export start-conformance-server copy-suite)
  (import (chezscheme) (h2o) (json) (datastar) (support))

  (define (field event name default)
    (cond [(assoc name event) => cdr] [else default]))

  (define (symbol-field event name default)
    (cond [(assoc name event) => (lambda (p) (string->symbol (cdr p)))] [else default]))

  ;; The suites' README gives an attribute value as a JSON boolean.
  (define (attribute-text value)
    (if (string? value) value (json->string value)))

  (define (send-event stream event)
    (let ([type (field event "type" #f)])
      (cond
        [(equal? type "patchElements")
         (patch-elements stream (field event "elements" #f)
           (patch-elements-options
             [selector (field event "selector" #f)]
             [mode (symbol-field event "mode" (patch-mode outer))]
             [namespace (symbol-field event "namespace" (element-namespace html))]
             [use-view-transition (field event "useViewTransition" #f)]
             [view-transition-selector (field event "viewTransitionSelector" #f)]
             [event-id (field event "eventId" #f)]
             [retry-duration (field event "retryDuration" 1000)]))]
        [(equal? type "patchSignals")
         (patch-signals stream
           (field event "signals-raw" (field event "signals" #f))
           (patch-signals-options
             [only-if-missing (field event "onlyIfMissing" #f)]
             [event-id (field event "eventId" #f)]
             [retry-duration (field event "retryDuration" 1000)]))]
        [(equal? type "executeScript")
         (execute-script stream (field event "script" #f)
           (execute-script-options
             [auto-remove (field event "autoRemove" #t)]
             [attributes (map (lambda (p) (cons (car p) (attribute-text (cdr p))))
                              (field event "attributes" '()))]
             [event-id (field event "eventId" #f)]
             [retry-duration (field event "retryDuration" 1000)]))]
        [else (assertion-violationf 'send-event "unknown event type ~s" type)])))

  (define (test-handler request)
    (let ([events (field (read-signals request) "events" '#())] [stream (make-stream)])
      (vector-for-each (lambda (event) (send-event stream event)) events)
      (stream-close! stream)
      (sse-response stream)))

  (define (start-conformance-server port)
    (start-server (list (http-listener "127.0.0.1" port))
      (list (make-host (list (format "127.0.0.1:~a" port))
              (list (route "/test" (handler test-handler)))))))

  ;; The suites write their results beside their cases, so they run from a
  ;; copy and leave the submodule clean.
  (define (copy-suite directory)
    (let ([copy (string-append scratch "/" (path-last directory))])
      (run "cp" "-R" (string-append "tests/datastar/sdk/" directory) copy)
      copy)))
