(define sse-headers '(("content-type" . "text/event-stream") ("cache-control" . "no-cache")))

(define (check-stream who stream)
  (unless (stream? stream) (assertion-violationf who "~s is not a stream" stream)))

(define sse-response
  (case-lambda
    [(stream) (sse-response stream '())]
    [(stream headers)
     (check-stream 'sse-response stream)
     (unless (list? headers)
       (assertion-violationf 'sse-response "~s is not a list of headers" headers))
     (stream-response 200 (append sse-headers headers) stream)]))

(define (put-text port s) (put-bytevector port (string->utf8 s)))

(define (put-data-line port name value)
  (put-text port "data: ")
  (put-text port name)
  (put-u8 port 32)
  (put-text port value)
  (put-u8 port 10))

;; The client's parser ends a line at CR, LF or CRLF, so each of them must
;; start a new data line. A line end at the very end would add an empty
;; line, which the client joins as a trailing newline.
(define (put-data-lines port name bytes)
  (let ([n (bytevector-length bytes)] [prefix (string->utf8 (string-append "data: " name " "))])
    (define (line start end)
      (put-bytevector port prefix)
      (put-bytevector port bytes start (fx- end start))
      (put-u8 port 10))
    (let loop ([start 0] [i 0])
      (if (fx= i n)
          (unless (fx= start n) (line start n))
          (let ([b (bytevector-u8-ref bytes i)])
            (cond
              [(fx= b 10) (line start i) (loop (fx+ i 1) (fx+ i 1))]
              [(fx= b 13)
               (line start i)
               (let ([next (if (and (fx< (fx+ i 1) n) (fx= (bytevector-u8-ref bytes (fx+ i 1)) 10))
                               (fx+ i 2)
                               (fx+ i 1))])
                 (loop next next))]
              [else (loop start (fx+ i 1))]))))))

;; One bytevector per event, sent with one stream-send!, so that events sent
;; from several threads never interleave.
(define (event-bytes type event-id retry-duration put-data)
  (let-values ([(port extract) (open-bytevector-output-port)])
    (put-text port "event: ")
    (put-text port type)
    (put-u8 port 10)
    (when event-id
      (put-text port "id: ")
      (put-text port event-id)
      (put-u8 port 10))
    (unless (= retry-duration default-retry-duration)
      (put-text port "retry: ")
      (put-text port (number->string retry-duration))
      (put-u8 port 10))
    (put-data port)
    (put-u8 port 10)
    (extract)))

(define (elements->bytevector who elements)
  (cond
    [(html? elements) (html->bytevector elements)]
    [(string? elements) (string->utf8 elements)]
    [(bytevector? elements) elements]
    [(not elements) #f]
    [else (assertion-violationf who "~s is not html, a string, a bytevector or #f" elements)]))

(define (element-event elements options)
  (event-bytes "datastar-patch-elements"
    (element-options-event-id options) (element-options-retry-duration options)
    (lambda (port)
      (let ([selector (element-options-selector options)]
            [mode (element-options-mode options)]
            [view-transition-selector (element-options-view-transition-selector options)]
            [namespace (element-options-namespace options)])
        (when selector (put-data-line port "selector" selector))
        (unless (eq? mode 'outer) (put-data-line port "mode" (symbol->string mode)))
        (when (element-options-use-view-transition options)
          (put-data-line port "useViewTransition" "true")
          (when view-transition-selector
            (put-data-line port "viewTransitionSelector" view-transition-selector)))
        (unless (eq? namespace 'html) (put-data-line port "namespace" (symbol->string namespace)))
        (when elements (put-data-lines port "elements" elements))))))

(define default-element-options (patch-elements-options))

(define patch-elements
  (case-lambda
    [(stream elements) (patch-elements stream elements default-element-options)]
    [(stream elements options)
     (check-stream 'patch-elements stream)
     (check-options 'patch-elements options element-options? 'patch-elements-options)
     (stream-send! stream
       (element-event (elements->bytevector 'patch-elements elements) options))]))

(define (json-object? x)
  (and (list? x) (for-all (lambda (p) (and (pair? p) (or (string? (car p)) (symbol? (car p))))) x)))

;; The client runs a string that is not JSON as JavaScript.
(define (signals->bytevector who signals)
  (cond
    [(string? signals)
     (unless (json-object? (string->json signals))
       (assertion-violationf who "~s is not a JSON object" signals))
     (string->utf8 signals)]
    [(json-object? signals) (json->bytevector signals)]
    [else (assertion-violationf who "~s is not a JSON object or a string" signals)]))

(define default-signal-options (patch-signals-options))

(define patch-signals
  (case-lambda
    [(stream signals) (patch-signals stream signals default-signal-options)]
    [(stream signals options)
     (check-stream 'patch-signals stream)
     (check-options 'patch-signals options signal-options? 'patch-signals-options)
     (let ([bytes (signals->bytevector 'patch-signals signals)])
       (stream-send! stream
         (event-bytes "datastar-patch-signals"
           (signal-options-event-id options) (signal-options-retry-duration options)
           (lambda (port)
             (when (signal-options-only-if-missing options)
               (put-data-line port "onlyIfMissing" "true"))
             (put-data-lines port "signals" bytes)))))]))

(define (contains-ci? s part)
  (let ([n (string-length s)] [m (string-length part)])
    (let loop ([i 0])
      (and (fx<= (fx+ i m) n)
           (or (let compare ([j 0])
                 (or (fx= j m)
                     (and (char-ci=? (string-ref s (fx+ i j)) (string-ref part j))
                          (compare (fx+ j 1)))))
               (loop (fx+ i 1)))))))

;; The HTML parser ends a script element at the first "</script", and after
;; "<!--" and "<script" it skips the next one.
(define (check-script who script)
  (unless (string? script) (assertion-violationf who "~s is not a string" script))
  (when (or (contains-ci? script "</script") (contains-ci? script "<!--"))
    (assertion-violationf who "~s contains </script or <!--" script)))

(define (put-attribute-value port s)
  (string-for-each
    (lambda (c)
      (case c
        [(#\&) (put-string port "&amp;")]
        [(#\<) (put-string port "&lt;")]
        [(#\>) (put-string port "&gt;")]
        [(#\") (put-string port "&quot;")]
        [else (put-char port c)]))
    s))

(define (script-element script options)
  (let-values ([(port extract) (open-string-output-port)])
    (put-string port "<script")
    (for-each
      (lambda (attribute)
        (put-char port #\space)
        (put-string port (car attribute))
        (put-string port "=\"")
        (put-attribute-value port (cdr attribute))
        (put-char port #\"))
      (script-options-attributes options))
    (when (script-options-auto-remove options)
      (put-string port " data-effect=\"el.remove()\""))
    (put-char port #\>)
    (put-string port script)
    (put-string port "</script>")
    (extract)))

(define default-script-options (execute-script-options))

(define execute-script
  (case-lambda
    [(stream script) (execute-script stream script default-script-options)]
    [(stream script options)
     (check-stream 'execute-script stream)
     (check-script 'execute-script script)
     (check-options 'execute-script options script-options? 'execute-script-options)
     (stream-send! stream
       (element-event (string->utf8 (script-element script options))
         (make-element-options '() "body" 'append 'html #f #f
           (script-options-event-id options) (script-options-retry-duration options))))]))
