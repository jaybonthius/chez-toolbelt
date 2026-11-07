;; Besides quotes and line ends, & is escaped because an HTML attribute
;; decodes &#39; to a quote, and a backslash because Datastar's statement
;; splitter takes one before the closing quote as escaping it.
(define (put-js-string port s)
  (put-char port #\')
  (string-for-each
    (lambda (c)
      (cond
        [(char=? c #\newline) (put-string port "\\n")]
        [(char=? c #\return) (put-string port "\\r")]
        [(char=? c #\tab) (put-string port "\\t")]
        [(or (char<? c #\space) (char=? c #\delete) (memv c '(#\' #\" #\\ #\& #\< #\>)))
         (format port "\\x~2,'0x" (char->integer c))]
        [(memv c '(#\x2028 #\x2029)) (format port "\\u~x" (char->integer c))]
        [else (put-char port c)]))
    s)
  (put-char port #\'))

(define (finite-number? x)
  (or (and (integer? x) (exact? x)) (and (flonum? x) (not (nan? x)) (not (infinite? x)))))

(define (put-js-number port x)
  (if (flonum? x) (put-string port (json->string x)) (put-string port (number->string x))))

;; JSON written as a JavaScript literal with single-quoted strings, so that
;; it contains no double quote to end the attribute. Keys are computed,
;; ['key']: value, because a literal '__proto__' key sets the prototype
;; instead of making a property.
(define (put-js-value who name port value)
  (let loop ([value value] [depth 0])
    (define (fail) (assertion-violationf who "~s is not a valid value for ~s" value name))
    (unless (fx< depth (json-max-depth))
      (assertion-violationf who "~s nests deeper than ~s" name (json-max-depth)))
    (cond
      [(string? value) (put-js-string port value)]
      [(finite-number? value) (put-js-number port value)]
      [(eq? value #t) (put-string port "true")]
      [(eq? value #f) (put-string port "false")]
      [(eq? value 'null) (put-string port "null")]
      [(vector? value)
       (put-char port #\[)
       (let elements ([i 0])
         (when (fx< i (vector-length value))
           (unless (fx= i 0) (put-string port ", "))
           (loop (vector-ref value i) (fx+ depth 1))
           (elements (fx+ i 1))))
       (put-char port #\])]
      [(json-object? value)
       (put-char port #\{)
       (let entries ([pairs value] [first? #t])
         (unless (null? pairs)
           (unless first? (put-string port ", "))
           (put-char port #\[)
           (let ([key (caar pairs)])
             (put-js-string port (if (symbol? key) (symbol->string key) key)))
           (put-string port "]: ")
           (loop (cdar pairs) (fx+ depth 1))
           (entries (cdr pairs) #f)))
       (put-char port #\})]
      [else (fail)])))

(define token-punctuation (string->list "!#$%&'*+-.^_`|~"))

(define (token? s)
  (and (string? s)
       (fx> (string-length s) 0)
       (not (string-has? s
              (lambda (c)
                (not (or (char<=? #\a c #\z) (char<=? #\A c #\Z) (char<=? #\0 c #\9)
                         (memv c token-punctuation))))))))

(define (headers? x)
  (and (list? x)
       (for-all (lambda (p) (and (pair? p) (token? (car p)) (header-text? (cdr p)))) x)))

(define-record-type action-options
  (nongenerative) (sealed #t)
  (fields text))

(define (checked-action-options given content-type filter-signals-include filter-signals-exclude
          selector headers open-when-hidden payload retry retry-interval retry-scaler
          retry-max-wait retry-max-count request-cancellation)
  (define who 'fetch-options)
  (define (given? name) (memq name given))
  (define (check name value valid?)
    (when (and (given? name) (not (valid? value)))
      (assertion-violationf who "~s is not a valid value for ~s" value name)))
  (check 'content-type content-type fetch-content-type?)
  (check 'filter-signals-include filter-signals-include string?)
  (check 'filter-signals-exclude filter-signals-exclude string?)
  (check 'selector selector string?)
  (check 'headers headers headers?)
  (check 'open-when-hidden open-when-hidden boolean?)
  (check 'retry retry fetch-retry?)
  (check 'retry-interval retry-interval count?)
  (check 'retry-scaler retry-scaler finite-number?)
  (check 'retry-max-wait retry-max-wait count?)
  (check 'retry-max-count retry-max-count count?)
  (check 'request-cancellation request-cancellation request-cancellation?)
  (let-values ([(port extract) (open-string-output-port)])
    (define first? #t)
    (define (entry name)
      (put-string port (if first? "{" ", "))
      (set! first? #f)
      (put-string port name)
      (put-string port ": "))
    (when (given? 'content-type)
      (entry "contentType")
      (put-js-string port (symbol->string content-type)))
    (when (or (given? 'filter-signals-include) (given? 'filter-signals-exclude))
      (entry "filterSignals")
      (put-char port #\{)
      (when (given? 'filter-signals-include)
        (put-string port "include: ")
        (put-js-string port filter-signals-include))
      (when (given? 'filter-signals-exclude)
        (when (given? 'filter-signals-include) (put-string port ", "))
        (put-string port "exclude: ")
        (put-js-string port filter-signals-exclude))
      (put-char port #\}))
    (when (given? 'selector)
      (entry "selector")
      (put-js-string port selector))
    (when (given? 'headers)
      (entry "headers")
      (put-js-value who 'headers port headers))
    (when (given? 'open-when-hidden)
      (entry "openWhenHidden")
      (put-string port (if open-when-hidden "true" "false")))
    (when (given? 'payload)
      (entry "payload")
      (put-js-value who 'payload port payload))
    (when (given? 'retry)
      (entry "retry")
      (put-js-string port (symbol->string retry)))
    (for-each
      (lambda (option name value)
        (when (given? option)
          (entry name)
          (put-js-number port value)))
      '(retry-interval retry-scaler retry-max-wait retry-max-count)
      '("retryInterval" "retryScaler" "retryMaxWait" "retryMaxCount")
      (list retry-interval retry-scaler retry-max-wait retry-max-count))
    (when (given? 'request-cancellation)
      (entry "requestCancellation")
      (put-js-string port (symbol->string request-cancellation)))
    (unless first? (put-char port #\}))
    (make-action-options (extract))))

(define-syntax fetch-options
  (lambda (x)
    (expand-options x 'fetch-options
      '(content-type filter-signals-include filter-signals-exclude selector headers
        open-when-hidden payload retry retry-interval retry-scaler retry-max-wait
        retry-max-count request-cancellation)
      #'checked-action-options)))

(define no-action-options (fetch-options))

(define (action who method url options)
  (unless (string? url) (assertion-violationf who "~s is not a string" url))
  (check-options who options action-options? 'fetch-options)
  (let-values ([(port extract) (open-string-output-port)])
    (put-char port #\@)
    (put-string port method)
    (put-char port #\()
    (put-js-string port url)
    (let ([text (action-options-text options)])
      (unless (string=? text "")
        (put-string port ", ")
        (put-string port text)))
    (put-char port #\))
    (extract)))

(define-syntax define-action
  (syntax-rules ()
    [(_ name method)
     (define name
       (case-lambda
         [(url) (action 'name method url no-action-options)]
         [(url options) (action 'name method url options)]))]))

(define-action get "get")
(define-action post "post")
(define-action put "put")
(define-action patch "patch")
(define-action query "query")
(define-action delete "delete")
