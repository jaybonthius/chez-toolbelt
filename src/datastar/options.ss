(define-enumeration patch-mode (outer inner remove replace prepend append before after) patch-modes)
(define-enumeration element-namespace (html svg mathml) element-namespaces)
(define-enumeration fetch-content-type (json form) fetch-content-types)
(define-enumeration fetch-retry (auto error always never) fetch-retries)
(define-enumeration request-cancellation (auto cleanup disabled) request-cancellations)

(define (member-of set)
  (let ([universe (enum-set-universe set)])
    (lambda (x) (and (symbol? x) (enum-set-member? x universe)))))

(define patch-mode? (member-of (patch-modes)))
(define element-namespace? (member-of (element-namespaces)))
(define fetch-content-type? (member-of (fetch-content-types)))
(define fetch-retry? (member-of (fetch-retries)))
(define request-cancellation? (member-of (request-cancellations)))

(define default-retry-duration 1000)

(define (count? n) (and (integer? n) (exact? n) (>= n 0)))

(define (string-has? s matches?)
  (let loop ([i 0])
    (and (fx< i (string-length s))
         (or (matches? (string-ref s i)) (loop (fx+ i 1))))))

(define (line-break? c) (or (char=? c #\newline) (char=? c #\return)))

;; A line break would end the data line and let the rest of the value be
;; read as another field.
(define (single-line? s) (and (string? s) (not (string-has? s line-break?))))

;; Event ids come back in a Last-Event-ID request header, and one-shot
;; options and action headers are headers too. fetch refuses NUL, CR, LF and
;; characters above U+00FF in a header, and a server reads its bytes as
;; Latin-1, so only printable ASCII and tab arrive unchanged.
(define (header-text? s)
  (and (string? s)
       (not (string-has? s (lambda (c) (not (or (char=? c #\tab) (char<=? #\space c #\~))))))))

(define (optional valid?) (lambda (x) (or (not x) (valid? x))))

(define (attribute-name? s)
  (and (string? s)
       (fx> (string-length s) 0)
       (not (string-has? s
              (lambda (c)
                (or (char-whitespace? c)
                    (char<? c #\space)
                    (char<=? #\delete c #\x9f)
                    (memv c '(#\" #\' #\< #\> #\/ #\=))))))))

(define (attributes? x)
  (and (list? x)
       (for-all (lambda (p) (and (pair? p) (attribute-name? (car p)) (string? (cdr p)))) x)))

(define omitted (list 'omitted))

(define (option who name value valid? default)
  (cond
    [(eq? value omitted) default]
    [(valid? value) value]
    [else (assertion-violationf who "~s is not a valid value for ~s" value name)]))

;; The values are bound before the call because a call's arguments are
;; evaluated in an unspecified order, and they must be evaluated as written.
(meta define (expand-options x who names procedure)
  (define (check-names given)
    (let loop ([given given] [seen '()])
      (unless (null? given)
        (let ([name (syntax->datum (car given))])
          (unless (memq name names)
            (syntax-violation who "unknown option" x (car given)))
          (when (memq name seen)
            (syntax-violation who "option given twice" x (car given)))
          (loop (cdr given) (cons name seen))))))
  (syntax-case x ()
    [(_ [name value] ...)
     (for-all identifier? #'(name ...))
     (begin
       (check-names #'(name ...))
       (with-syntax ([procedure procedure]
                     [(temporary ...) (generate-temporaries #'(name ...))])
         (with-syntax ([(argument ...)
                        (let ([given (map cons (syntax->datum #'(name ...)) #'(temporary ...))])
                          (map (lambda (name) (cond [(assq name given) => cdr] [else #'omitted]))
                               names))])
           #'(let* ([temporary value] ...) (procedure '(name ...) argument ...)))))]
    [_ (syntax-violation who (format "expected (~a [name value] ...)" who) x)]))

(define-record-type element-options
  (nongenerative) (sealed #t)
  (fields given selector mode namespace use-view-transition view-transition-selector
          event-id retry-duration))

(define (checked-element-options given selector mode namespace use-view-transition
          view-transition-selector event-id retry-duration)
  (define who 'patch-elements-options)
  (make-element-options given
    (option who 'selector selector (optional single-line?) #f)
    (option who 'mode mode patch-mode? 'outer)
    (option who 'namespace namespace element-namespace? 'html)
    (option who 'use-view-transition use-view-transition boolean? #f)
    (option who 'view-transition-selector view-transition-selector (optional single-line?) #f)
    (option who 'event-id event-id (optional header-text?) #f)
    (option who 'retry-duration retry-duration count? default-retry-duration)))

(define-syntax patch-elements-options
  (lambda (x)
    (expand-options x 'patch-elements-options
      '(selector mode namespace use-view-transition view-transition-selector event-id
        retry-duration)
      #'checked-element-options)))

(define-record-type signal-options
  (nongenerative) (sealed #t)
  (fields given only-if-missing event-id retry-duration))

(define (checked-signal-options given only-if-missing event-id retry-duration)
  (define who 'patch-signals-options)
  (make-signal-options given
    (option who 'only-if-missing only-if-missing boolean? #f)
    (option who 'event-id event-id (optional header-text?) #f)
    (option who 'retry-duration retry-duration count? default-retry-duration)))

(define-syntax patch-signals-options
  (lambda (x)
    (expand-options x 'patch-signals-options '(only-if-missing event-id retry-duration)
      #'checked-signal-options)))

(define-record-type script-options
  (nongenerative) (sealed #t)
  (fields given auto-remove attributes event-id retry-duration))

(define (checked-script-options given auto-remove attributes event-id retry-duration)
  (define who 'execute-script-options)
  (make-script-options given
    (option who 'auto-remove auto-remove boolean? #t)
    (option who 'attributes attributes attributes? '())
    (option who 'event-id event-id (optional header-text?) #f)
    (option who 'retry-duration retry-duration count? default-retry-duration)))

(define-syntax execute-script-options
  (lambda (x)
    (expand-options x 'execute-script-options '(auto-remove attributes event-id retry-duration)
      #'checked-script-options)))

(define (check-options who options options? form)
  (unless (options? options)
    (assertion-violationf who "~s is not made by ~s" options form)))

;; The client reads only some options from a one-shot response's headers;
;; the others would be dropped silently.
(define (refuse-options who given names)
  (for-each
    (lambda (name)
      (when (memq name given)
        (assertion-violationf who "option ~s cannot be sent in a one-shot response" name)))
    names))
