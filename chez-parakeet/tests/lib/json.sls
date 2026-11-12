(library (json)
  (export string->json json-ref)
  (import (chezscheme))

  (define (string->json s)
    (let ([p (open-string-input-port s)])
      (let ([value (read-value p)])
        (skip-space p)
        (unless (eof-object? (peek-char p)) (error 'string->json "trailing text" s))
        value)))

  (define (json-ref value . path)
    (fold-left
      (lambda (value key)
        (if (string? key)
            (let ([entry (assoc key value)])
              (unless entry (error 'json-ref "no such key" key))
              (cdr entry))
            (list-ref value key)))
      value path))

  (define (skip-space p)
    (let ([c (peek-char p)])
      (when (and (char? c) (char-whitespace? c))
        (read-char p)
        (skip-space p))))

  (define (expect p word value)
    (string-for-each
      (lambda (c) (unless (eqv? (read-char p) c) (error 'string->json "bad literal" word)))
      word)
    value)

  (define (read-value p)
    (skip-space p)
    (let ([c (peek-char p)])
      (case c
        [(#\{) (read-char p) (read-members p)]
        [(#\[) (read-char p) (read-elements p)]
        [(#\") (read-char p) (read-string-body p)]
        [(#\t) (expect p "true" #t)]
        [(#\f) (expect p "false" #f)]
        [(#\n) (expect p "null" 'null)]
        [else (read-number p)])))

  (define (read-members p)
    (skip-space p)
    (if (eqv? (peek-char p) #\})
        (begin (read-char p) '())
        (let loop ([members '()])
          (skip-space p)
          (unless (eqv? (read-char p) #\") (error 'string->json "expected a key"))
          (let ([key (read-string-body p)])
            (skip-space p)
            (unless (eqv? (read-char p) #\:) (error 'string->json "expected :"))
            (let ([members (cons (cons key (read-value p)) members)])
              (skip-space p)
              (case (read-char p)
                [(#\,) (loop members)]
                [(#\}) (reverse members)]
                [else (error 'string->json "expected , or }")]))))))

  (define (read-elements p)
    (skip-space p)
    (if (eqv? (peek-char p) #\])
        (begin (read-char p) '())
        (let loop ([elements '()])
          (let ([elements (cons (read-value p) elements)])
            (skip-space p)
            (case (read-char p)
              [(#\,) (loop elements)]
              [(#\]) (reverse elements)]
              [else (error 'string->json "expected , or ]")])))))

  (define (read-string-body p)
    (call-with-string-output-port
      (lambda (out)
        (let loop ()
          (let ([c (read-char p)])
            (cond
              [(eof-object? c) (error 'string->json "unterminated string")]
              [(char=? c #\") (void)]
              [(char=? c #\\)
               (let ([e (read-char p)])
                 (case e
                   [(#\n) (put-char out #\newline)]
                   [(#\t) (put-char out #\tab)]
                   [(#\r) (put-char out #\return)]
                   [(#\b) (put-char out #\backspace)]
                   [(#\f) (put-char out #\page)]
                   [(#\u)
                    (let ([code (string->number (string (read-char p) (read-char p)
                                                  (read-char p) (read-char p)) 16)])
                      (put-char out (integer->char code)))]
                   [else (put-char out e)]))
               (loop)]
              [else (put-char out c) (loop)]))))))

  (define (read-number p)
    (let ([text (call-with-string-output-port
                  (lambda (out)
                    (let loop ()
                      (let ([c (peek-char p)])
                        (when (and (char? c) (memv c (string->list "+-0123456789.eE")))
                          (put-char out (read-char p))
                          (loop))))))])
      (or (string->number text) (error 'string->json "bad number" text)))))
