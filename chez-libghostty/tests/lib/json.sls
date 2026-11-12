(library (json)
  (export read-json json-ref)
  (import (chezscheme))

  (define (json-ref object . keys)
    (fold-left (lambda (value key) (and value (cond [(assoc key value) => cdr] [else #f])))
      object keys))

  (define (read-json p)
    (define (skip!)
      (let ([c (peek-char p)])
        (when (and (char? c) (char-whitespace? c)) (read-char p) (skip!))))
    (define (expect! c)
      (unless (eqv? (read-char p) c) (error 'read-json "expected a character" c)))
    (define (read-string*)
      (expect! #\")
      (let ([out (open-output-string)])
        (let loop ()
          (let ([c (read-char p)])
            (cond
              [(char=? c #\") (get-output-string out)]
              [(char=? c #\\)
               (let ([e (read-char p)])
                 (case e
                   [(#\n) (write-char #\newline out)]
                   [(#\t) (write-char #\tab out)]
                   [(#\u) (write-char (integer->char (string->number (get-string-n p 4) 16)) out)]
                   [else (write-char e out)])
                 (loop))]
              [else (write-char c out) (loop)])))))
    (define (read-word*)
      (let loop ([acc '()])
        (let ([c (peek-char p)])
          (if (and (char? c) (or (char-alphabetic? c) (char-numeric? c) (memv c '(#\- #\+ #\.))))
              (loop (cons (read-char p) acc))
              (list->string (reverse acc))))))
    (define (read-sequence close read-item)
      (read-char p)
      (skip!)
      (if (eqv? (peek-char p) close)
          (begin (read-char p) '())
          (let loop ([acc (list (read-item))])
            (skip!)
            (let ([c (read-char p)])
              (cond
                [(eqv? c close) (reverse acc)]
                [(eqv? c #\,) (skip!) (loop (cons (read-item) acc))]
                [else (error 'read-json "expected , or a closing bracket" c)])))))
    (define (read-value)
      (skip!)
      (let ([c (peek-char p)])
        (cond
          [(eqv? c #\{)
           (read-sequence #\}
             (lambda ()
               (let ([key (read-string*)])
                 (skip!)
                 (expect! #\:)
                 (cons key (read-value)))))]
          [(eqv? c #\[) (read-sequence #\] read-value)]
          [(eqv? c #\") (read-string*)]
          [else
           (let ([word (read-word*)])
             (cond
               [(string=? word "true") #t]
               [(string=? word "false") #f]
               [(string=? word "null") 'null]
               [(string->number word)]
               [else (error 'read-json "unexpected text" word)]))])))
    (read-value)))
