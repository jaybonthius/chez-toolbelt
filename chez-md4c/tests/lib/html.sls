(library (html)
  (export blocks->html spec-examples)
  (import (chezscheme) (md4c))

  (define (put-escaped port s)
    (string-for-each
      (lambda (c)
        (case c
          [(#\&) (put-string port "&amp;")]
          [(#\<) (put-string port "&lt;")]
          [(#\>) (put-string port "&gt;")]
          [(#\") (put-string port "&quot;")]
          [(#\') (put-string port "&#x27;")]
          [else (put-char port c)]))
      s))

  ;; As md4c-html's render_url_escaped.
  (define (put-url-escaped port s)
    (bytevector-for-each
      (lambda (b)
        (let ([c (integer->char b)])
          (cond
            [(char=? c #\&) (put-string port "&amp;")]
            [(and (fx< b 128)
                  (or (char-alphabetic? c) (char-numeric? c)
                      (memv c (string->list "~-_.+!*(),%#@?=;:/$"))))
             (put-char port c)]
            [else (put-string port (format "%~2,'0X" b))])))
      (string->utf8 s)))

  (define (bytevector-for-each f bv)
    (let loop ([i 0])
      (when (fx< i (bytevector-length bv))
        (f (bytevector-u8-ref bv i))
        (loop (fx+ i 1)))))

  (define (put-inlines port inlines)
    (for-each
      (lambda (inline)
        (cond
          [(markdown-text? inline) (put-escaped port (markdown-text-string inline))]
          [(markdown-line-break? inline)
           (put-string port (if (markdown-line-break-hard? inline) "<br>\n" "\n"))]
          [(markdown-span? inline)
           (let ([tag (case (markdown-span-kind inline)
                        [(strong) "strong"]
                        [(emphasis) "em"]
                        [(strikethrough) "del"]
                        [(code) "code"])])
             (put-string port (string-append "<" tag ">"))
             (put-inlines port (markdown-span-inlines inline))
             (put-string port (string-append "</" tag ">")))]
          [(markdown-link? inline)
           (put-string port "<a href=\"")
           (put-url-escaped port (markdown-link-href inline))
           (put-string port "\"")
           (put-title port (markdown-link-title inline))
           (put-string port ">")
           (put-inlines port (markdown-link-inlines inline))
           (put-string port "</a>")]
          [(markdown-image? inline)
           (put-string port "<img src=\"")
           (put-url-escaped port (markdown-image-src inline))
           (put-string port "\" alt=\"")
           (put-escaped port (markdown-inlines-text (markdown-image-inlines inline)))
           (put-string port "\"")
           (put-title port (markdown-image-title inline))
           (put-string port ">")]))
      inlines))

  (define (put-title port title)
    (unless (string=? title "")
      (put-string port " title=\"")
      (put-escaped port title)
      (put-string port "\"")))

  (define (put-cell port tag alignment cell)
    (put-string port (string-append "<" tag))
    (when alignment
      (put-string port (string-append " align=\"" (symbol->string alignment) "\"")))
    (put-string port ">")
    (put-inlines port cell)
    (put-string port (string-append "</" tag ">\n")))

  (define (put-block port block tight?)
    (cond
      [(markdown-paragraph? block)
       (unless tight? (put-string port "<p>"))
       (put-inlines port (markdown-paragraph-inlines block))
       (unless tight? (put-string port "</p>\n"))]
      [(markdown-heading? block)
       (let ([n (number->string (markdown-heading-level block))])
         (put-string port (string-append "<h" n ">"))
         (put-inlines port (markdown-heading-inlines block))
         (put-string port (string-append "</h" n ">\n")))]
      [(markdown-code-block? block)
       (let ([language (markdown-code-block-language block)])
         (if (string=? language "")
             (put-string port "<pre><code>")
             (begin
               (put-string port "<pre><code class=\"language-")
               (put-escaped port language)
               (put-string port "\">")))
         (put-escaped port (markdown-code-block-text block))
         (put-string port "</code></pre>\n"))]
      [(markdown-quote? block)
       (put-string port "<blockquote>\n")
       (for-each (lambda (b) (put-block port b #f)) (markdown-quote-blocks block))
       (put-string port "</blockquote>\n")]
      [(markdown-list? block)
       (let ([ordered? (markdown-list-ordered? block)]
             [tight? (markdown-list-tight? block)])
         (put-string port
           (cond
             [(not ordered?) "<ul>\n"]
             [(= (markdown-list-first-number block) 1) "<ol>\n"]
             [else (format "<ol start=\"~a\">\n" (markdown-list-first-number block))]))
         (for-each
           (lambda (item)
             (put-string port
               (if (markdown-item-task? item)
                   (string-append
                     "<li class=\"task-list-item\"><input type=\"checkbox\" class=\"task-list-item-checkbox\" disabled"
                     (if (markdown-item-checked? item) " checked" "")
                     ">")
                   "<li>"))
             (for-each (lambda (b) (put-block port b tight?)) (markdown-item-blocks item))
             (put-string port "</li>\n"))
           (markdown-list-items block))
         (put-string port (if ordered? "</ol>\n" "</ul>\n")))]
      [(markdown-table? block)
       (let ([alignments (markdown-table-alignments block)])
         (put-string port "<table>\n<thead>\n<tr>\n")
         (for-each (lambda (a cell) (put-cell port "th" a cell)) alignments (markdown-table-header block))
         (put-string port "</tr>\n</thead>\n")
         (unless (null? (markdown-table-rows block))
           (put-string port "<tbody>\n")
           (for-each
             (lambda (row)
               (put-string port "<tr>\n")
               (for-each (lambda (a cell) (put-cell port "td" a cell)) alignments row)
               (put-string port "</tr>\n"))
             (markdown-table-rows block))
           (put-string port "</tbody>\n"))
         (put-string port "</table>\n"))]
      [(markdown-rule? block) (put-string port "<hr>\n")]))

  (define (blocks->html blocks)
    (let-values ([(port html) (open-string-output-port)])
      (for-each (lambda (b) (put-block port b #f)) blocks)
      (html)))

  ;; The file writes a tab as an arrow.
  (define (spec-examples path)
    (define fence "````````````````````````````````")
    (define (joined lines)
      (list->string
        (map (lambda (c) (if (char=? c #\x2192) #\tab c))
             (string->list (apply string-append (map (lambda (l) (string-append l "\n")) lines))))))
    (let ([lines (call-with-input-file path
                   (lambda (port)
                     (let loop ([lines '()])
                       (let ([line (get-line port)])
                         (if (eof-object? line) (reverse lines) (loop (cons line lines)))))))])
      (let loop ([lines lines] [number 1] [examples '()])
        (cond
          [(null? lines) (reverse examples)]
          [(string=? (car lines) (string-append fence " example"))
           (let collect ([rest (cdr lines)] [part '()] [parts '()] [n (+ number 1)])
             (let ([line (car rest)])
               (cond
                 [(string=? line fence)
                  (let ([parts (reverse (cons (reverse part) parts))])
                    (loop (cdr rest) (+ n 1)
                          (cons (list (joined (car parts)) (joined (cadr parts)) number) examples)))]
                 [(string=? line ".") (collect (cdr rest) '() (cons (reverse part) parts) (+ n 1))]
                 [else (collect (cdr rest) (cons line part) parts (+ n 1))])))]
          [else (loop (cdr lines) (+ number 1) examples)]))))
  )
