(import (chezscheme) (md4c) (check) (support))

(define pieces
  '#("*" "**" "_" "`" "```" "~" "~~" "[" "]" "(" ")" "![" "](" "<" ">" "> " "#" "# " "-" "- " "---"
     "|" "|-|" ":" "1. " "- [x] " "- [ ] " "&amp;" "&#" "&#x" ";" "&ngE;" "\\" "\n" "\n\n" "\r" "\r\n"
     " " "  " "\t" "    " "a" "bc" "é" "日本" "😀" "\x0;" "https://x.org" "www.y.org" "a@b.org"
     "javascript:" "\"" "'" "=" "===" "\xa0;" "\x3000;" "\f" "\v" "+ " "2) " "~~~" "\\*" "\\_"
     "\\[" "\\`" ">>" "    > "))

(define (random-document length)
  (let-values ([(port document) (open-string-output-port)])
    (do ([i 0 (+ i 1)]) ((= i length) (document))
      (put-string port (vector-ref pieces (random (vector-length pieces)))))))

(define (followable? address mailto?)
  (define (prefix? p)
    (and (<= (string-length p) (string-length address))
         (string-ci=? p (substring address 0 (string-length p)))))
  (or (prefix? "http:") (prefix? "https:") (and mailto? (prefix? "mailto:"))))

(define (broken document)
  (define problems '())
  (define (problem! what x) (set! problems (cons (list what x) problems)))
  (define n (string-length document))
  (define (check-inlines inlines start end)
    (for-each
      (lambda (i)
        (let ([s (markdown-inline-start i)] [e (markdown-inline-end i)])
          (when s
            (unless (<= start s e end) (problem! 'inline-outside-parent (list s e start end))))
          (when (markdown-text? i)
            (when (and s (not (string=? (substring document s e) (markdown-text-string i)))
                       (not (char=? (string-ref document s) #\&)))
              (problem! 'text-not-its-source (list s e (markdown-text-string i)))))
          (when (markdown-link? i)
            (unless (followable? (markdown-link-href i) #t) (problem! 'link-refused (markdown-link-href i))))
          (when (markdown-image? i)
            (unless (followable? (markdown-image-src i) #f) (problem! 'image-refused (markdown-image-src i))))
          (let ([children (cond
                            [(markdown-span? i) (markdown-span-inlines i)]
                            [(markdown-link? i) (markdown-link-inlines i)]
                            [(markdown-image? i) (markdown-image-inlines i)]
                            [else '()])])
            (if s (check-inlines children s e) (check-inlines children start end)))))
      inlines)
    (markdown-inlines-text inlines))
  (define (check-blocks blocks start end)
    (let loop ([blocks blocks] [previous-end start])
      (unless (null? blocks)
        (let* ([b (car blocks)] [s (markdown-block-start b)] [e (markdown-block-end b)])
          (unless (<= start previous-end s e end)
            (problem! 'block-out-of-order (list previous-end s e start end)))
          (cond
            [(markdown-paragraph? b) (check-inlines (markdown-paragraph-inlines b) s e)]
            [(markdown-heading? b) (check-inlines (markdown-heading-inlines b) s e)]
            [(markdown-table? b)
             (for-each (lambda (cell) (check-inlines cell s e))
                       (apply append (markdown-table-header b) (markdown-table-rows b)))]
            [(markdown-quote? b) (check-blocks (markdown-quote-blocks b) s e)]
            [(markdown-list? b) (check-blocks (markdown-list-items b) s e)]
            [(markdown-item? b) (check-blocks (markdown-item-blocks b) s e)])
          (loop (cdr blocks) e)))))
  (let ([blocks (markdown-parse document)])
    (check-blocks blocks 0 n)
    (unless (equal? (blocks->datum blocks) (blocks->datum (markdown-parse document)))
      (problem! 'parses-differ document)))
  problems)

(test "random documents keep every invariant"
  (random-seed 20261111)
  (do ([i 0 (+ i 1)]) ((= i 20000))
    (let ([document (random-document (+ 1 (random 40)))])
      (check-equal? (list document '()) (list document (broken document))))))

(test-exit)
