(import (chezscheme) (md4c) (check) (html) (support))

;; Raw HTML is read as text and links to anything but http:, https: and
;; mailto: are their text, so samples with raw HTML or such links render
;; differently from md4c's HTML; they are only parsed.
(define (raw-html? markdown)
  (let ([n (string-length markdown)])
    (let loop ([i 0])
      (and (< (+ i 1) n)
           (or (and (char=? (string-ref markdown i) #\<)
                    (let ([c (string-ref markdown (+ i 1))])
                      (or (char-alphabetic? c) (memv c '(#\/ #\! #\?)))))
               (loop (+ i 1)))))))

(define (refused-address? html)
  (define (refused-after? attribute allowed)
    (let ([n (string-length attribute)])
      (let loop ([i 0])
        (and (<= (+ i n) (string-length html))
             (or (and (string=? (substring html i (+ i n)) attribute)
                      (not (exists (lambda (scheme)
                                     (let ([end (+ i n (string-length scheme))])
                                       (and (<= end (string-length html))
                                            (string=? (substring html (+ i n) end) scheme))))
                                   allowed)))
                 (loop (+ i 1)))))))
  (or (refused-after? "href=\"" '("http:" "https:" "mailto:"))
      (refused-after? "src=\"" '("http:" "https:"))))

;; The other samples whose HTML differs from md4c's, by file and number
;; (from 1): all four are autolinks, which are always on.
(define differences
  '(("spec-permissive-autolinks.txt" 30)
    ("spec.txt" 608) ("spec.txt" 611) ("spec.txt" 612)))

;; As md4c's spec runner, which normalizes HTML before comparing.
(define (normalized html)
  (let-values ([(port result) (open-string-output-port)])
    (let loop ([i 0])
      (cond
        [(= i (string-length html)) (result)]
        [(char=? (string-ref html i) #\newline) (loop (+ i 1))]
        [(and (<= (+ i 3) (string-length html)) (string=? (substring html i (+ i 3)) " />"))
         (put-char port #\>)
         (loop (+ i 3))]
        [(and (<= (+ i 6) (string-length html)) (string=? (substring html i (+ i 6)) "&#x27;"))
         (put-char port #\')
         (loop (+ i 6))]
        [else (put-char port (string-ref html i)) (loop (+ i 1))]))))

(define (check-file file)
  (let loop ([examples (spec-examples (string-append "vendor/md4c/test/" file))]
             [number 1] [compared 0])
    (if (null? examples)
        compared
        (let* ([example (car examples)]
               [markdown (car example)]
               [expected (cadr example)]
               [html (blocks->html (markdown-parse markdown))])
          (if (or (raw-html? markdown) (refused-address? expected))
              (loop (cdr examples) (+ number 1) compared)
              (let ([same? (string=? (normalized html) (normalized expected))]
                    [expected-same? (not (member (list file number) differences))])
                (check-equal? (list file number expected-same?) (list file number same?))
                (loop (cdr examples) (+ number 1) (+ compared 1))))))))

;; How many samples of each file are compared, so that a change in which
;; are skipped shows.
(for-each
  (lambda (file compared)
    (test file (check-equal? compared (check-file file))))
  '("spec.txt" "spec-tables.txt" "spec-tasklists.txt" "spec-strikethrough.txt"
    "spec-permissive-autolinks.txt")
  '(433 11 5 5 29))

(test-exit)
