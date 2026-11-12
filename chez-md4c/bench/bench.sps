(import (chezscheme) (md4c))

(define (repeated n s) (apply string-append (make-list n s)))

(define chat-reply
  (string-append
    "# Parsing Markdown\n\n"
    "Here is **how** to parse *Markdown* with `md4c`, as [the docs](https://github.com/mity/md4c) say.\n"
    "It is fast &amp; safe: raw <b>HTML</b> stays text.\n\n"
    "## Steps\n\n"
    "1. Read the input.\n2. Call `md_parse` with the callbacks.\n3. Walk the events.\n\n"
    "- [x] tables\n- [ ] footnotes\n- plain item with a link to https://example.org/path?q=1\n\n"
    "```scheme\n(define (f x)\n  (* x x))\n(display (f 12))\n```\n\n"
    "| Name | Value | Note |\n|:-----|------:|:----:|\n| a | 1 | first |\n| b | 22 | *second* |\n| c | 333 | `third` |\n\n"
    "> A quoted paragraph with ~~strikethrough~~ and an image ![logo](https://example.org/logo.png \"Logo\").\n> It continues here.\n\n"
    "Some more text with _emphasis_, __strong__, and a hard break  \nafter it. Entities: &copy; &#65; &#x42;.\n\n"
    "---\n\n"
    "Final paragraph: \xe9; \x65e5;\x672c; \x1f600; and <https://autolink.example>.\n"))

(define unicode
  (repeated 2000 "\xe9; \x65e5;\x672c; \x1f600; *\xfc;* `\xdf;` [\xf1;](https://x.org)\n\n- \xf6;\n\n| \x3b1; | \x3b2; |\n|---|---|\n| \x3b3; | \x3b4; |\n\n"))

(define huge
  (repeated 20000 "## Heading\n\nSome *text* with `code` and [a link](https://x.org).\n\n- one\n- two\n\n```\ncode\n```\n\n"))

(define nested
  (let ([depth 5000])
    (string-append (make-string depth #\>) " deep\n\n"
                   (make-string depth #\[) "x" (make-string depth #\]) "\n\n"
                   (make-string depth #\*) "x" (make-string depth #\*) "\n")))

(define md4c-events
  (let ([parse (foreign-procedure "chez_md4c_parse" (u8* size_t) uptr)]
        [size (foreign-procedure "chez_md4c_size" (uptr) size_t)]
        [take! (foreign-procedure "chez_md4c_take" (uptr u8*) void)])
    (lambda (bytes)
      (let* ([parsed (parse bytes (bytevector-length bytes))]
             [events (make-bytevector (size parsed))])
        (take! parsed events)
        events))))

;; The fastest of five half-second batches, since other processes only ever
;; slow a batch down.
(define (measure thunk)
  (define (batch)
    (collect (collect-maximum-generation))
    (let ([start (real-time)] [before (statistics)])
      (let loop ([n 1])
        (thunk)
        (let ([elapsed (- (real-time) start)])
          (if (< elapsed 500)
              (loop (+ n 1))
              (values (/ (* 1000. elapsed) n)
                      (/ (sstats-bytes (sstats-difference (statistics) before)) n)))))))
  (thunk)
  (let loop ([i 0] [best +inf.0] [bytes 0])
    (if (= i 5)
        (values best bytes)
        (let-values ([(time bytes) (batch)])
          (loop (+ i 1) (min best time) bytes)))))

(define (bench name document)
  (let ([bytes (string->utf8 document)])
    (let-values ([(parse-time parse-bytes) (measure (lambda () (markdown-parse document)))]
                 [(md4c-time _) (measure (lambda () (md4c-events bytes)))])
      (printf "~8a ~8d chars   markdown-parse ~9,1f us ~11,0f bytes   md4c alone ~9,1f us\n"
        name (string-length document) parse-time parse-bytes md4c-time))))

(bench "chat" chat-reply)
(bench "unicode" unicode)
(bench "huge" huge)
(bench "nested" nested)
