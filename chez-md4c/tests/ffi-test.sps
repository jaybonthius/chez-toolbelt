(import (chezscheme) (md4c) (check) (support))

(test "the shared object offers only the C half's entries"
  (check (for-all foreign-entry? '("chez_md4c_parse" "chez_md4c_size" "chez_md4c_take")))
  (check-equal? '() (filter foreign-entry? '("md_parse" "entity_lookup" "md_html"))))

(test "long strings cross intact"
  (let* ([address (string-append "https://x.org/" (make-string 1000000 #\a))]
         [title (apply string-append (make-list 100000 "&amp;\xe9;"))]
         [link (car (links (string-append "[a](" address " \"" title "\")")))])
    (check-equal? address (markdown-link-href link))
    (check-equal? (apply string-append (make-list 100000 "&\xe9;")) (markdown-link-title link)))
  (let* ([lines (apply string-append (make-list 100000 "x \xe9;\x1f600;\n"))]
         [code (car (markdown-parse (string-append "```\n" lines "```\n")))])
    (check-equal? lines (markdown-code-block-text code))))

(test "a document of nothing but events md4c makes up"
  (let ([inlines (markdown-paragraph-inlines (car (markdown-parse (make-string 100000 #\nul))))])
    (check-equal? 100000 (length inlines))
    (check (for-all (lambda (i) (and (equal? "\xfffd;" (markdown-text-string i))
                                     (not (markdown-inline-start i))))
                    inlines))))

(test "every offset is in characters, past four-byte UTF-8"
  (let* ([document (string-append (make-string 100000 #\x1f600) " *a* [b](https://c.org)")]
         [inlines (all-inlines (markdown-parse document))])
    (check-equal? '("*a*" "a" " " "[b](https://c.org)" "b")
      (map (lambda (i) (source document i)) (filter markdown-inline-start (cdr inlines))))))

(test-exit)
