(import (chezscheme) (md4c) (check) (support))

(test "the records of each kind of block"
  (let ([blocks (markdown-parse "# One\n\nText\n\n```scheme extra\n(x)\n```\n\n> quoted\n\n3. a\n4. b\n\n- [x] done\n- [ ] open\n- plain\n\n| a | b |\n|:-:|--:|\n| 1 | 2 |\n\n***\n\n    indented\n")])
    (check-equal? 9 (length blocks))
    (let ([h (list-ref blocks 0)] [p (list-ref blocks 1)] [code (list-ref blocks 2)]
          [blockquote (list-ref blocks 3)] [ordered (list-ref blocks 4)] [tasks (list-ref blocks 5)]
          [table (list-ref blocks 6)] [rule (list-ref blocks 7)] [indented (list-ref blocks 8)])
      (check-equal? 1 (markdown-heading-level h))
      (check-equal? "One" (markdown-inlines-text (markdown-heading-inlines h)))
      (check-equal? "Text" (markdown-inlines-text (markdown-paragraph-inlines p)))
      (check-equal? #t (markdown-code-block-fenced? code))
      (check-equal? "scheme extra" (markdown-code-block-info code))
      (check-equal? "scheme" (markdown-code-block-language code))
      (check-equal? "(x)\n" (markdown-code-block-text code))
      (check-equal? 1 (length (markdown-quote-blocks blockquote)))
      (check (markdown-paragraph? (car (markdown-quote-blocks blockquote))))
      (check-equal? #t (markdown-list-ordered? ordered))
      (check-equal? 3 (markdown-list-first-number ordered))
      (check-equal? #t (markdown-list-tight? ordered))
      (check-equal? #f (markdown-list-ordered? tasks))
      (check-equal? 1 (markdown-list-first-number tasks))
      (check-equal? '(#t #t #f) (map markdown-item-task? (markdown-list-items tasks)))
      (check-equal? '(#t #f #f) (map markdown-item-checked? (markdown-list-items tasks)))
      (check-equal? '(center right) (markdown-table-alignments table))
      (check-equal? '("a" "b") (map markdown-inlines-text (markdown-table-header table)))
      (check-equal? '(("1" "2"))
        (map (lambda (row) (map markdown-inlines-text row)) (markdown-table-rows table)))
      (check (markdown-rule? rule))
      (check-equal? #f (markdown-code-block-fenced? indented))
      (check-equal? "" (markdown-code-block-info indented))
      (check-equal? "" (markdown-code-block-language indented))
      (check-equal? "indented\n" (markdown-code-block-text indented))
      (check (for-all markdown-block? blocks)))))

(test "list items always hold blocks"
  (let ([tight (car (markdown-parse "- a\n- b *c*\n"))]
        [loose (car (markdown-parse "- a\n\n- b\n\n  > c\n"))])
    (check-equal? #t (markdown-list-tight? tight))
    (check (for-all (lambda (item) (for-all markdown-paragraph? (markdown-item-blocks item)))
                    (markdown-list-items tight)))
    (check-equal? '("a" "b c")
      (map (lambda (item) (markdown-inlines-text (markdown-paragraph-inlines (car (markdown-item-blocks item)))))
           (markdown-list-items tight)))
    (check-equal? #f (markdown-list-tight? loose))
    (check-equal? '(1 2) (map (lambda (item) (length (markdown-item-blocks item)))
                              (markdown-list-items loose)))
    (check (markdown-quote? (cadr (markdown-item-blocks (cadr (markdown-list-items loose))))))))

(test "a table without body rows, and one with short and long rows"
  (let ([empty (car (markdown-parse "| a | b |\n|---|---|\n"))]
        [ragged (car (markdown-parse "| a | b |\n|---|---|\n| 1 |\n| 1 | 2 | 3 |\n"))])
    (check-equal? '(#f #f) (markdown-table-alignments empty))
    (check-equal? '() (markdown-table-rows empty))
    (check-equal? '(("1" "") ("1" "2"))
      (map (lambda (row) (map markdown-inlines-text row)) (markdown-table-rows ragged)))))

(test "inline records: spans, links, images, breaks"
  (let ([inlines (markdown-paragraph-inlines
                   (car (markdown-parse "**b** *e* ~~s~~ `c` [l](https://x.org \"T\") <https://y.org> ![i](https://i.org/i.png 'I')\nsoft  \nhard")))])
    (check-equal? '(strong emphasis strikethrough code)
      (map markdown-span-kind (filter markdown-span? inlines)))
    (let ([links (filter markdown-link? inlines)])
      (check-equal? '("https://x.org" "https://y.org") (map markdown-link-href links))
      (check-equal? '("T" "") (map markdown-link-title links))
      (check-equal? '(#f #t) (map markdown-link-autolink? links)))
    (let ([image (find markdown-image? inlines)])
      (check-equal? "https://i.org/i.png" (markdown-image-src image))
      (check-equal? "I" (markdown-image-title image))
      (check-equal? "i" (markdown-inlines-text (markdown-image-inlines image))))
    (check-equal? '(#f #t) (map markdown-line-break-hard? (filter markdown-line-break? inlines)))
    (check (for-all markdown-inline? inlines))
    (check-equal? "b e s c l https://y.org i soft\nhard" (markdown-inlines-text inlines))))

(test "an empty or blank document has no blocks"
  (check-equal? '() (markdown-parse ""))
  (check-equal? '() (markdown-parse " \n\t\n\n")))

(test "line endings: CR LF and lone CR"
  (let ([crlf "# A\r\n\r\nb\r\nc\r\n"] [cr "# A\r\rb\rc\r"])
    (check-equal? '("# A" "b\r\nc") (map (lambda (b) (source crlf b)) (markdown-parse crlf)))
    (check-equal? '("# A" "b\rc") (map (lambda (b) (source cr b)) (markdown-parse cr)))))

(test "arguments are checked"
  (check-raises assertion-violation? (markdown-parse 'x))
  (check-raises assertion-violation? (markdown-parse (string->utf8 "x")))
  (check-raises assertion-violation? (markdown-inlines-text "x"))
  (check-raises assertion-violation? (markdown-inlines-text '(1)))
  (check-raises assertion-violation? (markdown-inlines-text (markdown-parse "x"))))

(test "a parse returns fresh records"
  (let ([a (markdown-parse "x *y*")] [b (markdown-parse "x *y*")])
    (check-equal? (blocks->datum a) (blocks->datum b))
    (check (not (eq? (car a) (car b))))))

(test-exit)
