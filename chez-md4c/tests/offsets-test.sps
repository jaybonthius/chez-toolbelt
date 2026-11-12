(import (chezscheme) (md4c) (check) (support))

(test "text offsets are characters, also past multi-byte UTF-8"
  (let ([document "é 日本 😀 *ü* `ß` [ñ](https://x.org)\n\n- ö\n\n| α | β |\n|---|---|\n| γ | δ |\n"])
    (for-each (lambda (t) (check-equal? (markdown-text-string t) (source document t)))
              (texts document))
    (check-equal? '("é 日本 😀 " "ü" " " "ß" " " "ñ" "ö" "α" "β" "γ" "δ")
      (map markdown-text-string (texts document)))))

(test "an entity's run covers the entity"
  (let* ([document "a &amp; b"] [runs (texts document)])
    (check-equal? '("a " "&" " b") (map markdown-text-string runs))
    (check-equal? "&amp;" (source document (cadr runs)))))

(test "text md4c made up has no offsets"
  (let ([inlines (markdown-paragraph-inlines (car (markdown-parse (string #\a #\nul #\b #\newline #\c))))])
    (check-equal? '((0 1) (#f #f) (2 3) (#f #f) (4 5))
      (map (lambda (i) (list (markdown-inline-start i) (markdown-inline-end i))) inlines))))

(test "spans and links cover their delimiters"
  (let ([document "x **b** *e* _u_ ~~s~~ ~t~ ``c`d`` ` p ` [l](https://x.org \"t\") <https://y.org> https://z.org ![i](https://i.org) é**ü** ***n*** [r][ref] [s]\n\n[ref]: https://r.org\n[s]: https://s.org\n"])
    (check-equal?
      '("**b**" "*e*" "_u_" "~~s~~" "~t~" "``c`d``" "` p `" "[l](https://x.org \"t\")"
        "<https://y.org>" "https://z.org" "![i](https://i.org)" "**ü**" "***n***" "**n**" "[r][ref]" "[s]")
      (map (lambda (i) (source document i))
           (filter (lambda (i) (not (markdown-text? i))) (all-inlines (markdown-parse document)))))))

(test "a link's destination and title end where md4c's do"
  (for-each
    (lambda (document link-source)
      (check-equal? (list document link-source)
        (list document (source document (car (links document))))))
    '("[a](https://x.org/(p) \"t (u\") z"
      "[a](<https://x.org/a)b> 't\\'') z"
      "[a](  https://x.org\n  (t)  ) z"
      "> [a](\n> https://x.org\n> \"t\") z"
      "[a] (b)\n\n[a]: https://x.org"
      "[a](b c) z\n\n[a]: https://x.org"
      "**[a](https://x.org)** z")
    '("[a](https://x.org/(p) \"t (u\")"
      "[a](<https://x.org/a)b> 't\\'')"
      "[a](  https://x.org\n  (t)  )"
      "[a](\n> https://x.org\n> \"t\")"
      "[a]"
      "[a]"
      "[a](https://x.org)")))

(test "an escape at the start of a span, link or image is inside it"
  (for-each
    (lambda (document)
      (check-equal? document
        (source document (car (filter (lambda (i) (not (markdown-text? i)))
                                      (all-inlines (markdown-parse document)))))))
    '("*\\*a*" "[\\*a](https://x.org)" "![\\*](https://x.org)" "[\\[1\\]](https://x.org)"))
  (let ([document "# \\*a\n---\n"])
    (check-equal? '("# \\*a" "---") (map (lambda (b) (source document b)) (markdown-parse document)))))

(test "a block ends after white space md4c keeps as text"
  (for-each
    (lambda (document)
      (check-equal? (list document) (map (lambda (b) (source document b)) (markdown-parse document))))
    '("a\xa0;" "> a\x3000;" "a\f" "a\v" "- a\x2028;")))

(test "an emphasis around a refused link covers the link"
  (let ([document "**[a](javascript:x)** z"])
    (check-equal? '("**[a](javascript:x)**")
      (map (lambda (i) (source document i)) (filter markdown-span? (all-inlines (markdown-parse document)))))))

(test "blocks run from their first line to the next block, without trailing space"
  (let* ([document "# Hé\n\npara\ngraph\n\n```js\nx\n```\n---\n- a\n- b\n\n  more\n\n> q\n> r\n\n    indented\n\n| a |\n|---|\n| 1 |\n\n- \n\n***\n"]
         [blocks (markdown-parse document)])
    (check-equal?
      '("# Hé" "para\ngraph" "```js\nx\n```" "---" "- a\n- b\n\n  more" "> q\n> r" "    indented"
        "| a |\n|---|\n| 1 |" "-" "***")
      (map (lambda (b) (source document b)) blocks))
    (check-equal? '("- a" "- b\n\n  more")
      (map (lambda (b) (source document b)) (markdown-list-items (list-ref blocks 4))))))

(test "empty blocks start after the block before them"
  (for-each
    (lambda (document sources)
      (check-equal? (list document sources)
        (list document (map (lambda (b) (source document b)) (markdown-parse document)))))
    '("```\n```\n***\n\n```\nx\n\n```\n- \n-\n> \n"
      "Foo\n---\n***\n\nBar\nbaz\n===\n- \n"
      "> a\n>\n> ---\n>\n> > \n\n---\n"
      "- a\n-\n\n  -\n***\n"
      "| a |\n|---|\n| |\n| |\n***\n"
      "| |\n|-|\n| |\n***\n"
      "```\n\n\nx\n```\n***\n"
      "# \n#\n***\n"
      ">\n\n>\n(")
    '(("```\n```" "***" "```\nx\n\n```" "- \n-" ">")
      ("Foo\n---" "***" "Bar\nbaz\n===" "-")
      ("> a\n>\n> ---\n>\n> >" "---")
      ("- a\n-\n\n  -" "***")
      ("| a |\n|---|\n| |\n| |" "***")
      ("| |\n|-|\n| |" "***")
      ("```\n\n\nx\n```" "***")
      ("#" "#" "***")
      (">" ">" "(")))
  (let* ([document "> a\n>\n> ---\n>\n> > \n"]
         [inner (markdown-quote-blocks (car (markdown-parse document)))])
    (check-equal? '("> a" "> ---" "> >") (map (lambda (b) (source document b)) inner))))

(test "a lazy line that looks blank inside a quote stays in its paragraph"
  (let* ([document ">g\n    >"]
         [paragraph (car (markdown-quote-blocks (car (markdown-parse document))))])
    (check-equal? ">g\n    >" (source document paragraph))
    (check-equal? "g >" (markdown-inlines-text (markdown-paragraph-inlines paragraph)))))

(test "nested blocks lie inside their containers"
  (let* ([document "> - a\n>   > b\n>\n>   c\n> - ```\n>   d\n>   ```\n\n1. x\n   1. y\n      - z\n"]
         [blocks (markdown-parse document)])
    (let walk ([blocks blocks] [start 0] [end (string-length document)])
      (for-each
        (lambda (b)
          (check (<= start (markdown-block-start b) (markdown-block-end b) end))
          (walk (cond
                  [(markdown-quote? b) (markdown-quote-blocks b)]
                  [(markdown-list? b) (markdown-list-items b)]
                  [(markdown-item? b) (markdown-item-blocks b)]
                  [else '()])
                (markdown-block-start b) (markdown-block-end b)))
        blocks))))

(test "a document being typed or streamed parses at every length"
  (let ([document "# Title\n\nSome **bold** and `code` with [a link](https://x.org \"t\").\n\n```scheme\n(define x 1)\n```\n\n| a | b |\n|---|---|\n| 1 | 2 |\n\n> - [x] é *ü* ![i](https://i.org)\n"])
    (let loop ([n 0])
      (when (<= n (string-length document))
        (let* ([prefix (substring document 0 n)] [blocks (markdown-parse prefix)])
          (for-each
            (lambda (b) (check (<= 0 (markdown-block-start b) (markdown-block-end b) n)))
            (all-blocks blocks))
          (for-each
            (lambda (i)
              (when (markdown-inline-start i)
                (check (<= 0 (markdown-inline-start i) (markdown-inline-end i) n))))
            (all-inlines blocks)))
        (loop (+ n 1))))))

(test-exit)
