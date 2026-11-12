(import (chezscheme) (md4c) (check) (support))

(test "raw HTML is text, never markup"
  (for-each
    (lambda (document)
      (check (for-all markdown-paragraph? (markdown-parse document)))
      (check (string-contains? (plain document) "<")))
    '("<script>alert(1)</script>"
      "<img src=x onerror=alert(1)>"
      "<div onclick=\"alert(1)\">\nhi\n</div>"
      "<iframe src=\"https://evil.example\"></iframe>"
      "a <span style=\"x\">b</span> c"
      "<!-- comment --> <?php x ?> <![CDATA[ y ]]>"))
  (check-equal? "<script>alert(1)</script>" (plain "<script>alert(1)</script>")))

(test "links that are not http, https or mailto are their text"
  (for-each
    (lambda (document) (check-equal? (list document '()) (list document (links document))))
    '("[a](javascript:alert(1))"
      "[a](JavaScript:alert(1))"
      "[a](java&#115;cript:alert(1))"
      "[a](javascript&colon;alert(1))"
      "[a](&#x6A;avascript:alert(1))"
      "[a](\\javascript:alert(1))"
      "[a]( javascript:alert(1) )"
      "[a](<javascript:alert(1)>)"
      "[a](data:text/html;base64,PHNjcmlwdD4=)"
      "[a](vbscript:msgbox(1))"
      "[a](file:///etc/passwd)"
      "[a](//evil.example/x)"
      "[a](/relative) [b](#frag) [c](x.html)"
      "<javascript:alert(1)>"
      "[a][r]\n\n[r]: javascript:alert(1)"
      "![x](javascript:alert(1))"
      "![x](mailto:a@b.org)"
      "![x](data:image/png;base64,AAAA)"))
  (check-equal? "click me" (plain "[click **me**](javascript:alert(1))"))
  (check-equal? '(1) (map markdown-inline-start (texts "[click](javascript:x)"))))

(test "http, https and mailto links stay links"
  (check-equal?
    '("https://a.org" "HTTP://B.ORG" "mailto:c@d.org" "mailto:e@f.org" "http://www.g.org"
      "https://i.org/i.png")
    (map (lambda (l) (if (markdown-link? l) (markdown-link-href l) (markdown-image-src l)))
         (links "[a](https://a.org) [b](HTTP://B.ORG) <mailto:c@d.org> e@f.org www.g.org ![i](https://i.org/i.png)"))))

(test "a link and the image inside it are judged apart"
  (check-equal? '("https://i.org/i.png")
    (map markdown-image-src (links "[![x](https://i.org/i.png)](javascript:alert(1))"))))

(test "a refused image inside a kept link leaves its text in the link"
  (let ([l (car (links "[![x](javascript:1) y](https://a.org)"))])
    (check (markdown-link? l))
    (check-equal? "x y" (markdown-inlines-text (markdown-link-inlines l)))))

(test "entities are decoded, safely"
  (check-equal? "<script>alert(1)</script>" (plain "&lt;script&gt;alert(1)&lt;/script&gt;"))
  (check-equal? "& \xa9; \x2267;\x338; A B" (plain "&amp; &copy; &ngE; &#65; &#x42;"))
  (check-equal? "\xfffd; \xfffd; \xfffd; \xfffd;" (plain "&#0; &#xD800; &#x110000; &#9999999;"))
  ;; Past seven digits it is no entity (CommonMark).
  (check-equal? "&#99999999;" (plain "&#99999999;"))
  (check-equal? "&bogus; &;" (plain "&bogus; &;"))
  (check-equal? "a\xfffd;b" (plain (string #\a #\nul #\b)))
  (check-equal? '("\"<") (map markdown-link-title (links "[a](https://x.org \"&quot;&lt;\")")))
  (check-equal? '("https://x.org/\xe9;") (map markdown-link-href (links "[a](https://x.org/&eacute;)")))
  (check-equal? '("h&s") (map markdown-code-block-info (markdown-parse "```h&amp;s\nx\n```\n"))))

(test "deeply nested input parses"
  (let* ([depth 5000]
         [quotes (string-append (make-string depth #\>) " deep")]
         [brackets (string-append (make-string depth #\[) "x" (make-string depth #\]))]
         [stars (string-append (make-string depth #\*) "x" (make-string depth #\*))]
         [images (string-append (apply string-append (make-list 1000 "![")) "x"
                                (apply string-append (make-list 1000 "](https://i.org)")))]
         [lists (apply string-append
                  (map (lambda (i) (string-append (make-string (* 2 i) #\space) "- i\n")) (iota 200)))])
    (check-equal? "deep" (plain quotes))
    (check-equal? (+ 1 (* 2 depth)) (string-length (plain brackets)))
    (check (string-contains? (plain stars) "x"))
    (check-equal? "x" (markdown-inlines-text (markdown-paragraph-inlines (car (markdown-parse images)))))
    (check-equal? 1 (length (markdown-parse lists)))))

(test "huge input parses"
  (let* ([section "## Heading\n\nSome *text* with `code` and [a link](https://x.org).\n\n- one\n- two\n\n```\ncode\n```\n\n"]
         [document (apply string-append (make-list 20000 section))]
         [blocks (markdown-parse document)])
    (check (> (string-length document) 1000000))
    (check-equal? (* 4 20000) (length blocks))))

;; Each document is built so that a search repeated per link, per nested
;; block or per cell would take quadratic time, ten seconds or more; each
;; must parse in well under that.
(test "hostile input parses in linear time"
  (define (seconds thunk)
    (let ([start (real-time)])
      (thunk)
      (/ (- (real-time) start) 1000.)))
  (define (repeated n s) (apply string-append (make-list n s)))
  (for-each
    (lambda (name document)
      (let ([time (seconds (lambda () (markdown-parse document)))])
        (check-equal? (list name #t) (list name (< time 2.)))))
    '(shortcut-links-before-parens nested-quotes-before-space empty-nested-quotes
      empty-nested-quotes-spaced blank-lines-in-deep-quotes wide-table reference-far-away
      unclosed-titles)
    (list
      (string-append (repeated 20000 "[a](((((((\n") "\n[a]: https://x.org\n")
      (string-append (make-string 20000 #\>) " x" (make-string 1000000 #\space))
      (make-string 100000 #\>)
      (repeated 50000 "> ")
      (string-append (make-string 2000 #\>) " x\n" (repeated 2000 (string-append (make-string 2000 #\>) "\n")))
      (string-append "|" (repeated 50000 " a |") "\n|" (repeated 50000 "---|") "\n")
      (string-append (repeated 20000 "[a] ") (make-string 1000000 #\newline) "[a]: https://x.org\n")
      (repeated 20000 "[a](https://x.org \"((((\")\n"))))

(test-exit)
