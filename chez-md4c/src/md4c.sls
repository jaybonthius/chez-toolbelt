(library (md4c)
  (export
    markdown-block? markdown-block-start markdown-block-end
    markdown-paragraph? markdown-paragraph-inlines
    markdown-heading? markdown-heading-level markdown-heading-inlines
    markdown-code-block? markdown-code-block-fenced? markdown-code-block-info
    markdown-code-block-language markdown-code-block-text
    markdown-quote? markdown-quote-blocks
    markdown-list? markdown-list-ordered? markdown-list-first-number markdown-list-tight?
    markdown-list-items
    markdown-item? markdown-item-task? markdown-item-checked? markdown-item-blocks
    markdown-table? markdown-table-alignments markdown-table-header markdown-table-rows
    markdown-rule?

    markdown-inline? markdown-inline-start markdown-inline-end
    markdown-text? markdown-text-string
    markdown-line-break? markdown-line-break-hard?
    markdown-span? markdown-span-kind markdown-span-inlines
    markdown-link? markdown-link-href markdown-link-title markdown-link-autolink?
    markdown-link-inlines
    markdown-image? markdown-image-src markdown-image-title markdown-image-inlines

    markdown-parse markdown-inlines-text)
  (import (chezscheme))

  (define-record-type markdown-block (nongenerative) (fields start end))

  (define-record-type markdown-paragraph (parent markdown-block) (nongenerative) (sealed #t)
    (fields inlines))

  (define-record-type markdown-heading (parent markdown-block) (nongenerative) (sealed #t)
    (fields level inlines))

  (define-record-type markdown-code-block (parent markdown-block) (nongenerative) (sealed #t)
    (fields fenced? info language text))

  (define-record-type markdown-quote (parent markdown-block) (nongenerative) (sealed #t)
    (fields blocks))

  (define-record-type markdown-list (parent markdown-block) (nongenerative) (sealed #t)
    (fields ordered? first-number tight? items))

  (define-record-type markdown-item (parent markdown-block) (nongenerative) (sealed #t)
    (fields task? checked? blocks))

  (define-record-type markdown-table (parent markdown-block) (nongenerative) (sealed #t)
    (fields alignments header rows))

  (define-record-type markdown-rule (parent markdown-block) (nongenerative) (sealed #t))

  (define-record-type markdown-inline (nongenerative) (fields start end))

  (define-record-type markdown-text (parent markdown-inline) (nongenerative) (sealed #t)
    (fields string))

  (define-record-type markdown-line-break (parent markdown-inline) (nongenerative) (sealed #t)
    (fields hard?))

  (define-record-type markdown-span (parent markdown-inline) (nongenerative) (sealed #t)
    (fields kind inlines))

  (define-record-type markdown-link (parent markdown-inline) (nongenerative) (sealed #t)
    (fields href title autolink? inlines))

  (define-record-type markdown-image (parent markdown-inline) (nongenerative) (sealed #t)
    (fields src title inlines))

  (define soft-break (make-markdown-line-break #f #f #f))
  (define hard-break (make-markdown-line-break #f #f #t))

  ;; The shared object is built beside this library's compiled object, so it
  ;; is found the way the library was. A program linked with it already has
  ;; its entries.
  (define md4c-library
    (unless (foreign-entry? "chez_md4c_parse")
      (let ([name "md4c/libchez-md4c.so"])
        (let loop ([directories (library-directories)])
          (cond
            [(null? directories)
             (errorf 'md4c "~a is in none of the library directories ~s" name (library-directories))]
            [(file-exists? (path-build (cdar directories) name))
             (load-shared-object (path-build (cdar directories) name))]
            [else (loop (cdr directories))])))))

  ;; Collect-safe, so that other threads can collect during a long parse;
  ;; the document is locked so that the collector cannot move it meanwhile.
  (define md4c-parse (foreign-procedure __collect_safe "chez_md4c_parse" (u8* size_t) uptr))
  (define md4c-events-size (foreign-procedure "chez_md4c_size" (uptr) size_t))
  (define md4c-take-events! (foreign-procedure "chez_md4c_take" (uptr u8*) void))

  (define no-offset #xffffffff)

  ;; An interrupt handler that escaped between these steps would leave the
  ;; document locked and md4c's events never freed.
  (define (document-events bytes)
    (with-interrupts-disabled
      (lock-object bytes)
      (let ([parsed (md4c-parse bytes (bytevector-length bytes))])
        (unlock-object bytes)
        (and (not (eqv? parsed 0))
             (let ([events (make-bytevector (md4c-events-size parsed))])
               (md4c-take-events! parsed events)
               events)))))

  (define-record-type node (nongenerative) (sealed #t)
    (fields children first last content-start tail-lines fence quote? build))

  ;; A generator of top-level blocks rather than a list of them, so that
  ;; each is converted as soon as the one after it is read and the document
  ;; is never held whole twice.
  (define (event-reader events document)
    (define position 0)

    (define (unexpected code)
      (assertion-violationf 'markdown-parse "md4c's events are out of step at ~s" code))

    (define (next-u8!)
      (let ([value (bytevector-u8-ref events position)])
        (set! position (fx+ position 1))
        value))

    (define (next-code!) (integer->char (next-u8!)))

    (define (next-u32!)
      (let ([value (bytevector-u32-ref events position (endianness little))])
        (set! position (fx+ position 4))
        value))

    (define (next-offset!)
      (let ([value (next-u32!)])
        (and (not (fx= value no-offset)) value)))

    (define (next-string!)
      (let* ([size (bytevector-u64-ref events position (endianness little))]
             [bytes (make-bytevector size)])
        (set! position (fx+ position 8))
        (bytevector-copy! events position bytes 0 size)
        (set! position (fx+ position size))
        (utf8->string bytes)))

    (define (read-inline! code items)
      (case code
        [(#\t)
         (let* ([start (next-u32!)] [end (fx+ start (next-u32!))])
           (values (cons (make-markdown-text start end (substring document start end)) items)
                   start end))]
        [(#\x)
         (let* ([start (next-offset!)]
                [end (let ([size (next-u32!)]) (and start (fx+ start size)))])
           (values (cons (make-markdown-text start end (next-string!)) items) start end))]
        [(#\n) (values (cons soft-break items) #f #f)]
        [(#\N) (values (cons hard-break items) #f #f)]
        [(#\e #\s #\~ #\`)
         (let-values ([(inlines first last) (read-inlines!)])
           (let-values ([(start end) (span-range document code first last)])
             (values (cons (make-markdown-span start end (span-kind code) inlines) items)
                     start end)))]
        [(#\a)
         (let* ([href (next-string!)] [title (next-string!)] [autolink? (fx= (next-u8!) 1)])
           (let-values ([(inlines first last) (read-inlines!)])
             (let-values ([(start end) (link-range document first last (next-u32!))])
               (values (if (link-address? href)
                           (cons (make-markdown-link start end href title autolink? inlines) items)
                           (append (reverse inlines) items))
                       start end))))]
        [(#\i)
         (let* ([src (next-string!)] [title (next-string!)])
           (let-values ([(inlines first last) (read-inlines!)])
             (let-values ([(start end) (image-range document first last (next-u32!))])
               (values (if (image-address? src)
                           (cons (make-markdown-image start end src title inlines) items)
                           (append (reverse inlines) items))
                       start end))))]
        [else (unexpected code)]))

    (define (read-inlines!)
      (let loop ([items '()] [first #f] [last #f])
        (let ([code (next-code!)])
          (if (char=? code #\.)
              (values (reverse! items) first last)
              (let-values ([(items start end) (read-inline! code items)])
                (loop items (or first (and start (escape-start document start))) (or end last)))))))

    (define (peek-code) (integer->char (bytevector-u8-ref events position)))

    (define (block-code? code) (memv code '(#\P #\H #\C #\Q #\U #\O #\L #\T #\-)))

    ;; A tight list item holds its text directly; each run of inlines becomes
    ;; a paragraph, so that items always hold blocks.
    (define (next-node!)
      (let ([code (peek-code)])
        (cond
          [(char=? code #\.) (next-code!) #f]
          [(block-code? code) (next-code!) (read-block! code)]
          [else
           (let loop ([run '()] [first #f] [last #f])
             (let ([code (peek-code)])
               (if (or (char=? code #\.) (block-code? code))
                   (leaf-node first last
                     (let ([inlines (reverse! run)])
                       (lambda (start end blocks) (make-markdown-paragraph start end inlines))))
                   (let-values ([(run start end) (read-inline! (next-code!) run)])
                     (loop run (or first start) (or end last))))))])))

    (define (read-blocks!)
      (let loop ([nodes '()] [first #f] [last #f])
        (let ([node (next-node!)])
          (if node
              (loop (cons node nodes) (or first (node-first node)) (or (node-last node) last))
              (values (reverse! nodes) first last)))))

    (define (leaf-node first last build)
      (make-node '() first last (and first (line-start document first)) (if first 0 1) #f #f build))

    (define (container-node children first last quote? build)
      (make-node children first last
        (and (pair? children) (node-content-start (car children)))
        (let loop ([children children] [tail 0] [contents? #f])
          (cond
            [(null? children) (if contents? tail (fxmax tail 1))]
            [(node-last (car children)) (loop (cdr children) (node-tail-lines (car children)) #t)]
            [else (loop (cdr children) (fx+ tail (node-tail-lines (car children))) contents?)]))
        #f quote? build))

    (define (read-block! code)
      (case code
        [(#\P)
         (let-values ([(inlines first last) (read-inlines!)])
           (leaf-node first last
             (lambda (start end blocks) (make-markdown-paragraph start end inlines))))]
        [(#\H)
         (let ([level (next-u8!)])
           (let-values ([(inlines first last) (read-inlines!)])
             (make-node '() first last (and first (line-start document first))
               ;; A setext heading's underline follows its text.
               (cond
                 [(not first) 1]
                 [(atx-heading-text? document first) 0]
                 [else 1])
               #f #f
               (lambda (start end blocks) (make-markdown-heading start end level inlines)))))]
        [(#\C) (read-code-block!)]
        [(#\Q)
         (let-values ([(children first last) (read-blocks!)])
           (container-node children first last #t
             (lambda (start end blocks) (make-markdown-quote start end blocks))))]
        [(#\U #\O)
         (let* ([ordered? (char=? code #\O)]
                [first-number (if ordered? (next-u32!) 1)]
                [tight? (fx= (next-u8!) 1)])
           (let-values ([(children first last) (read-blocks!)])
             (container-node children first last #f
               (lambda (start end items)
                 (make-markdown-list start end ordered? first-number tight? items)))))]
        [(#\L)
         (let ([mark (integer->char (next-u8!))])
           (let-values ([(children first last) (read-blocks!)])
             (container-node children first last #f
               (lambda (start end blocks)
                 (make-markdown-item start end (not (char=? mark #\nul)) (and (memv mark '(#\x #\X)) #t)
                   blocks)))))]
        [(#\T) (read-table!)]
        [(#\-)
         (next-code!)
         (leaf-node #f #f (lambda (start end blocks) (make-markdown-rule start end)))]
        [else (unexpected code)]))

    (define (read-code-block!)
      (let* ([fence (let ([c (next-u8!)]) (and (fx> c 0) (integer->char c)))]
             [info-start (next-offset!)]
             [info (next-string!)]
             [language (next-string!)]
             [text (open-output-string)])
        (let loop ([first #f] [last #f])
          (let ([code (next-code!)])
            (case code
              [(#\.)
               (make-node '() first last
                 (cond
                   [(not fence) (and first (line-start document first))]
                   [info-start (line-start document info-start)]
                   [first (previous-filled-line document (line-start document first))]
                   [else #f])
                 (cond
                   [(not first) 1]
                   [(and fence
                         (fence-line? document
                           (first-filled-line document (next-line-start document last) quote-markers any-quotes)
                           fence))
                    1]
                   [else 0])
                 fence #f
                 (let ([text (get-output-string text)])
                   (lambda (start end blocks)
                     (make-markdown-code-block start end (and fence #t) info language text))))]
              [(#\t)
               (let* ([start (next-u32!)] [end (fx+ start (next-u32!))])
                 (put-string text document start (fx- end start))
                 (loop (or first start) end))]
              [(#\x)
               (let* ([start (next-offset!)]
                      [end (let ([size (next-u32!)]) (and start (fx+ start size)))])
                 (put-string text (next-string!))
                 (loop (or first start) (or end last)))]
              [else (unexpected code)])))))

    ;; The first row is the header; md4c gives every table exactly one. Each
    ;; row is one line, and the delimiter row follows the header.
    (define (read-table!)
      (define (read-row!)
        (let loop ([alignments '()] [cells '()] [first #f] [last #f])
          (let ([code (next-code!)])
            (case code
              [(#\.) (values (reverse! alignments) (reverse! cells) first last)]
              [(#\D)
               (let ([alignment (case (integer->char (next-u8!))
                                  [(#\l) 'left]
                                  [(#\c) 'center]
                                  [(#\r) 'right]
                                  [else #f])])
                 (let-values ([(inlines start end) (read-inlines!)])
                   (loop (cons alignment alignments) (cons inlines cells)
                         (or first start) (or end last))))]
              [else (unexpected code)]))))
      (let loop ([alignments '()] [rows '()] [count 0] [last-row #f] [first #f] [last #f])
        (let ([code (next-code!)])
          (case code
            [(#\.)
             (let* ([rows (reverse! rows)]
                    [header (if (pair? rows) (car rows) '())]
                    [body (if (pair? rows) (cdr rows) '())])
               (make-node '() first last (and first (line-start document first))
                 (cond
                   [(not last-row) (fx+ count 1)]
                   [(fx= last-row 0) count]
                   [else (fx- count (fx+ last-row 1))])
                 #f #f
                 (lambda (start end blocks)
                   (make-markdown-table start end alignments header body))))]
            [(#\R)
             (let-values ([(row-alignments cells start end) (read-row!)])
               (loop (if (null? rows) row-alignments alignments) (cons cells rows)
                     (fx+ count 1) (if end count last-row)
                     (or first start) (or end last)))]
            [else (unexpected code)]))))

    next-node!)

  (define (span-kind code)
    (case code
      [(#\e) 'emphasis]
      [(#\s) 'strong]
      [(#\~) 'strikethrough]
      [(#\`) 'code]))

  ;; Only http:, https: and, for a link, mailto: addresses are followed,
  ;; once decoded and in any case. Relative addresses are refused too, so
  ;; javascript:, data: and vbscript: are refused however they are written.
  (define (image-address? address)
    (or (prefix-ci? "http:" address) (prefix-ci? "https:" address)))

  (define (link-address? address)
    (or (image-address? address) (prefix-ci? "mailto:" address)))

  (define (prefix-ci? prefix s)
    (let ([n (string-length prefix)])
      (and (fx<= n (string-length s))
           (let loop ([i 0])
             (or (fx= i n)
                 (and (char=? (char-downcase (string-ref s i)) (string-ref prefix i))
                      (loop (fx+ i 1))))))))

  ;; md4c drops the backslash of an escape, so contents that start with one
  ;; start a character before their first text, where their delimiters are
  ;; looked for.
  (define (escape-start document i)
    (if (and (char-at? document (fx- i 1) #\\)
             (fx< i (string-length document))
             (let ([c (string-ref document i)])
               (and (char<=? #\! c #\~) (not (char-alphabetic? c)) (not (char-numeric? c)))))
        (fx- i 1)
        i))

  (define (char-at? document i c)
    (and (fx<= 0 i) (fx< i (string-length document)) (char=? (string-ref document i) c)))

  (define (line-end? c) (or (char=? c #\newline) (char=? c #\return)))

  (define (line-start document i)
    (let loop ([i (fxmin i (string-length document))])
      (if (and (fx> i 0) (not (line-end? (string-ref document (fx- i 1)))))
          (loop (fx- i 1))
          i)))

  (define (line-end document i)
    (let ([size (string-length document)])
      (let loop ([i i])
        (if (and (fx< i size) (not (line-end? (string-ref document i))))
            (loop (fx+ i 1))
            i))))

  (define (next-line-start document i)
    (let ([end (line-end document i)] [size (string-length document)])
      (cond
        [(fx= end size) size]
        [(and (char=? (string-ref document end) #\return) (char-at? document (fx+ end 1) #\newline))
         (fx+ end 2)]
        [else (fx+ end 1)])))

  (define (quote-markers document i)
    (let ([end (line-end document i)])
      (let loop ([i i] [markers 0])
        (if (fx= i end)
            markers
            (case (string-ref document i)
              [(#\space #\tab) (loop (fx+ i 1) markers)]
              [(#\>) (loop (fx+ i 1) (fx+ markers 1))]
              [else #f])))))

  ;; quote-markers, remembering each line's answer, since blocks nested in
  ;; quotes ask about the same lines at every depth.
  (define (remembering-quote-markers)
    (let ([known (make-eqv-hashtable)])
      (lambda (document i)
        (let ([markers (hashtable-ref known i 'unknown)])
          (if (eq? markers 'unknown)
              (let ([markers (quote-markers document i)])
                (hashtable-set! known i markers)
                markers)
              markers)))))

  ;; A line with more markers than the QUOTES quotes it is inside opens
  ;; another quote.
  (define (blank-line? line-markers document i quotes)
    (let ([markers (line-markers document i)])
      (and markers (fx<= markers quotes))))

  ;; Code's own blank lines may sit inside any number of quotes.
  (define any-quotes (most-positive-fixnum))

  (define (fence-line? document i fence)
    (let ([end (line-end document i)])
      (let loop ([i i] [count 0])
        (if (fx= i end)
            (fx>= count 3)
            (let ([c (string-ref document i)])
              (cond
                [(char=? c fence) (loop (fx+ i 1) (fx+ count 1))]
                [(memv c '(#\space #\tab)) (loop (fx+ i 1) count)]
                [(and (char=? c #\>) (fx= count 0)) (loop (fx+ i 1) count)]
                [else #f]))))))

  (define (previous-line-start document line)
    (line-start document
      (if (and (fx> line 1) (char=? (string-ref document (fx- line 1)) #\newline)
               (char=? (string-ref document (fx- line 2)) #\return))
          (fx- line 2)
          (fx- line 1))))

  (define (previous-filled-line document line)
    (let loop ([line line])
      (if (fx= line 0)
          0
          (let ([above (previous-line-start document line)])
            (if (and (fx> above 0) (blank-line? quote-markers document above any-quotes))
                (loop above)
                above)))))

  (define (skip-lines document i count line-markers quotes)
    (if (fx= count 0)
        i
        (skip-lines document
          (next-line-start document (first-filled-line document i line-markers quotes))
          (fx- count 1) line-markers quotes)))

  (define (atx-heading-text? document i)
    (let loop ([i i])
      (and (fx> i 0)
           (let ([c (string-ref document (fx- i 1))])
             (cond
               [(memv c '(#\space #\tab)) (loop (fx- i 1))]
               [else (char=? c #\#)])))))

  (define (first-filled-line document i line-markers quotes)
    (let loop ([i i])
      (if (and (fx< i (string-length document)) (blank-line? line-markers document i quotes))
          (loop (next-line-start document i))
          i)))

  ;; md4c keeps any other white space, such as U+00A0 or a form feed, as
  ;; text, so trimming it could end a block before its own text.
  (define (trim-white-space document start end)
    (let loop ([end end])
      (if (and (fx> end start)
               (memv (string-ref document (fx- end 1)) '(#\space #\tab #\newline #\return)))
          (loop (fx- end 1))
          end)))

  (define (marker-line document end)
    (let loop ([i end] [markers 0])
      (if (or (fx= i 0) (line-end? (string-ref document (fx- i 1))))
          (values i markers)
          (case (string-ref document (fx- i 1))
            [(#\space #\tab) (loop (fx- i 1) markers)]
            [(#\>) (loop (fx- i 1) (fx+ markers 1))]
            [else (values #f #f)]))))

  ;; Nested blocks that share an end share its line, so it is scanned once
  ;; however deep they go.
  (define (trim-quoted document start end line markers quotes)
    (if (and line (fx> line start) (fx<= markers quotes))
        (let ([end (trim-white-space document start line)])
          (let-values ([(line markers) (marker-line document end)])
            (trim-quoted document start end line markers quotes)))
        (values end line markers)))

  (define (run-length document i c limit forward?)
    (let loop ([n 0])
      (if (and (fx< n limit) (char-at? document (if forward? (fx+ i n) (fx- i (fx+ n 1))) c))
          (loop (fx+ n 1))
          n)))

  ;; md4c reports no positions for spans, so a span's source is worked out
  ;; from the source of its contents, FIRST to LAST, and the delimiters
  ;; around them.
  (define (span-range document code first last)
    (cond
      [(not (and first last)) (values #f #f)]
      [(memv code '(#\e #\s))
       (let ([width (if (char=? code #\e) 1 2)]
             [c (and (fx> first 0) (string-ref document (fx- first 1)))])
         (if (memv c '(#\* #\_))
             (values (fx- first (run-length document first c width #f))
                     (fx+ last (run-length document last c width #t)))
             (values first last)))]
      [(char=? code #\~)
       (let ([before (run-length document first #\~ 2 #f)])
         (values (fx- first before) (fx+ last (run-length document last #\~ before #t))))]
      [else
       (let* ([space-before (if (char-at? document (fx- first 1) #\space) 1 0)]
              [ticks (run-length document (fx- first space-before) #\` (fx- first space-before) #f)]
              [space-after (if (and (fx= space-before 1) (char-at? document last #\space)) 1 0)])
         (values (fx- first (fx+ space-before ticks))
                 (fx+ last space-after (run-length document (fx+ last space-after) #\` ticks #t))))]))

  ;; LIMIT is where the next text from the document starts: no link's
  ;; destination, title or reference label reaches past it, which keeps the
  ;; search for them linear on hostile input.
  (define (link-range document first last limit)
    (cond
      [(not (and first last)) (values #f #f)]
      [(char-at? document (fx- first 1) #\<)
       (values (fx- first 1) (if (char-at? document last #\>) (fx+ last 1) last))]
      [(char-at? document (fx- first 1) #\[)
       (values (fx- first 1)
               (if (char-at? document last #\]) (link-tail-end document (fx+ last 1) limit) last))]
      [else (values first last)]))

  (define (image-range document first last limit)
    (if (not (and first last))
        (values #f #f)
        (values (if (and (char-at? document (fx- first 1) #\[) (char-at? document (fx- first 2) #\!))
                    (fx- first 2)
                    first)
                (if (char-at? document last #\]) (link-tail-end document (fx+ last 1) limit) last))))

  (define (link-tail-end document i limit)
    (define (at? j c) (and (fx< j limit) (char=? (string-ref document j) c)))
    (define (space? j) (and (fx< j limit) (memv (string-ref document j) '(#\space #\tab #\newline #\return))))
    ;; A destination or title may go on in the next line of a quote.
    (define (skip-space j)
      (cond
        [(space? j)
         (if (line-end? (string-ref document j))
             (let markers ([j (fx+ j 1)])
               (if (or (space? j) (at? j #\>)) (markers (fx+ j 1)) j))
             (skip-space (fx+ j 1)))]
        [else j]))
    (define (skip-delimited j close newline-ok?)
      (let loop ([j j])
        (cond
          [(fx>= j limit) #f]
          [(char=? (string-ref document j) #\\) (loop (fx+ j 2))]
          [(char=? (string-ref document j) close) (fx+ j 1)]
          [(and (not newline-ok?) (line-end? (string-ref document j))) #f]
          [else (loop (fx+ j 1))])))
    (define (skip-destination j)
      (if (at? j #\<)
          (skip-delimited (fx+ j 1) #\> #f)
          (let loop ([j j] [depth 0])
            (cond
              [(or (fx>= j limit) (space? j)) j]
              [(at? j #\\) (loop (fx+ j 2) depth)]
              [(at? j #\() (loop (fx+ j 1) (fx+ depth 1))]
              [(at? j #\)) (if (fx= depth 0) j (loop (fx+ j 1) (fx- depth 1)))]
              [else (loop (fx+ j 1) depth)]))))
    (define (skip-title j)
      (cond
        [(at? j #\") (skip-delimited (fx+ j 1) #\" #t)]
        [(at? j #\') (skip-delimited (fx+ j 1) #\' #t)]
        [(at? j #\() (skip-delimited (fx+ j 1) #\) #t)]
        [else j]))
    (cond
      [(at? i #\()
       (let* ([j (skip-destination (skip-space (fx+ i 1)))]
              [j (and j (skip-title (skip-space j)))]
              [j (and j (skip-space j))])
         (if (and j (at? j #\))) (fx+ j 1) i))]
      [(at? i #\[) (or (skip-delimited (fx+ i 1) #\] #t) i)]
      [else i]))

  (define (block-start document line-markers node previous previous-start parent-start quotes)
    (or (node-content-start node)
        (if previous
            (let ([after (skip-lines document
                           (if (node-last previous)
                               (next-line-start document (node-last previous))
                               previous-start)
                           (node-tail-lines previous)
                           line-markers
                           ;; Inside a quote, a line of its markers alone is
                           ;; blank, unless the quote is empty: then the line
                           ;; is the quote.
                           (if (and (node-quote? previous) (pair? (node-children previous)))
                               (fx+ quotes 1)
                               quotes))]
                  [fence (and (not (node-last previous)) (node-fence previous))])
              (let ([at (first-filled-line document after line-markers quotes)])
                (if (and fence (fence-line? document at fence))
                    (first-filled-line document (next-line-start document at) line-markers quotes)
                    at)))
            (first-filled-line document parent-start line-markers quotes))))

  ;; END, the container's, is already trimmed and ends END-LINE with
  ;; END-MARKERS, so that blocks nested deeply at the end of a document do
  ;; not each scan its end again.
  (define (convert-blocks document line-markers next-node! start end end-line end-markers quotes)
    (define (start-of node previous previous-start)
      (fxmax start
        (fxmin end (block-start document line-markers node previous previous-start start quotes))))
    (let ([first (next-node!)])
      (let loop ([node first] [from (and first (start-of first #f start))] [blocks '()])
        (if (not node)
            (reverse! blocks)
            ;; Lazy continuation text can look like white space or quote
            ;; markers, so no block is trimmed before its last contents.
            (let* ([following (next-node!)]
                   [next (and following (start-of following node from))]
                   [lowest (fxmin (fxmax from (or (node-last node) from)) end)])
              (let*-values ([(to line markers)
                             (if next
                                 (let ([to (trim-white-space document lowest (fxmax lowest next))])
                                   (let-values ([(line markers) (marker-line document to)])
                                     (values to line markers)))
                                 (values end end-line end-markers))]
                            [(to line markers) (trim-quoted document lowest to line markers quotes)])
                (loop following next
                      (cons ((node-build node) from to
                             (convert-blocks document line-markers (list-reader (node-children node))
                               from to line markers (if (node-quote? node) (fx+ quotes 1) quotes)))
                            blocks))))))))

  (define (list-reader items)
    (lambda ()
      (and (pair? items)
           (let ([item (car items)])
             (set! items (cdr items))
             item))))

  (define (markdown-parse document)
    (unless (string? document)
      (assertion-violationf 'markdown-parse "~s is not a string" document))
    (let ([bytes (string->utf8 document)])
      ;; md4c counts in 32 bits, and every offset must also differ from
      ;; the one that means none.
      (unless (< (bytevector-length bytes) no-offset)
        (raise (condition (make-implementation-restriction-violation)
                          (make-who-condition 'markdown-parse)
                          (make-message-condition "the document's UTF-8 is longer than md4c's limit")
                          (make-irritants-condition (list (bytevector-length bytes) (- no-offset 1))))))
      (let ([events (document-events bytes)])
        (unless events
          (error 'markdown-parse "md4c ran out of memory"))
        (let ([end (trim-white-space document 0 (string-length document))])
          (let-values ([(line markers) (marker-line document end)])
            (convert-blocks document (remembering-quote-markers) (event-reader events document)
              0 end line markers 0))))))

  (define (markdown-inlines-text inlines)
    (define (write-inlines inlines port)
      (for-each
        (lambda (inline)
          (cond
            [(markdown-text? inline) (put-string port (markdown-text-string inline))]
            [(markdown-line-break? inline)
             (put-char port (if (markdown-line-break-hard? inline) #\newline #\space))]
            [(markdown-span? inline) (write-inlines (markdown-span-inlines inline) port)]
            [(markdown-link? inline) (write-inlines (markdown-link-inlines inline) port)]
            [(markdown-image? inline) (write-inlines (markdown-image-inlines inline) port)]
            [else (assertion-violationf 'markdown-inlines-text "~s is not a Markdown inline" inline)]))
        inlines))
    (unless (list? inlines)
      (assertion-violationf 'markdown-inlines-text "~s is not a list" inlines))
    (let-values ([(port text) (open-string-output-port)])
      (write-inlines inlines port)
      (text))))
