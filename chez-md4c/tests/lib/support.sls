(library (support)
  (export all-inlines all-blocks texts links plain source string-contains? blocks->datum)
  (import (chezscheme) (md4c))

  (define (inline-children inline)
    (cond
      [(markdown-span? inline) (markdown-span-inlines inline)]
      [(markdown-link? inline) (markdown-link-inlines inline)]
      [(markdown-image? inline) (markdown-image-inlines inline)]
      [else '()]))

  (define (block-children block)
    (cond
      [(markdown-quote? block) (markdown-quote-blocks block)]
      [(markdown-list? block) (markdown-list-items block)]
      [(markdown-item? block) (markdown-item-blocks block)]
      [else '()]))

  (define (block-inlines block)
    (cond
      [(markdown-paragraph? block) (markdown-paragraph-inlines block)]
      [(markdown-heading? block) (markdown-heading-inlines block)]
      [(markdown-table? block)
       (apply append (apply append (markdown-table-header block) (markdown-table-rows block)))]
      [else '()]))

  (define (all-blocks blocks)
    (apply append (map (lambda (b) (cons b (all-blocks (block-children b)))) blocks)))

  (define (all-inlines blocks)
    (define (of-inlines inlines)
      (apply append (map (lambda (i) (cons i (of-inlines (inline-children i)))) inlines)))
    (apply append (map (lambda (b) (of-inlines (block-inlines b))) (all-blocks blocks))))

  (define (texts document)
    (filter markdown-text? (all-inlines (markdown-parse document))))

  (define (links document)
    (filter (lambda (i) (or (markdown-link? i) (markdown-image? i)))
            (all-inlines (markdown-parse document))))

  (define (plain document)
    (apply string-append (map markdown-text-string (texts document))))

  (define (source document x)
    (if (markdown-block? x)
        (substring document (markdown-block-start x) (markdown-block-end x))
        (substring document (markdown-inline-start x) (markdown-inline-end x))))

  (define (string-contains? s part)
    (let ([n (string-length part)])
      (let loop ([i 0])
        (and (<= (+ i n) (string-length s))
             (or (string=? (substring s i (+ i n)) part)
                 (loop (+ i 1)))))))

  ;; Records are equal only to themselves, so two parses are compared as
  ;; plain data.
  (define (blocks->datum blocks)
    (define (inlines->datum inlines) (map inline->datum inlines))
    (define (inline->datum i)
      (list* (markdown-inline-start i) (markdown-inline-end i)
        (cond
          [(markdown-text? i) (list 'text (markdown-text-string i))]
          [(markdown-line-break? i) (list 'break (markdown-line-break-hard? i))]
          [(markdown-span? i) (list (markdown-span-kind i) (inlines->datum (markdown-span-inlines i)))]
          [(markdown-link? i)
           (list 'link (markdown-link-href i) (markdown-link-title i) (markdown-link-autolink? i)
                 (inlines->datum (markdown-link-inlines i)))]
          [(markdown-image? i)
           (list 'image (markdown-image-src i) (markdown-image-title i)
                 (inlines->datum (markdown-image-inlines i)))])))
    (define (block->datum b)
      (list* (markdown-block-start b) (markdown-block-end b)
        (cond
          [(markdown-paragraph? b) (list 'paragraph (inlines->datum (markdown-paragraph-inlines b)))]
          [(markdown-heading? b)
           (list 'heading (markdown-heading-level b) (inlines->datum (markdown-heading-inlines b)))]
          [(markdown-code-block? b)
           (list 'code (markdown-code-block-fenced? b) (markdown-code-block-info b)
                 (markdown-code-block-language b) (markdown-code-block-text b))]
          [(markdown-quote? b) (list 'quote (blocks->datum (markdown-quote-blocks b)))]
          [(markdown-list? b)
           (list 'list (markdown-list-ordered? b) (markdown-list-first-number b)
                 (markdown-list-tight? b) (blocks->datum (markdown-list-items b)))]
          [(markdown-item? b)
           (list 'item (markdown-item-task? b) (markdown-item-checked? b)
                 (blocks->datum (markdown-item-blocks b)))]
          [(markdown-table? b)
           (list 'table (markdown-table-alignments b)
                 (map inlines->datum (markdown-table-header b))
                 (map (lambda (row) (map inlines->datum row)) (markdown-table-rows b)))]
          [(markdown-rule? b) (list 'rule)])))
    (map block->datum blocks)))
