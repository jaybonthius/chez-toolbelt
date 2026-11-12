(define segments '())
(define segments-length 0)

(define (save! text)
  (set! segments (cons text segments))
  (set! segments-length (fx+ segments-length (text-length text))))

(define (save-range! buf from to)
  (let ([text (make-text (fx- to from))])
    (text-copy! buf from text 0 (fx- to from))
    (save! text)))

;; The port is left after the value's text before the text is parsed, so
;; that an error leaves it there too.
(define (finish buf from to skipped)
  (set-port-index! port to)
  (if (null? segments)
      (parse-whole buf from to (fx- skipped from))
      (begin
        (save-range! buf from to)
        (finish-saved skipped))))

(define (parse-whole text from to offset)
  (let-values ([(v end) (parse 'json-read text from to offset max-depth max-digits duplicates? #t)])
    v))

(define (finish-saved skipped)
  (let ([text (make-text segments-length)])
    (let loop ([runs segments] [end segments-length])
      (unless (null? runs)
        (let* ([run (car runs)] [start (fx- end (text-length run))])
          (text-copy! run 0 text start (text-length run))
          (loop (cdr runs) start))))
    (parse-whole text 0 segments-length skipped)))

;; A port whose buffer stays empty after a lookahead, such as Chez's
;; console port, has no buffer to scan, so it is read one unit at a time.
(define (buffered?)
  (fx< (port-index port) (port-size port)))

(define-syntax with-next-buffer
  (syntax-rules ()
    [(_ (buf from n) skipped continue read-units)
     (begin
       (save-range! buf from n)
       (set-port-index! port n)
       (cond
         [(eof-object? (lookahead port)) (finish-saved skipped)]
         [(buffered?)
          (let ([buf (port-buffer port)] [from (port-index port)])
            (continue buf from (port-size port) from))]
         [else (read-units)]))]))

(define (scan-token buf from n j skipped)
  (cond
    [(fx= j n)
     (with-next-buffer (buf from n) skipped
       (lambda (buf from n j) (scan-token buf from n j skipped))
       (lambda () (units-in-token skipped)))]
    [(json-delimiter? (unit-at buf j)) (finish buf from j skipped)]
    [else (scan-token buf from n (fx+ j 1) skipped)]))

(define (scan-structure buf from n j depth skipped)
  (if (fx= j n)
      (with-next-buffer (buf from n) skipped
        (lambda (buf from n j) (scan-structure buf from n j depth skipped))
        (lambda () (units-in-structure depth skipped)))
      (let ([u (unit-at buf j)])
        (cond
          [(fx< u 91)
           (if (fx= u 34)
               (scan-string buf from n (fx+ j 1) depth skipped)
               (scan-structure buf from n (fx+ j 1) depth skipped))]
          [(or (fx= u 91) (fx= u 123))
           (if (fx= depth max-depth)
               (finish buf from (fx+ j 1) skipped)
               (scan-structure buf from n (fx+ j 1) (fx+ depth 1) skipped))]
          [(or (fx= u 93) (fx= u 125))
           (if (fx= depth 1)
               (finish buf from (fx+ j 1) skipped)
               (scan-structure buf from n (fx+ j 1) (fx- depth 1) skipped))]
          [else (scan-structure buf from n (fx+ j 1) depth skipped)]))))

(define (scan-string buf from n j depth skipped)
  (if (fx= j n)
      (with-next-buffer (buf from n) skipped
        (lambda (buf from n j) (scan-string buf from n j depth skipped))
        (lambda () (units-in-string depth skipped)))
      (let ([u (unit-at buf j)])
        (cond
          [(fx> u 92) (scan-string buf from n (fx+ j 1) depth skipped)]
          [(fx= u 92) (scan-escaped buf from n (fx+ j 1) depth skipped)]
          [(fx= u 34)
           (if (fx= depth 0)
               (finish buf from (fx+ j 1) skipped)
               (scan-structure buf from n (fx+ j 1) depth skipped))]
          [else (scan-string buf from n (fx+ j 1) depth skipped)]))))

(define (scan-escaped buf from n j depth skipped)
  (if (fx= j n)
      (with-next-buffer (buf from n) skipped
        (lambda (buf from n j) (scan-escaped buf from n j depth skipped))
        (lambda () (units-in-escape depth skipped)))
      (scan-string buf from n (fx+ j 1) depth skipped)))

(define unit-output #f)
(define extract-units #f)

(define (lookahead-code)
  (let ([x (lookahead port)])
    (if (eof-object? x) -1 (unit-code x))))

(define (take-unit!)
  (unless unit-output
    (let-values ([(output extract) (open-text-output)])
      (set! unit-output output)
      (set! extract-units extract)))
  (put-unit unit-output (get port)))

(define (finish-units skipped)
  (when unit-output (save! (extract-units)))
  (finish-saved skipped))

(define (units-in-token skipped)
  (let ([u (lookahead-code)])
    (if (or (fx< u 0) (json-delimiter? u))
        (finish-units skipped)
        (begin (take-unit!) (units-in-token skipped)))))

(define (units-in-structure depth skipped)
  (let ([u (lookahead-code)])
    (if (fx< u 0)
        (finish-units skipped)
        (begin
          (take-unit!)
          (cond
            [(fx= u 34) (units-in-string depth skipped)]
            [(or (fx= u 91) (fx= u 123))
             (if (fx= depth max-depth)
                 (finish-units skipped)
                 (units-in-structure (fx+ depth 1) skipped))]
            [(or (fx= u 93) (fx= u 125))
             (if (fx= depth 1)
                 (finish-units skipped)
                 (units-in-structure (fx- depth 1) skipped))]
            [else (units-in-structure depth skipped)])))))

(define (units-in-string depth skipped)
  (let ([u (lookahead-code)])
    (if (fx< u 0)
        (finish-units skipped)
        (begin
          (take-unit!)
          (cond
            [(fx= u 92) (units-in-escape depth skipped)]
            [(fx= u 34)
             (if (fx= depth 0)
                 (finish-units skipped)
                 (units-in-structure depth skipped))]
            [else (units-in-string depth skipped)])))))

(define (units-in-escape depth skipped)
  (if (fx< (lookahead-code) 0)
      (finish-units skipped)
      (begin (take-unit!) (units-in-string depth skipped))))

(define (units-in-value skipped)
  (let ([u (lookahead-code)])
    (take-unit!)
    (cond
      [(fx= u 34) (units-in-string 0 skipped)]
      [(or (fx= u 91) (fx= u 123))
       (if (fx= max-depth 0)
           (finish-units skipped)
           (units-in-structure 1 skipped))]
      [(or (fx= u 93) (fx= u 125)) (finish-units skipped)]
      [else (units-in-token skipped)])))

;; The buffer may end inside a value, so a failure, or a number or literal
;; that reaches the buffer's end, is not final: the value is scanned to its
;; end and parsed again.
(define (read-in-buffer buf i n skipped)
  (let-values ([(v j) (parse 'json-read buf i n (fx- skipped i)
                        max-depth max-digits duplicates? #f)])
    (if (and j
             (let ([opening (unit-at buf i)])
               (or (fx= opening 34) (fx= opening 91) (fx= opening 123)
                   (and (fx< j n) (json-delimiter? (unit-at buf j))))))
        (begin
          (set-port-index! port j)
          v)
        (scan-value buf i n skipped))))

(define (scan-value buf i n skipped)
  (let ([u (unit-at buf i)])
    (cond
      [(fx= u 34) (scan-string buf i n (fx+ i 1) 0 skipped)]
      [(or (fx= u 91) (fx= u 123))
       (if (fx= max-depth 0)
           (finish buf i (fx+ i 1) skipped)
           (scan-structure buf i n (fx+ i 1) 1 skipped))]
      [(or (fx= u 93) (fx= u 125)) (finish buf i (fx+ i 1) skipped)]
      [else (scan-token buf i n (fx+ i 1) skipped)])))

(let skip ([skipped (skip-byte-order-mark)])
  (cond
    [(eof-object? (lookahead port)) (eof-object)]
    [(buffered?)
     (let ([buf (port-buffer port)] [n (port-size port)])
       (let whitespace ([i (port-index port)] [skipped skipped])
         (cond
           [(fx= i n) (set-port-index! port i) (skip skipped)]
           [(json-whitespace? (unit-at buf i)) (whitespace (fx+ i 1) (fx+ skipped 1))]
           [else (read-in-buffer buf i n skipped)])))]
    [(json-whitespace? (lookahead-code)) (get port) (skip (fx+ skipped 1))]
    [else (units-in-value skipped)]))
