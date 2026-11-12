(import (chezscheme) (ghostty) (check))

(define (bytes s) (string->utf8 s))

(define-syntax with-terminal
  (syntax-rules ()
    [(_ (t columns rows) body ...)
     (let ([t (make-terminal columns rows)])
       (dynamic-wind void (lambda () body ...) (lambda () (terminal-close! t))))]))

(define (head bv n)
  (let ([out (make-bytevector n)]) (bytevector-copy! bv 0 out 0 n) out))

(define (append-byte bv byte)
  (let ([out (make-bytevector (+ 1 (bytevector-length bv)) byte)])
    (bytevector-copy! bv 0 out 0 (bytevector-length bv))
    out))

(define (ghostty-error-with result)
  (lambda (c) (and (ghostty-error? c) (eq? (ghostty-error-result c) result))))

;; Wide rows fill a page with few rows, so the history spans several pages.
(define (history-snapshot)
  (with-terminal (t 10 3)
    (do ([i 0 (+ i 1)]) ((= i 300))
      (terminal-write! t (bytes (format "line-~a\r\n" i))))
    (terminal-resize! t 500 3)
    (terminal->snapshot t)))

(test "snapshots restore screens, history and parser state"
  (with-terminal (source 12 3)
    (terminal-set-continuation-max-bytes! source 64)
    (do ([i 0 (+ i 1)]) ((= i 100)) (terminal-write! source (bytes "styled Ω\r\n")))
    (terminal-write! source (bytes "primary\x1b;[?1049h\x1b;[H\x1b;[31mΩ\x1b;[0m"))
    (let* ([encoded (terminal->snapshot source)] [restored (snapshot->terminal encoded)])
      (check-equal? (bytes "GHOSTSNP") (head encoded 8))
      (check-equal? encoded (terminal->snapshot restored))
      (check-equal? "Ω" (terminal->string restored))
      (check-equal? 'alternate (terminal-active-screen restored))
      (terminal-write! source (bytes "\x1b;[?1049l"))
      (terminal-write! restored (bytes "\x1b;[?1049l"))
      (check-equal? (terminal->string source) (terminal->string restored))
      (check-equal? (terminal-total-rows source) (terminal-total-rows restored))
      (terminal-close! restored))
    (terminal-write! source (bytevector 27 91 51 49))
    (let* ([encoded (terminal->snapshot source)] [restored (snapshot->terminal encoded 4)])
      (check (not (terminal-vt-ground? restored)))
      (check-raises (ghostty-error-with 'limit-exceeded) (snapshot->terminal encoded 3))
      (terminal-write! source (bytevector 109 65))
      (terminal-write! restored (bytevector 109 65))
      (check-equal? (terminal->string source) (terminal->string restored))
      (terminal-close! restored))))

(test "bad snapshots are refused"
  (with-terminal (source 10 2)
    (terminal-write! source (bytevector 65))
    (let* ([encoded (terminal->snapshot source)] [n (bytevector-length encoded)])
      (for-each
        (lambda (bad) (check-raises (ghostty-error-with 'invalid-value) (snapshot->terminal bad)))
        (list (bytevector) (head encoded 9) (head encoded (- n 10)) (append-byte encoded 88)))
      (let ([corrupt (bytevector-copy encoded)])
        (bytevector-u8-set! corrupt (- n 1) (fxlogxor 255 (bytevector-u8-ref corrupt (- n 1))))
        (check-raises ghostty-error? (snapshot->terminal corrupt))))
    (terminal-write! source (bytevector 27 91))
    (check-raises (ghostty-error-with 'invalid-value) (terminal->snapshot source))
    (check-raises assertion-violation? (snapshot->terminal "bytes"))))

(test "incremental decoding: ready, then history a page at a time"
  (let* ([encoded (history-snapshot)] [reference (snapshot->terminal encoded)]
         [d (make-snapshot-decoder (append-byte encoded 90))])
    (check-equal? 0 (snapshot-decoder-source-offset d))
    (check-raises assertion-violation? (snapshot-decoder-next! d))
    (let ([t (snapshot-decoder-ready! d)])
      (check-raises assertion-violation? (snapshot-decoder-ready! d))
      (collect)
      (let-values ([(primary alternate) (snapshot-decoder-history d)])
        (check (> primary 0))
        (check-equal? #f alternate))
      (let ([first (snapshot-decoder-next! d)])
        (check-equal? 'primary (snapshot-progress-screen first))
        (check (> (snapshot-progress-rows first) 0))
        (let loop ([pages 1])
          (let ([next (snapshot-decoder-next! d)])
            (if next
                (loop (+ pages 1))
                (check (>= pages 2))))))
      (check-equal? #f (snapshot-decoder-next! d))
      (check-equal? (bytevector-length encoded) (snapshot-decoder-source-offset d))
      (check-equal? (terminal->snapshot reference) (terminal->snapshot t))
      (snapshot-decoder-close! d)
      (snapshot-decoder-close! d)
      (check (snapshot-decoder-closed? d))
      (check-raises assertion-violation? (snapshot-decoder-next! d))
      (terminal-write! t (bytevector 88))
      (terminal-close! t))
    (terminal-close! reference)))

(test "decoder options retain the continuation and compress history"
  (with-terminal (source 12 3)
    (terminal-set-continuation-max-bytes! source 64)
    (terminal-write! source (bytevector 27 91 51 49))
    (let* ([encoded (terminal->snapshot source)]
           [plain (snapshot->terminal encoded 8)]
           [kept (snapshot->terminal encoded 8 (snapshot-decoder-options retain-continuation))])
      (check-equal? 0 (terminal-continuation-max-bytes plain))
      (check-equal? 8 (terminal-continuation-max-bytes kept))
      (check-equal? (bytevector 27 91 51 49) (terminal-continuation kept))
      (terminal-close! plain)
      (terminal-close! kept)))
  (let* ([encoded (history-snapshot)]
         [compressed-pages
          (lambda (t)
            (let ([pages (memory-usage-primary-compressed-pages (terminal-memory-usage t))])
              (terminal-close! t)
              pages))])
    (check-equal? 0 (compressed-pages (snapshot->terminal encoded)))
    (check (> (compressed-pages
                (snapshot->terminal encoded #f (snapshot-decoder-options compress-history)))
              0))
    (let* ([d (make-snapshot-decoder encoded #f (snapshot-decoder-options compress-history))]
           [t (snapshot-decoder-ready! d)])
      (let loop () (when (snapshot-decoder-next! d) (loop)))
      (snapshot-decoder-close! d)
      (check (> (compressed-pages t) 0))))
  (check-raises assertion-violation? (make-snapshot-decoder (bytevector) #f (format-options trim)))
  (check-raises assertion-violation? (snapshot->terminal (bytevector) #f '(compress-history))))

(test "a resize after ready skips history that no longer fits"
  (let* ([d (make-snapshot-decoder (history-snapshot))] [t (snapshot-decoder-ready! d)])
    (terminal-resize! t 21 3)
    (terminal-write! t (bytevector 88))
    (let loop ([pages 0])
      (let ([progress (snapshot-decoder-next! d)])
        (if progress
            (begin
              (check-equal? 0 (snapshot-progress-rows progress))
              (loop (+ pages 1)))
            (check (>= pages 2)))))
    (snapshot-decoder-close! d)
    (terminal-close! t)))

(test "closing a terminal first closes the decoder borrowing it"
  (let* ([d (make-snapshot-decoder (history-snapshot))] [t (snapshot-decoder-ready! d)])
    (snapshot-decoder-next! d)
    (terminal-close! t)
    (check (snapshot-decoder-closed? d))
    (check-raises assertion-violation? (snapshot-decoder-next! d))
    (snapshot-decoder-close! d))
  (let ([t (let ([d (make-snapshot-decoder (history-snapshot))]) (snapshot-decoder-ready! d))])
    (collect)
    (collect)
    (terminal-write! t (bytevector 89))
    (check (string? (terminal->string t)))
    (terminal-close! t))
  (let ([d (make-snapshot-decoder (history-snapshot))])
    (snapshot-decoder-ready! d)
    (collect)
    (snapshot-decoder-next! d)
    (snapshot-decoder-close! d)
    (collect)))

(test-exit)
