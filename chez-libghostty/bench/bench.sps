(import (chezscheme) (ghostty))

(define (bytes s) (string->utf8 s))

(define (measure label batch)
  (batch)
  (let loop ([i 0] [results '()])
    (if (< i 3)
        (let ([gc0 (collections)] [alloc0 (sstats-bytes (statistics))]
              [cpu0 (cpu-time)] [real0 (current-time 'time-monotonic)])
          (batch)
          (let ([real (time-difference (current-time 'time-monotonic) real0)])
            (loop (+ i 1)
              (cons (list (+ (time-second real) (/ (time-nanosecond real) 1e9))
                          (/ (- (cpu-time) cpu0) 1000.0)
                          (- (sstats-bytes (statistics)) alloc0)
                          (- (collections) gc0))
                    results))))
        (let ([results (reverse results)])
          (printf "~a\n  real ~{~,6f~^ / ~} s; cpu ~{~,3f~^ / ~} s; allocated ~{~a~^ / ~}; collections ~{~a~^ / ~}\n"
            label (map car results) (map cadr results) (map caddr results) (map cadddr results))))))

(define (repeat n thunk)
  (do ([i 0 (+ i 1)]) ((= i n)) (thunk)))

(define change (bytes "\x1b;[HX"))

(define (render-batch t rs read)
  (lambda ()
    (repeat 100
      (lambda ()
        (terminal-write! t change)
        (render-state-update! rs t)
        (read rs)
        (render-state-clean! rs)))))

(define (screen-workloads t rs label)
  (measure (format "~a: all rows as styled runs, 100 frames" label)
    (render-batch t rs render-state-rows))
  (measure (format "~a: changed rows as styled runs, 100 frames" label)
    (render-batch t rs render-state-dirty-rows)))

(define (base64 bv)
  (let ([alphabet "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"]
        [n (bytevector-length bv)]
        [out (open-output-string)])
    (do ([i 0 (+ i 3)]) ((>= i n) (get-output-string out))
      (let* ([b0 (bytevector-u8-ref bv i)]
             [b1 (if (< (+ i 1) n) (bytevector-u8-ref bv (+ i 1)) 0)]
             [b2 (if (< (+ i 2) n) (bytevector-u8-ref bv (+ i 2)) 0)]
             [word (+ (* b0 65536) (* b1 256) b2)])
        (write-char (string-ref alphabet (fxsrl word 18)) out)
        (write-char (string-ref alphabet (fxand (fxsrl word 12) 63)) out)
        (write-char (if (< (+ i 1) n) (string-ref alphabet (fxand (fxsrl word 6) 63)) #\=) out)
        (write-char (if (< (+ i 2) n) (string-ref alphabet (fxand word 63)) #\=) out)))))

(define (terminal-benchmarks)
  (let ([t (make-terminal 80 24)] [rs (make-render-state)])
    (let ([payload (make-bytevector 8192 120)])
      (measure "8192-byte writes, 100 writes"
        (lambda () (repeat 100 (lambda () (terminal-write! t payload))))))
    (screen-workloads t rs "80x24")
    (terminal-resize! t 160 48 8 16)
    (terminal-write! t (make-bytevector 7680 120))
    (screen-workloads t rs "160x48")
    (terminal-write! t
      (bytes (string-append "\x1b;[H"
               (apply string-append
                 (map (lambda (i) (format "\x1b;[3~am~a" (modulo i 8) (make-string 20 #\y)))
                      (iota 384)))
               "\x1b;[0m")))
    (measure "160x48 eight colors per row: all rows as styled runs, 100 frames"
      (render-batch t rs render-state-rows))
    (terminal-write! t
      (bytes (string-append "\x1b;[H\x1b;_Ga=T,t=d,f=32,i=1,p=1,s=128,v=128,c=16,r=8;"
               (base64 (make-bytevector 65536 0)) "\x1b;\\")))
    (measure "160x48 plus a 65536-byte Kitty image: rows, placements and pixels, 100 frames"
      (render-batch t rs
        (lambda (rs)
          (render-state-rows rs)
          (for-each (lambda (p) (terminal-kitty-image t (kitty-placement-image-id p)))
            (terminal-kitty-placements t)))))
    (render-state-close! rs)
    (terminal-close! t)))

(define (input-benchmarks)
  (let ([k (make-key-encoder)] [m (make-mouse-encoder)] [t (make-terminal 80 24)]
        [key (make-key-event 'press 'a (key-modifiers ctrl) "a")]
        [click (make-mouse-event 'press 'left 15 25)])
    (mouse-encoder-set-geometry! m (make-mouse-geometry 800 480 10 20 0 0 0 0))
    (terminal-write! t (bytes "\x1b;[?1000h\x1b;[?1006h"))
    (measure "key encodings, 100000"
      (lambda () (repeat 100000 (lambda () (key-encoder-encode k key)))))
    (measure "mouse encodings, 100000"
      (lambda () (repeat 100000 (lambda () (mouse-encoder-encode m click)))))
    (measure "mouse syncs from a terminal, 100000"
      (lambda () (repeat 100000 (lambda () (mouse-encoder-sync! m t)))))
    (key-encoder-close! k)
    (mouse-encoder-close! m)
    (terminal-close! t)))

(define (utility-benchmarks)
  (let ([black (make-rgb 0 0 0)] [white (make-rgb 255 255 255)]
        [paste (make-bytevector 4096 97)]
        [title (bytes (string-append "2;" (make-string 254 #\t)))])
    (measure "generated palettes, 1000"
      (lambda () (repeat 1000 (lambda () (color-generate-palette black white)))))
    (measure "bracketed 4096-byte pastes, 1000"
      (lambda () (repeat 1000 (lambda () (paste-encode-bracketed paste)))))
    (measure "first-grapheme widths, 10000"
      (lambda () (repeat 10000 (lambda () (unicode-grapheme-width '(#x1f469 #x200d #x1f4bb 97))))))
    (measure "OSC titles of 256 bytes, 10000"
      (lambda () (repeat 10000 (lambda () (osc-parse title)))))
    (measure "SGR sequences of four attributes, 10000"
      (lambda () (repeat 10000 (lambda () (sgr-parse '(1 4 38 2 10 20 30 48 5 200))))))))

(terminal-benchmarks)
(input-benchmarks)
(utility-benchmarks)
