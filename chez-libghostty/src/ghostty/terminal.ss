(define-record-type (terminal new-terminal terminal?)
  (nongenerative)
  (sealed #t)
  (opaque #t)
  (fields native effects))

(define (check-terminal who t) (check who (terminal? t) t "a terminal"))

(define (terminal-lock t) (native-lock (terminal-native t)))

;; A read may run inside the terminal's own callbacks, where libghostty-vt
;; allows reading the terminal.
(define-syntax with-terminal
  (syntax-rules ()
    [(_ who t-expr (pointer) body ...)
     (let ([t t-expr])
       (check-terminal who t)
       (with-native who (terminal-native t) "terminal" (pointer) body ...))]))

;; A change may not run inside the terminal's callbacks: libghostty-vt is in
;; the middle of processing input there.
(define-syntax with-terminal-change
  (syntax-rules ()
    [(_ who t-expr (pointer) body ...)
     (let ([t t-expr])
       (with-terminal who t (pointer)
         (when (lock-busy? (terminal-lock t))
           (assertion-violationf who "cannot change a terminal inside one of its callbacks"))
         body ...))]))

;; A handler's raise is held until the native call returns, so that it
;; never unwinds through C.
(define (call-with-terminal-effects who t proc)
  (check-terminal who t)
  (let ([e (terminal-effects t)] [failure #f])
    (let ([result
           (with-terminal-change who t (pointer)
             (let ([lock (terminal-lock t)])
               (dynamic-wind
                 (lambda () (lock-busy?-set! lock #t))
                 (lambda () (parameterize ([in-terminal-callback? #t]) (proc pointer)))
                 (lambda ()
                   (lock-busy?-set! lock #f)
                   (set! failure (effects-failure e))
                   (effects-failure-set! e #f)
                   (free-effect-replies! e)))))])
      (when failure (raise (car failure)))
      result)))

(define (make-terminal columns rows)
  (define who 'make-terminal)
  (check who (and (u16? columns) (fx> columns 0)) columns "an integer from 1 to 65535")
  (check who (and (u16? rows) (fx> rows 0)) rows "an integer from 1 to 65535")
  (release-dropped-natives!)
  (let ([out (pointer-out)])
    (check-result who (ghostty-terminal-new 0 out columns rows) columns rows)
    (adopt-terminal (pointer-in out))))

(define (adopt-terminal pointer)
  (let* ([lock (make-lock #t)]
         [effects (make-effects lock)]
         [id (register-effects! effects)]
         [native (make-native pointer
                   (lambda (pointer)
                     (unregister-effects! id)
                     (ghostty-terminal-free pointer))
                   lock)])
    (ghostty-terminal-set pointer (GhosttyTerminalOption userdata) id)
    (register-native! (new-terminal native effects) native)))

(define (terminal-close! t)
  (check-terminal 'terminal-close! t)
  (close-native! 'terminal-close! (terminal-native t) "terminal"))

(define (terminal-closed? t)
  (check-terminal 'terminal-closed? t)
  (native-closed? (terminal-native t)))

;; The input stays locked so that a collection during a callback cannot
;; move it while libghostty-vt reads it.
(define (call-with-locked-slice bv start proc)
  (dynamic-wind
    (lambda () (lock-object bv))
    (lambda () (proc (fx+ (object->reference-address bv) start)))
    (lambda () (unlock-object bv))))

(define terminal-write!
  (case-lambda
    [(t bytes) (terminal-write! t bytes 0 (if (bytevector? bytes) (bytevector-length bytes) 0))]
    [(t bytes start end)
     (define who 'terminal-write!)
     (check-terminal who t)
     (check-slice who bytes start end)
     (call-with-terminal-effects who t
       (lambda (pointer)
         (call-with-locked-slice bytes start
           (lambda (address) (ghostty-terminal-vt-write pointer address (fx- end start))))))]))

(define terminal-write-until-ground!
  (case-lambda
    [(t bytes)
     (terminal-write-until-ground! t bytes 0 (if (bytevector? bytes) (bytevector-length bytes) 0))]
    [(t bytes start end)
     (define who 'terminal-write-until-ground!)
     (check-terminal who t)
     (check-slice who bytes start end)
     (let ([consumed (make-immobile-bytevector 8 0)])
       (let ([code
              (call-with-terminal-effects who t
                (lambda (pointer)
                  (call-with-locked-slice bytes start
                    (lambda (address)
                      (ghostty-terminal-vt-write-until-ground pointer address (fx- end start)
                        consumed)))))])
         (unless (or (fx= code (GhosttyResult success)) (fx= code (GhosttyResult no-value)))
           (raise-result who code))
         (values (bytevector-u64-native-ref consumed 0) (fx= code (GhosttyResult success)))))]))

(define (terminal-reset! t)
  (call-with-terminal-effects 'terminal-reset! t
    (lambda (pointer) (ghostty-terminal-reset pointer))))

(define terminal-resize!
  (case-lambda
    [(t columns rows) (terminal-resize! t columns rows 0 0)]
    [(t columns rows cell-width cell-height)
     (define who 'terminal-resize!)
     (check who (and (u16? columns) (fx> columns 0)) columns "an integer from 1 to 65535")
     (check who (and (u16? rows) (fx> rows 0)) rows "an integer from 1 to 65535")
     (check-u32 who cell-width)
     (check-u32 who cell-height)
     (check-result who
       (call-with-terminal-effects who t
         (lambda (pointer) (ghostty-terminal-resize pointer columns rows cell-width cell-height))))]))

(define (terminal-read who t key size ref)
  (with-terminal who t (pointer)
    (let ([out (make-bytevector size 0)])
      (check-result who (ghostty-terminal-get pointer key out))
      (ref out))))

(define (terminal-read/optional who t key size ref)
  (with-terminal who t (pointer)
    (let* ([out (make-bytevector size 0)]
           [code (ghostty-terminal-get pointer key out)])
      (cond
        [(fx= code (GhosttyResult success)) (ref out)]
        [(fx= code (GhosttyResult no-value)) #f]
        [else (raise-result who code)]))))

(define (u16-ref bv) (bytevector-u16-native-ref bv 0))
(define (u32-ref bv) (bytevector-u32-native-ref bv 0))
(define (u64-ref bv) (bytevector-u64-native-ref bv 0))
(define (bool-ref bv) (not (fx= 0 (bytevector-u8-ref bv 0))))
(define (int-ref bv) (bytevector-s32-native-ref bv 0))
(define (borrowed-string-ref bv) (utf8->string (borrowed-bytes bv 0)))

(define (terminal-columns t) (terminal-read 'terminal-columns t (GhosttyTerminalData cols) 2 u16-ref))
(define (terminal-rows t) (terminal-read 'terminal-rows t (GhosttyTerminalData rows) 2 u16-ref))
(define (terminal-pixel-width t)
  (terminal-read 'terminal-pixel-width t (GhosttyTerminalData width-px) 4 u32-ref))
(define (terminal-pixel-height t)
  (terminal-read 'terminal-pixel-height t (GhosttyTerminalData height-px) 4 u32-ref))
(define (terminal-total-rows t)
  (terminal-read 'terminal-total-rows t (GhosttyTerminalData total-rows) 8 u64-ref))
(define (terminal-scrollback-rows t)
  (terminal-read 'terminal-scrollback-rows t (GhosttyTerminalData scrollback-rows) 8 u64-ref))
(define (terminal-title t)
  (terminal-read 'terminal-title t (GhosttyTerminalData title) 16 borrowed-string-ref))
(define (terminal-pwd t)
  (terminal-read 'terminal-pwd t (GhosttyTerminalData pwd) 16 borrowed-string-ref))
(define (terminal-mouse-tracking? t)
  (terminal-read 'terminal-mouse-tracking? t (GhosttyTerminalData mouse-tracking) 1 bool-ref))
(define (terminal-viewport-active? t)
  (terminal-read 'terminal-viewport-active? t (GhosttyTerminalData viewport-active) 1 bool-ref))
(define (terminal-vt-ground? t)
  (terminal-read 'terminal-vt-ground? t (GhosttyTerminalData vt-ground) 1 bool-ref))
(define (terminal-vt-processing-error? t)
  (terminal-read 'terminal-vt-processing-error? t (GhosttyTerminalData vt-processing-error) 1
    bool-ref))
(define (terminal-continuation-max-bytes t)
  (terminal-read 'terminal-continuation-max-bytes t (GhosttyTerminalData continuation-max-bytes) 8
    u64-ref))
(define (terminal-scrollback-max-bytes t)
  (terminal-read/optional 'terminal-scrollback-max-bytes t
    (GhosttyTerminalData scrollback-max-bytes) 8 u64-ref))
(define (terminal-scrollback-max-lines t)
  (terminal-read/optional 'terminal-scrollback-max-lines t
    (GhosttyTerminalData scrollback-max-lines) 8 u64-ref))
(define (terminal-clipboard-write-max-bytes t)
  (terminal-read 'terminal-clipboard-write-max-bytes t
    (GhosttyTerminalData clipboard-write-max-bytes) 8 u64-ref))
(define (terminal-kitty-image-storage-limit t)
  (terminal-read/optional 'terminal-kitty-image-storage-limit t
    (GhosttyTerminalData kitty-image-storage-limit) 8 u64-ref))
(define (terminal-kitty-image-file-medium? t)
  (terminal-read/optional 'terminal-kitty-image-file-medium? t
    (GhosttyTerminalData kitty-image-medium-file) 1 bool-ref))
(define (terminal-kitty-image-shared-memory-medium? t)
  (terminal-read/optional 'terminal-kitty-image-shared-memory-medium? t
    (GhosttyTerminalData kitty-image-medium-shared-mem) 1 bool-ref))
(define (terminal-kitty-image-temp-directory t)
  (let ([dir (terminal-read/optional 'terminal-kitty-image-temp-directory t
               (GhosttyTerminalData kitty-image-medium-temp-file) 16 borrowed-string-ref)])
    (and dir (not (string=? dir "")) dir)))

(define (terminal-active-screen t)
  (terminal-read 'terminal-active-screen t (GhosttyTerminalData active-screen) 4
    (lambda (bv) (c-enum->symbol GhosttyTerminalScreen (int-ref bv)))))

(define (terminal-mouse-shape t)
  (terminal-read 'terminal-mouse-shape t (GhosttyTerminalData mouse-shape) 4
    (lambda (bv) (c-enum->symbol GhosttyMouseShape (int-ref bv)))))

(define-enumeration kitty-key-flag
  (disambiguate report-events report-alternates report-all report-associated)
  kitty-key-flags)

(define (bits->enum-set set bits)
  (let ([names (enum-set->list (enum-set-universe set))])
    ((enum-set-constructor set)
     (let loop ([names names] [bit 0] [acc '()])
       (if (null? names)
           acc
           (loop (cdr names) (fx+ bit 1)
             (if (fxbit-set? bits bit) (cons (car names) acc) acc)))))))

(define (enum-set->bits set)
  (let ([index (enum-set-indexer set)])
    (fold-left (lambda (bits name) (fxior bits (fxsll 1 (index name)))) 0 (enum-set->list set))))

(define (terminal-kitty-keyboard-flags t)
  (terminal-read 'terminal-kitty-keyboard-flags t (GhosttyTerminalData kitty-keyboard-flags) 1
    (lambda (bv) (bits->enum-set (kitty-key-flags) (bytevector-u8-ref bv 0)))))

(define-value-record cursor (fields x y pending-wrap? visible? at-prompt?))

(define (terminal-cursor t)
  (define who 'terminal-cursor)
  (with-terminal who t (pointer)
    (let ([read (lambda (key size ref)
                  (let ([out (make-bytevector size 0)])
                    (check-result who (ghostty-terminal-get pointer key out))
                    (ref out)))])
      (make-cursor
        (read (GhosttyTerminalData cursor-x) 2 u16-ref)
        (read (GhosttyTerminalData cursor-y) 2 u16-ref)
        (read (GhosttyTerminalData cursor-pending-wrap) 1 bool-ref)
        (read (GhosttyTerminalData cursor-visible) 1 bool-ref)
        (read (GhosttyTerminalData cursor-at-prompt) 1 bool-ref)))))

(define (terminal-cursor-pen t)
  (with-terminal 'terminal-cursor-pen t (pointer)
    (let ([out (c-sized-struct GhosttyStyle)])
      (check-result 'terminal-cursor-pen
        (ghostty-terminal-get pointer (GhosttyTerminalData cursor-style) out))
      (style-at out))))

(define-value-record scrollbar (fields total offset length))

(define (terminal-scrollbar t)
  (terminal-read 'terminal-scrollbar t (GhosttyTerminalData scrollbar)
    (ftype-sizeof GhosttyTerminalScrollbar)
    (lambda (bv)
      (make-scrollbar (c-ref GhosttyTerminalScrollbar (total) bv)
                      (c-ref GhosttyTerminalScrollbar (offset) bv)
                      (c-ref GhosttyTerminalScrollbar (len) bv)))))

(define-value-record color-theme (fields foreground background cursor palette))

(define (read-terminal-colors who t foreground background cursor palette)
  (with-terminal who t (pointer)
    (let ([color (lambda (key)
                   (let* ([out (c-struct GhosttyColorRgb)]
                          [code (ghostty-terminal-get pointer key out)])
                     (cond
                       [(fx= code (GhosttyResult success)) (rgb-at out 0)]
                       [(fx= code (GhosttyResult no-value)) #f]
                       [else (raise-result who code)])))]
          [palette-out (make-bytevector (fx* 3 256) 0)])
      (check-result who (ghostty-terminal-get pointer palette palette-out))
      (make-color-theme (color foreground) (color background) (color cursor)
        (palette-at palette-out 0)))))

(define (terminal-colors t)
  (read-terminal-colors 'terminal-colors t
    (GhosttyTerminalData color-foreground)
    (GhosttyTerminalData color-background)
    (GhosttyTerminalData color-cursor)
    (GhosttyTerminalData color-palette)))

(define (terminal-default-colors t)
  (read-terminal-colors 'terminal-default-colors t
    (GhosttyTerminalData color-foreground-default)
    (GhosttyTerminalData color-background-default)
    (GhosttyTerminalData color-cursor-default)
    (GhosttyTerminalData color-palette-default)))

(define-value-record memory-usage
  (fields compression-supported?
    primary-pages primary-virtual-bytes primary-resident-bytes primary-compressed-pages
    primary-compressed-bytes primary-image-bytes
    alternate-pages alternate-virtual-bytes alternate-resident-bytes alternate-compressed-pages
    alternate-compressed-bytes alternate-image-bytes))

(define (terminal-memory-usage t)
  (with-terminal 'terminal-memory-usage t (pointer)
    (let ([out (c-sized-struct GhosttyTerminalMemoryUsage)])
      (check-result 'terminal-memory-usage
        (ghostty-terminal-get pointer (GhosttyTerminalData memory-usage) out))
      (let-syntax ([field (syntax-rules ()
                            [(_ name) (c-ref GhosttyTerminalMemoryUsage (name) out)])])
        (make-memory-usage
          (field compression_supported)
          (field primary_pages) (field primary_virtual_bytes) (field primary_resident_bytes)
          (field primary_compressed_pages) (field primary_compressed_bytes)
          (field primary_image_bytes)
          (field alternate_pages) (field alternate_virtual_bytes)
          (field alternate_resident_bytes) (field alternate_compressed_pages)
          (field alternate_compressed_bytes) (field alternate_image_bytes))))))

(define terminal-mode-table
  '((kam 2 . #t) (insert 4 . #t) (srm 12 . #t) (linefeed 20 . #t)
    (decckm 1 . #f) (deccolm 3 . #f) (slow-scroll 4 . #f) (reverse-colors 5 . #f)
    (origin 6 . #f) (wraparound 7 . #f) (autorepeat 8 . #f) (x10-mouse 9 . #f)
    (cursor-blinking 12 . #f) (cursor-visible 25 . #f) (enable-mode-3 40 . #f)
    (reverse-wrap 45 . #f) (alt-screen-legacy 47 . #f) (keypad-keys 66 . #f)
    (backarrow-key-mode 67 . #f) (left-right-margin 69 . #f) (normal-mouse 1000 . #f)
    (button-mouse 1002 . #f) (any-mouse 1003 . #f) (focus-event 1004 . #f)
    (utf8-mouse 1005 . #f) (sgr-mouse 1006 . #f) (alt-scroll 1007 . #f)
    (urxvt-mouse 1015 . #f) (sgr-pixels-mouse 1016 . #f) (numlock-keypad 1035 . #f)
    (alt-esc-prefix 1036 . #f) (alt-sends-esc 1039 . #f) (reverse-wrap-ext 1045 . #f)
    (alt-screen 1047 . #f) (save-cursor 1048 . #f) (alt-screen-save 1049 . #f)
    (bracketed-paste 2004 . #f) (sync-output 2026 . #f) (grapheme-cluster 2027 . #f)
    (color-scheme-report 2031 . #f) (visibility-report 2033 . #f)
    (in-band-resize 2048 . #f) (paste-events 5522 . #f)))

(define (terminal-modes) (map car terminal-mode-table))

(define (mode->c who mode)
  (let ([entry (assq mode terminal-mode-table)])
    (unless entry (assertion-violationf who "~s is not a terminal mode" mode))
    (fxior (cadr entry) (if (cddr entry) #x8000 0))))

(define (mode-config who mode value)
  (let ([config (c-struct GhosttyTerminalModeConfig)])
    (c-set! GhosttyTerminalModeConfig (mode) config (mode->c who mode))
    (c-set! GhosttyTerminalModeConfig (value) config value)
    config))

(define (terminal-mode-enabled? t mode)
  (define who 'terminal-mode-enabled?)
  (let ([config (mode-config who mode #f)])
    (with-terminal who t (pointer)
      (check-result who (ghostty-terminal-get pointer (GhosttyTerminalData mode) config) mode)
      (c-ref GhosttyTerminalModeConfig (value) config))))

(define (set-mode! who t option mode value)
  (check-boolean who value)
  (let ([config (mode-config who mode value)])
    (with-terminal-change who t (pointer)
      (check-result who (ghostty-terminal-set-value pointer option config) mode))))

(define (terminal-set-mode! t mode enabled?)
  (set-mode! 'terminal-set-mode! t (GhosttyTerminalOption mode) mode enabled?))

(define (terminal-set-mode-default! t mode enabled?)
  (set-mode! 'terminal-set-mode-default! t (GhosttyTerminalOption mode-default) mode enabled?))

(define (set-option! who t option value)
  (with-terminal-change who t (pointer)
    (check-result who
      (if value
          (ghostty-terminal-set-value pointer option value)
          (ghostty-terminal-set pointer option 0)))))

(define (size-option who x)
  (check who (or (not x) (size? x)) x "an unsigned 64-bit integer or #f")
  (and x (let ([bv (make-bytevector 8)]) (bytevector-u64-native-set! bv 0 x) bv)))

(define (bool-option who x)
  (check-boolean who x)
  (bytevector (if x 1 0)))

;; The string's bytes are reached through a pointer inside the option, so
;; they must neither move nor be reclaimed before the call returns.
(define (call-with-string-option who x proc)
  (check who (or (not x) (string? x)) x "a string or #f")
  (if x
      (let ([bytes (immobile-copy (string->utf8 x))] [s (c-struct GhosttyString)])
        (c-set! GhosttyString (ptr) s (immobile-address bytes))
        (c-set! GhosttyString (len) s (bytevector-length bytes))
        (let ([result (proc s)])
          (keep-live bytes)
          result))
      (proc #f)))

(define (set-string-option! who t option x)
  (call-with-string-option who x (lambda (s) (set-option! who t option s))))

(define (terminal-set-title! t title)
  (set-string-option! 'terminal-set-title! t (GhosttyTerminalOption title) title))

(define (terminal-set-pwd! t pwd)
  (set-string-option! 'terminal-set-pwd! t (GhosttyTerminalOption pwd) pwd))

(define (terminal-set-terminfo-name! t name)
  (set-string-option! 'terminal-set-terminfo-name! t (GhosttyTerminalOption terminfo-name) name))

(define (color-option who color)
  (check who (or (not color) (rgb? color)) color "an rgb color or #f")
  (and color (rgb->c color)))

(define (terminal-set-default-foreground! t color)
  (set-option! 'terminal-set-default-foreground! t (GhosttyTerminalOption color-foreground)
    (color-option 'terminal-set-default-foreground! color)))

(define (terminal-set-default-background! t color)
  (set-option! 'terminal-set-default-background! t (GhosttyTerminalOption color-background)
    (color-option 'terminal-set-default-background! color)))

(define (terminal-set-default-cursor-color! t color)
  (set-option! 'terminal-set-default-cursor-color! t (GhosttyTerminalOption color-cursor)
    (color-option 'terminal-set-default-cursor-color! color)))

(define (terminal-set-default-palette! t palette)
  (define who 'terminal-set-default-palette!)
  (when palette (check-palette who palette))
  (set-option! who t (GhosttyTerminalOption color-palette) (and palette (palette->c palette))))

(define (terminal-set-scrollback-max-bytes! t limit)
  (set-option! 'terminal-set-scrollback-max-bytes! t (GhosttyTerminalOption scrollback-max-bytes)
    (size-option 'terminal-set-scrollback-max-bytes! limit)))

(define (terminal-set-scrollback-max-lines! t limit)
  (set-option! 'terminal-set-scrollback-max-lines! t (GhosttyTerminalOption scrollback-max-lines)
    (size-option 'terminal-set-scrollback-max-lines! limit)))

(define (terminal-set-continuation-max-bytes! t limit)
  (define who 'terminal-set-continuation-max-bytes!)
  (check-size who limit)
  (set-option! who t (GhosttyTerminalOption continuation-max-bytes) (size-option who limit)))

(define (terminal-set-apc-max-bytes! t limit)
  (set-option! 'terminal-set-apc-max-bytes! t (GhosttyTerminalOption apc-max-bytes)
    (size-option 'terminal-set-apc-max-bytes! limit)))

(define (terminal-set-kitty-graphics-max-bytes! t limit)
  (set-option! 'terminal-set-kitty-graphics-max-bytes! t (GhosttyTerminalOption apc-max-bytes-kitty)
    (size-option 'terminal-set-kitty-graphics-max-bytes! limit)))

(define (terminal-set-unknown-max-bytes! t limit)
  (define who 'terminal-set-unknown-max-bytes!)
  (check-size who limit)
  (set-option! who t (GhosttyTerminalOption unknown-max-bytes) (size-option who limit)))

(define (terminal-set-clipboard-write-max-bytes! t limit)
  (set-option! 'terminal-set-clipboard-write-max-bytes! t
    (GhosttyTerminalOption clipboard-write-max-bytes)
    (size-option 'terminal-set-clipboard-write-max-bytes! limit)))

(define (terminal-set-kitty-image-storage-limit! t limit)
  (define who 'terminal-set-kitty-image-storage-limit!)
  (check-size who limit)
  (set-option! who t (GhosttyTerminalOption kitty-image-storage-limit) (size-option who limit)))

(define (terminal-set-kitty-image-file-medium! t enabled?)
  (set-option! 'terminal-set-kitty-image-file-medium! t
    (GhosttyTerminalOption kitty-image-medium-file)
    (bool-option 'terminal-set-kitty-image-file-medium! enabled?)))

(define (terminal-set-kitty-image-shared-memory-medium! t enabled?)
  (set-option! 'terminal-set-kitty-image-shared-memory-medium! t
    (GhosttyTerminalOption kitty-image-medium-shared-mem)
    (bool-option 'terminal-set-kitty-image-shared-memory-medium! enabled?)))

(define (terminal-set-kitty-image-temp-directory! t directory)
  (set-string-option! 'terminal-set-kitty-image-temp-directory! t
    (GhosttyTerminalOption kitty-image-medium-temp-file) directory))

(define (terminal-set-default-cursor-style! t style)
  (define who 'terminal-set-default-cursor-style!)
  (set-option! who t (GhosttyTerminalOption default-cursor-style)
    (and style
         (let ([bv (make-bytevector 4)])
           (bytevector-s32-native-set! bv 0 (symbol->c-enum GhosttyTerminalCursorStyle style who))
           bv))))

(define (terminal-set-default-cursor-blink! t blink?)
  (set-option! 'terminal-set-default-cursor-blink! t (GhosttyTerminalOption default-cursor-blink)
    (bool-option 'terminal-set-default-cursor-blink! blink?)))

(define (terminal-set-glyph-protocol! t enabled?)
  (set-option! 'terminal-set-glyph-protocol! t (GhosttyTerminalOption glyph-protocol)
    (bool-option 'terminal-set-glyph-protocol! enabled?)))

(define (terminal-set-title-report! t enabled?)
  (set-option! 'terminal-set-title-report! t (GhosttyTerminalOption title-report)
    (bool-option 'terminal-set-title-report! enabled?)))

(define (terminal-set-resize-pull-scrollback! t enabled?)
  (set-option! 'terminal-set-resize-pull-scrollback! t
    (GhosttyTerminalOption resize-pull-scrollback)
    (bool-option 'terminal-set-resize-pull-scrollback! enabled?)))

(define (terminal-set-checksum-report! t enabled?)
  (set-option! 'terminal-set-checksum-report! t (GhosttyTerminalOption xt-checksum-report)
    (bool-option 'terminal-set-checksum-report! enabled?)))

(define (terminal-set-checksum-extension! t bits)
  (define who 'terminal-set-checksum-extension!)
  (check who (and (fixnum? bits) (fx<= 0 bits 31)) bits "an integer from 0 to 31")
  (set-option! who t (GhosttyTerminalOption xt-checksum-extension) (bytevector bits)))

(define (scroll-viewport! who t tag value)
  (let ([behavior (c-struct GhosttyTerminalScrollViewport)])
    (c-set! GhosttyTerminalScrollViewport (tag) behavior tag)
    (c-set! GhosttyTerminalScrollViewport (value delta) behavior value)
    (with-terminal-change who t (pointer)
      (ghostty-terminal-scroll-viewport pointer (c-ptr behavior)))))

(define (terminal-scroll-to-top! t)
  (scroll-viewport! 'terminal-scroll-to-top! t (GhosttyTerminalScrollViewportTag top) 0))

(define (terminal-scroll-to-bottom! t)
  (scroll-viewport! 'terminal-scroll-to-bottom! t (GhosttyTerminalScrollViewportTag bottom) 0))

(define (terminal-scroll-by! t rows)
  (check 'terminal-scroll-by! (s64? rows) rows "a signed 64-bit integer")
  (scroll-viewport! 'terminal-scroll-by! t (GhosttyTerminalScrollViewportTag delta) rows))

(define (terminal-scroll-to-row! t row)
  (check 'terminal-scroll-to-row! (and (s64? row) (>= row 0)) row "an unsigned 63-bit integer")
  (scroll-viewport! 'terminal-scroll-to-row! t (GhosttyTerminalScrollViewportTag row) row))

(define (terminal-scroll-by-prompts! t prompts)
  (check 'terminal-scroll-by-prompts! (s64? prompts) prompts "a signed 64-bit integer")
  (scroll-viewport! 'terminal-scroll-by-prompts! t (GhosttyTerminalScrollViewportTag delta-prompt)
    prompts))

(define (compress! who t mode)
  (with-terminal-change who t (pointer)
    (let ([out (make-bytevector 4 0)])
      (check-result who (ghostty-terminal-compress pointer mode out))
      (c-enum->symbol GhosttyTerminalCompressionResult (int-ref out)))))

(define (terminal-compress! t)
  (compress! 'terminal-compress! t (GhosttyTerminalCompressionMode incremental)))

(define (terminal-compress-fully! t)
  (compress! 'terminal-compress-fully! t (GhosttyTerminalCompressionMode full)))

(define (terminal-compression-activity t)
  (with-terminal 'terminal-compression-activity t (pointer)
    (let ([out (make-bytevector 8 0)])
      (check-result 'terminal-compression-activity
        (ghostty-terminal-compression-activity pointer out))
      (u64-ref out))))

(define-enumeration format-option
  (unwrap trim palette modes scrolling-region tabstops pwd keyboard
   cursor style hyperlink protection kitty-keyboard charsets)
  format-options)

(define (format-kind who kind)
  (symbol->c-enum GhosttyFormatterFormat kind who))

(define (check-format-options who options)
  (check who (enum-set-of? options (format-options))
    options "a set of format options"))

(define (terminal-format t kind options)
  (define who 'terminal-format)
  (check-format-options who options)
  (let ([emit (format-kind who kind)]
        [on? (lambda (name) (enum-set-member? name options))]
        [opts (c-sized-struct GhosttyFormatterTerminalOptions)])
    (c-set! GhosttyFormatterTerminalOptions (emit) opts emit)
    (c-set! GhosttyFormatterTerminalOptions (unwrap) opts (on? 'unwrap))
    (c-set! GhosttyFormatterTerminalOptions (trim) opts (on? 'trim))
    (let-syntax ([extra (syntax-rules ()
                          [(_ (field ...) name)
                           (c-set! GhosttyFormatterTerminalOptions (extra field ...) opts (on? 'name))])])
      (c-set! GhosttyFormatterTerminalOptions (extra size) opts
        (ftype-sizeof GhosttyFormatterTerminalExtra))
      (c-set! GhosttyFormatterTerminalOptions (extra screen size) opts
        (ftype-sizeof GhosttyFormatterScreenExtra))
      (extra (palette) palette)
      (extra (modes) modes)
      (extra (scrolling_region) scrolling-region)
      (extra (tabstops) tabstops)
      (extra (pwd) pwd)
      (extra (keyboard) keyboard)
      (extra (screen cursor) cursor)
      (extra (screen style) style)
      (extra (screen hyperlink) hyperlink)
      (extra (screen protection) protection)
      (extra (screen kitty_keyboard) kitty-keyboard)
      (extra (screen charsets) charsets))
    (with-terminal who t (pointer)
      (let ([formatter (pointer-out)])
        (check-result who (ghostty-formatter-terminal-new 0 formatter pointer (c-ptr opts)))
        (let ([formatter (pointer-in formatter)] [out-ptr (pointer-out)] [out-len (pointer-out)])
          (let ([code (ghostty-formatter-format-alloc formatter 0 out-ptr out-len)])
            (ghostty-formatter-free formatter)
            (check-result who code)
            (utf8->string (take-allocated-bytes out-ptr out-len))))))))

(define (terminal->string t)
  (terminal-format t 'plain (format-options trim)))

(define (terminal-continuation t)
  (with-terminal 'terminal-continuation t (pointer)
    (let ([out-ptr (pointer-out)] [out-len (pointer-out)])
      (check-result 'terminal-continuation
        (ghostty-terminal-continuation-alloc pointer 0 out-ptr out-len))
      (take-allocated-bytes out-ptr out-len))))
