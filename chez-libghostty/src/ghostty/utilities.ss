(define-value-record build-info
  (fields simd? kitty-graphics? tmux-control-mode? optimize version major minor patch pre build))

(define (libghostty-build-info)
  (define who 'libghostty-build-info)
  (let ([get (lambda (key size ref)
               (let ([out (make-bytevector size 0)])
                 (check-result who (ghostty-build-info key out))
                 (ref out)))])
    (make-build-info
      (get (GhosttyBuildInfo simd) 1 bool-ref)
      (get (GhosttyBuildInfo kitty-graphics) 1 bool-ref)
      (get (GhosttyBuildInfo tmux-control-mode) 1 bool-ref)
      (get (GhosttyBuildInfo optimize) 4
        (lambda (bv) (c-enum->symbol GhosttyOptimizeMode (int-ref bv))))
      (get (GhosttyBuildInfo version-string) 16 borrowed-string-ref)
      (get (GhosttyBuildInfo version-major) 8 u64-ref)
      (get (GhosttyBuildInfo version-minor) 8 u64-ref)
      (get (GhosttyBuildInfo version-patch) 8 u64-ref)
      (get (GhosttyBuildInfo version-pre) 16 borrowed-string-ref)
      (get (GhosttyBuildInfo version-build) 16 borrowed-string-ref))))

(define (parse-color who parse text)
  (check who (string? text) text "a string")
  (let ([bytes (string->utf8 text)] [out (c-struct GhosttyColorRgb)])
    (let ([code (parse bytes (bytevector-length bytes) out)])
      (cond
        [(fx= code (GhosttyResult success)) (rgb-at out 0)]
        [(fx= code (GhosttyResult invalid-value)) #f]
        [else (raise-result who code text)]))))

(define (color-parse text) (parse-color 'color-parse ghostty-color-parse text))

(define (color-parse-x11 text) (parse-color 'color-parse-x11 ghostty-color-parse-x11 text))

(define (color-parse-palette-entry text)
  (define who 'color-parse-palette-entry)
  (check who (string? text) text "a string")
  (let ([bytes (string->utf8 text)] [index (make-bytevector 1 0)] [out (c-struct GhosttyColorRgb)])
    (let ([code (ghostty-color-parse-palette-entry bytes (bytevector-length bytes) index out)])
      (cond
        [(fx= code (GhosttyResult success)) (values (bytevector-u8-ref index 0) (rgb-at out 0))]
        [(fx= code (GhosttyResult invalid-value)) (values #f #f)]
        [else (raise-result who code text)]))))

(define (color-default-palette)
  (let ([out (make-bytevector (fx* 3 256) 0)])
    (ghostty-color-palette-default out)
    (palette-at out 0)))

(define (generate-palette who background foreground base preserve harmonious?)
  (check-rgb who background)
  (check-rgb who foreground)
  (when base (check-palette who base))
  (check who (and (list? preserve) (for-all u8? preserve)) preserve "a list of palette indices")
  (let ([mask (c-struct GhosttyColorPaletteMask)] [out (make-bytevector (fx* 3 256) 0)])
    (for-each
      (lambda (i)
        (let ([offset (fx* 8 (fxsrl i 6))])
          (bytevector-u64-native-set! mask offset
            (logor (bytevector-u64-native-ref mask offset) (ash 1 (fxand i 63))))))
      preserve)
    (ghostty-color-palette-generate (and base (palette->c base)) mask (rgb->c background)
      (rgb->c foreground) harmonious? out)
    (palette-at out 0)))

(define color-generate-palette
  (case-lambda
    [(background foreground) (color-generate-palette background foreground #f '())]
    [(background foreground base) (color-generate-palette background foreground base '())]
    [(background foreground base preserve)
     (generate-palette 'color-generate-palette background foreground base preserve #f)]))

(define color-generate-harmonious-palette
  (case-lambda
    [(background foreground) (color-generate-harmonious-palette background foreground #f '())]
    [(background foreground base)
     (color-generate-harmonious-palette background foreground base '())]
    [(background foreground base preserve)
     (generate-palette 'color-generate-harmonious-palette background foreground base preserve
       #t)]))

(define (color-luminance color)
  (check-rgb 'color-luminance color)
  (ghostty-color-luminance (rgb->c color)))

(define (color-perceived-luminance color)
  (check-rgb 'color-perceived-luminance color)
  (ghostty-color-perceived-luminance (rgb->c color)))

(define (color-contrast a b)
  (check-rgb 'color-contrast a)
  (check-rgb 'color-contrast b)
  (ghostty-color-contrast (rgb->c a) (rgb->c b)))

(define (c-string-at address)
  (let loop ([n 0])
    (if (fx= 0 (foreign-ref 'unsigned-8 address n))
        (utf8->string (address->bytevector address n))
        (loop (fx+ n 1)))))

(define (color-x11-colors)
  (let ([table (ghostty-color-x11-names)] [size (ftype-sizeof GhosttyColorX11Entry)])
    (let loop ([i (fx- (ghostty-color-x11-name-count) 1)] [acc '()])
      (if (fx< i 0)
          acc
          (let ([p (make-ftype-pointer GhosttyColorX11Entry (+ table (fx* i size)))])
            (loop (fx- i 1)
              (cons (cons (c-string-at (ftype-ref GhosttyColorX11Entry (name) p))
                          (make-rgb (ftype-ref GhosttyColorX11Entry (color r) p)
                                    (ftype-ref GhosttyColorX11Entry (color g) p)
                                    (ftype-ref GhosttyColorX11Entry (color b) p)))
                    acc)))))))

(define (color-scheme-report-encode scheme)
  (define who 'color-scheme-report-encode)
  (let ([value (symbol->c-enum GhosttyColorScheme scheme who)])
    (encode-with who (lambda (buf n len) (ghostty-color-scheme-report-encode value buf n len)))))

(define (focus-encode event)
  (define who 'focus-encode)
  (let ([value (symbol->c-enum GhosttyFocusEvent event who)])
    (encode-with who (lambda (buf n len) (ghostty-focus-encode value buf n len)))))

(define (mode-report-encode mode state)
  (define who 'mode-report-encode)
  (let ([mode (mode->c who mode)] [state (symbol->c-enum GhosttyModeReportState state who)])
    (encode-with who (lambda (buf n len) (ghostty-mode-report-encode mode state buf n len)))))

(define (size-report-encode style size)
  (define who 'size-report-encode)
  (let ([style (symbol->c-enum GhosttySizeReportStyle style who)] [bv (c-struct GhosttySizeReportSize)])
    (check-terminal-size who size)
    (c-set! GhosttySizeReportSize (rows) bv (terminal-size-rows size))
    (c-set! GhosttySizeReportSize (columns) bv (terminal-size-columns size))
    (c-set! GhosttySizeReportSize (cell_width) bv (terminal-size-cell-width size))
    (c-set! GhosttySizeReportSize (cell_height) bv (terminal-size-cell-height size))
    (encode-with who (lambda (buf n len) (ghostty-size-report-encode style (c-ptr bv) buf n len)))))

(define (paste-safe? text)
  (let ([bytes (text->bytevector 'paste-safe? text)])
    (ghostty-paste-is-safe bytes (bytevector-length bytes))))

;; The encoder rewrites its input in place.
(define (encode-paste who text bracketed?)
  (let ([bytes (text->bytevector who text)])
    (encode-with who
      (lambda (buf n len)
        (let ([copy (bytevector-copy bytes)])
          (ghostty-paste-encode copy (bytevector-length copy) bracketed? buf n len))))))

(define (paste-encode text) (encode-paste 'paste-encode text #f))

(define (paste-encode-bracketed text) (encode-paste 'paste-encode-bracketed text #t))

(define (unicode-codepoint-width codepoint)
  (check-u32 'unicode-codepoint-width codepoint)
  (ghostty-unicode-codepoint-width codepoint))

(define (unicode-grapheme-width codepoints)
  (define who 'unicode-grapheme-width)
  (let ([values-list (cond
                       [(string? codepoints) (map char->integer (string->list codepoints))]
                       [(and (list? codepoints) (for-all u32? codepoints)) codepoints]
                       [else (assertion-violationf who "~s is not a string or a list of codepoints"
                               codepoints)])])
    (let ([bv (make-bytevector (fx* 4 (length values-list)))] [width (make-bytevector 1 0)])
      (let loop ([l values-list] [i 0])
        (unless (null? l)
          (bytevector-u32-native-set! bv i (car l))
          (loop (cdr l) (fx+ i 4))))
      (let ([count (ghostty-unicode-grapheme-width bv (length values-list) width)])
        (values count (bytevector-u8-ref width 0))))))

(define-value-record osc-command (fields kind data))

(define osc-parse
  (case-lambda
    [(bytes) (osc-parse bytes 'bel)]
    [(bytes terminator)
     (define who 'osc-parse)
     (let ([bytes (text->bytevector who bytes)]
           [terminator (case terminator
                         [(bel) #x07]
                         [(st) #x5C]
                         [else (assertion-violationf who "~s is not bel or st" terminator)])]
           [out (pointer-out)])
       (check-result who (ghostty-osc-new 0 out))
       (let ([parser (pointer-in out)])
         (dynamic-wind
           void
           (lambda ()
             (let ([limit (make-bytevector 8)])
               (bytevector-u64-native-set! limit 0 (bytevector-length bytes))
               (ghostty-osc-set parser (GhosttyOscOption unknown-max-bytes) limit))
             (do ([i 0 (fx+ i 1)]) ((fx= i (bytevector-length bytes)))
               (ghostty-osc-next parser (bytevector-u8-ref bytes i)))
             (let* ([command (ghostty-osc-end parser terminator)]
                    [kind (or (and (not (eqv? command 0))
                                   (c-enum->symbol GhosttyOscCommandType
                                     (ghostty-osc-command-type command)))
                              'invalid)]
                    [data (lambda (key)
                            (let ([s (make-bytevector (ftype-sizeof GhosttyString) 0)])
                              (and (ghostty-osc-command-data command key s) (borrowed-bytes s 0))))])
               (make-osc-command kind
                 (case kind
                   [(change-window-title)
                    (let ([p (make-bytevector 8 0)])
                      (and (ghostty-osc-command-data command
                             (GhosttyOscCommandData change-window-title-str) p)
                           (c-string-at (bytevector-u64-native-ref p 0))))]
                   [(unknown) (data (GhosttyOscCommandData unknown-content))]
                   [else #f]))))
           (lambda () (ghostty-osc-free parser)))))]))

(define-value-record sgr-attribute (fields kind value))
(define-value-record sgr-unknown (fields full partial))

(define (u16-list-at address count)
  (let loop ([i (fx- count 1)] [acc '()])
    (if (fx< i 0) acc (loop (fx- i 1) (cons (foreign-ref 'unsigned-16 address (fx* 2 i)) acc)))))

(define (sgr-attribute-from-c bv)
  (let* ([tag (c-ref GhosttySgrAttribute (tag) bv)]
         [kind (c-enum->symbol GhosttySgrAttributeTag tag)]
         [rgb (lambda ()
                (make-rgb (c-ref GhosttySgrAttribute (value direct_color_fg r) bv)
                          (c-ref GhosttySgrAttribute (value direct_color_fg g) bv)
                          (c-ref GhosttySgrAttribute (value direct_color_fg b) bv)))]
         [index (lambda () (c-ref GhosttySgrAttribute (value fg_256) bv))])
    (make-sgr-attribute kind
      (case kind
        [(underline)
         (or (c-enum->symbol GhosttySgrUnderline (c-ref GhosttySgrAttribute (value underline) bv))
             'single)]
        [(underline-color direct-color-fg direct-color-bg) (rgb)]
        [(underline-color-256 bg-8 fg-8 bright-bg-8 bright-fg-8 bg-256 fg-256) (index)]
        [(unknown)
         (make-sgr-unknown
           (u16-list-at (c-ref GhosttySgrAttribute (value unknown full_ptr) bv)
             (c-ref GhosttySgrAttribute (value unknown full_len) bv))
           (u16-list-at (c-ref GhosttySgrAttribute (value unknown partial_ptr) bv)
             (c-ref GhosttySgrAttribute (value unknown partial_len) bv)))]
        [else #f]))))

(define sgr-parse
  (case-lambda
    [(params) (sgr-parse params #f)]
    [(params separators)
     (define who 'sgr-parse)
     (check who (and (list? params) (for-all u16? params)) params
       "a list of integers from 0 to 65535")
     (check who (or (not separators)
                    (and (string? separators) (fx= (string-length separators) (length params))))
       separators "#f or a string as long as the parameter list")
     (let ([numbers (make-bytevector (fx* 2 (length params)))]
           [seps (and separators (string->utf8 separators))]
           [out (pointer-out)])
       (let loop ([l params] [i 0])
         (unless (null? l)
           (bytevector-u16-native-set! numbers i (car l))
           (loop (cdr l) (fx+ i 2))))
       (check-result who (ghostty-sgr-new 0 out))
       (let ([parser (pointer-in out)])
         (dynamic-wind
           void
           (lambda ()
             (check-result who (ghostty-sgr-set-params parser numbers seps (length params)))
             (let ([attribute (c-struct GhosttySgrAttribute)])
               (let loop ([acc '()])
                 (if (ghostty-sgr-next parser attribute)
                     (loop (cons (sgr-attribute-from-c attribute) acc))
                     (reverse acc)))))
           (lambda () (ghostty-sgr-free parser)))))]))
