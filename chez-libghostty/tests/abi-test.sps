(import (chezscheme) (ghostty ffi) (ghostty) (json) (check))

(define manifest (read-json (open-input-string (ghostty-type-json))))

(define (type name) (json-ref manifest "types" name))

(define (c-name symbol)
  (list->string
    (map (lambda (c) (if (char=? c #\-) #\_ (char-upcase c))) (string->list (symbol->string symbol)))))

(define-syntax layout
  (syntax-rules ()
    [(_ T field ...)
     (list (symbol->string 'T) (ftype-sizeof T)
       (list (cons (symbol->string 'field) (ftype-offset T field)) ...)
       (ftype-pointer-ftype (make-ftype-pointer T 0)))]))

(define layouts
  (list
    (layout GhosttyString ptr len)
    (layout GhosttyBuffer ptr cap len)
    (layout GhosttyCodepoints ptr len)
    (layout GhosttyColorRgb r g b)
    (layout GhosttyColorPaletteMask bits)
    (layout GhosttyColorX11Entry name color)
    (layout GhosttyStyleColorValue palette rgb _padding)
    (layout GhosttyStyleColor tag value)
    (layout GhosttyStyle size fg_color bg_color underline_color bold italic faint blink inverse
      invisible strikethrough overline underline)
    (layout GhosttyPointCoordinate x y)
    (layout GhosttyPointValue coordinate _padding)
    (layout GhosttyPoint tag value)
    (layout GhosttyGridRef size node x y)
    (layout GhosttySelection size start end rectangle)
    (layout GhosttySelectionBuffer ptr cap len)
    (layout GhosttyTerminalSelectWordOptions size ref boundary_codepoints boundary_codepoints_len)
    (layout GhosttyTerminalSelectWordBetweenOptions size start end boundary_codepoints
      boundary_codepoints_len)
    (layout GhosttyTerminalSelectLineOptions size ref whitespace whitespace_len
      semantic_prompt_boundary)
    (layout GhosttyTerminalSelectionFormatOptions size emit unwrap trim selection)
    (layout GhosttyFormatterScreenExtra size cursor style hyperlink protection kitty_keyboard charsets)
    (layout GhosttyFormatterTerminalExtra size palette modes scrolling_region tabstops pwd keyboard
      screen)
    (layout GhosttyFormatterTerminalOptions size emit unwrap trim extra selection)
    (layout GhosttyTerminalScrollbar total offset len)
    (layout GhosttyTerminalMemoryUsage size compression_supported primary_pages primary_virtual_bytes
      primary_resident_bytes primary_compressed_pages primary_compressed_bytes primary_image_bytes
      alternate_pages alternate_virtual_bytes alternate_resident_bytes alternate_compressed_pages
      alternate_compressed_bytes alternate_image_bytes)
    (layout GhosttyTerminalModeConfig mode value)
    (layout GhosttyTerminalScrollViewportValue delta row delta_prompt _padding)
    (layout GhosttyTerminalScrollViewport tag value)
    (layout GhosttyTerminalUnknownStringSequence truncated content)
    (layout GhosttyTerminalUnknownOscSequence truncated content terminator)
    (layout GhosttyTerminalUnknownSequenceValue apc osc _padding)
    (layout GhosttyTerminalUnknownSequence tag value)
    (layout GhosttyClipboardContent mime data)
    (layout GhosttyClipboardWriteReply size result remember)
    (layout GhosttyClipboardWrite size location contents contents_len name granted can_remember ctx
      reply)
    (layout GhosttyClipboardReadReply size result contents contents_len available available_len
      remember)
    (layout GhosttyClipboardRead size location mimes mimes_len list name granted can_remember ctx
      reply)
    (layout GhosttyTerminalDesktopNotification size title body)
    (layout GhosttyTerminalProgressReport size state progress)
    (layout GhosttyTerminalProgramStatus size state kind progress id app title message)
    (layout GhosttyTerminalSemanticPrompt size kind prompt_kind has_exit_code exit_code command error)
    (layout GhosttyTerminalResizeWindowGrid rows columns)
    (layout GhosttyTerminalResizeWindowValue grid _padding)
    (layout GhosttyTerminalResizeWindow tag value)
    (layout GhosttySizeReportSize rows columns cell_width cell_height)
    (layout GhosttyDeviceAttributesPrimary conformance_level features num_features)
    (layout GhosttyDeviceAttributesSecondary device_type firmware_version rom_cartridge)
    (layout GhosttyDeviceAttributesTertiary unit_id)
    (layout GhosttyDeviceAttributes primary secondary tertiary)
    (layout GhosttyRenderStateRowSelection size start_x end_x)
    (layout GhosttyRenderStateCursor size viewport_has_value viewport_x viewport_y wide_tail visible
      blinking password_input visual_style)
    (layout GhosttyRenderStateColors size background foreground cursor cursor_has_value palette)
    (layout GhosttyRenderStateOverscan above below)
    (layout GhosttyRenderStateRowId bits)
    (layout GhosttyCellsView ptr len)
    (layout GhosttyMouseEncoderSize size screen_width screen_height cell_width cell_height padding_top
      padding_bottom padding_right padding_left)
    (layout GhosttyMousePosition x y)
    (layout GhosttySurfacePosition x y)
    (layout GhosttySelectionGestureBehaviors single_click double_click triple_click)
    (layout GhosttySelectionGestureGeometry columns cell_width padding_left screen_height)
    (layout GhosttyWriter write userdata)
    (layout GhosttyMimeReader read userdata)
    (layout GhosttyPaste size location source mimes mimes_len reader allow_unsafe)
    (layout GhosttySgrUnknown full_ptr full_len partial_ptr partial_len)
    (layout GhosttySgrAttributeValue unknown underline underline_color underline_color_256
      direct_color_fg direct_color_bg bg_8 fg_8 bright_bg_8 bright_fg_8 bg_256 fg_256 _padding)
    (layout GhosttySgrAttribute tag value)
    (layout GhosttyKittyGraphicsPlacementRenderInfo size pixel_width pixel_height grid_cols grid_rows
      viewport_col viewport_row viewport_visible source_x source_y source_width source_height)))

(define (layout-mismatches layout)
  (let ([name (car layout)] [size (cadr layout)] [fields (caddr layout)])
    (let ([t (type name)])
      (if (not t)
          (list (list name 'not-in-manifest))
          (let ([manifest-fields (json-ref t "fields")])
            (append
              (if (= size (json-ref t "size")) '() (list (list name 'size size (json-ref t "size"))))
              (if (equal? (list-sort string<? (map car fields)) (list-sort string<? (map car manifest-fields)))
                  '()
                  (list (list name 'fields (map car fields) (map car manifest-fields))))
              (fold-right
                (lambda (field acc)
                  (let ([offset (json-ref manifest-fields (car field) "offset")])
                    (if (eqv? offset (cdr field))
                        acc
                        (cons (list name (car field) (cdr field) offset) acc))))
                '() fields)))))))

(define scalar-types
  '((integer-8 "i8") (unsigned-8 "u8") (integer-16 "i16") (unsigned-16 "u16") (integer-32 "i32")
    (unsigned-32 "u32") (int "i32") (unsigned "u32") (integer-64 "i64") (unsigned-64 "u64")
    (size_t "u64") (iptr "i64") (uptr "u64" "pointer") (void* "pointer") (stdbool "bool")
    (double "f64") (single-float "f32")))

(define (manifest-scalar name)
  (let ([t (type name)])
    (cond
      [(not t) name]
      [(equal? "alias" (json-ref t "kind")) (manifest-scalar (json-ref t "type"))]
      [(equal? "enum" (json-ref t "kind")) (json-ref t "underlying")]
      [else name])))

;; Offsets and sizes alone miss a last field one size too small, or one
;; unsigned where C is signed.
(define (field-type-matches? ours theirs)
  (let ([manifest-type (json-ref theirs "type")])
    (cond
      [(equal? manifest-type "array")
       (and (pair? ours) (eq? (car ours) 'array) (eqv? (cadr ours) (json-ref theirs "count"))
            (field-type-matches? (caddr ours) (list (cons "type" (json-ref theirs "elem")))))]
      [(assq ours scalar-types)
       => (lambda (entry) (and (member (manifest-scalar manifest-type) (cdr entry)) #t))]
      [(and (pair? ours) (eq? (car ours) '*)) (equal? manifest-type "pointer")]
      [else (equal? (symbol->string ours) manifest-type)])))

(define (field-type-mismatches name description)
  (let ([manifest-fields (json-ref (type name) "fields")])
    (fold-right
      (lambda (field acc)
        (let ([theirs (json-ref manifest-fields (symbol->string (car field)))])
          (if (and theirs (field-type-matches? (cadr field) theirs))
              acc
              (cons (list name (car field) (cadr field) (and theirs (json-ref theirs "type"))) acc))))
      '() (cdr description))))

(define (enum-mismatches e)
  (let* ([name (c-enum-c-type e)] [t (type name)])
    (if (not (and t (equal? "enum" (json-ref t "kind"))))
        (list (list name 'not-an-enum))
        (let ([values (filter (lambda (entry)
                                (let ([n (string-length (car entry))])
                                  (not (and (>= n 9) (string=? "MAX_VALUE" (substring (car entry) (- n 9) n))))))
                              (json-ref t "values"))])
          (append
            (if (equal? (c-enum-prefix e) (json-ref t "prefix")) '() (list (list name 'prefix)))
            (if (= (length values) (length (c-enum-entries e))) '() (list (list name 'count)))
            (fold-right
              (lambda (entry acc)
                (let ([value (json-ref values (c-name (car entry)))])
                  (if (eqv? value (cdr entry)) acc (cons (list name (car entry) (cdr entry) value) acc))))
              '() (c-enum-entries e)))))))

(define (manifest-cell-bits)
  (let* ([bits (json-ref (type "GhosttyCell") "bits")]
         [content (json-ref bits "content")]
         [arm (lambda (arm field)
                (let ([f (json-ref content "arms" arm "bits" field)])
                  (list (+ (json-ref content "lsb") (json-ref f "lsb")) (json-ref f "width"))))]
         [plain (lambda (field) (list (json-ref bits field "lsb") (json-ref bits field "width")))]
         [rgb (let ([r (arm "BG_COLOR_RGB" "r")] [g (arm "BG_COLOR_RGB" "g")] [b (arm "BG_COLOR_RGB" "b")])
                (and (= (car g) (+ (car r) 8)) (= (car b) (+ (car g) 8))
                     (equal? (map cadr (list r g b)) '(8 8 8))
                     (list (car r) 24)))])
    (list
      (cons 'content_tag (plain "content_tag"))
      (cons 'content.codepoint (arm "CODEPOINT" "codepoint"))
      (cons 'content.index (arm "BG_COLOR_PALETTE" "index"))
      (cons 'content.rgb rgb)
      (cons 'style_id (plain "style_id"))
      (cons 'wide (plain "wide"))
      (cons 'protected (plain "protected"))
      (cons 'hyperlink (plain "hyperlink"))
      (cons 'semantic_content (plain "semantic_content")))))

(define (packed-mismatches ours theirs)
  (filter (lambda (field) (not (equal? (cdr field) (cdr (assq (car field) theirs))))) ours))

(test "the manifest describes the platform the binding assumes"
  (check-equal? 1 (json-ref manifest "schema"))
  (check-equal? "x86_64" (json-ref manifest "abi" "target"))
  (check-equal? 8 (json-ref manifest "abi" "pointer_size"))
  (check-equal? "little" (json-ref manifest "abi" "endian")))

(test "every structure matches the library's layout"
  (for-each (lambda (l) (check-equal? '() (layout-mismatches l))) layouts)
  (for-each (lambda (l) (check-equal? '() (field-type-mismatches (car l) (cadddr l)))) layouts)
  (check-equal? 66 (length layouts)))

(test "every enumeration matches the library's values"
  (for-each (lambda (e) (check-equal? '() (enum-mismatches e))) (c-enums))
  (check-equal? 74 (length (c-enums))))

(test "the packed cell layout matches the library's"
  (check-equal? '() (packed-mismatches GhosttyCell (manifest-cell-bits))))

(test "the checks catch deliberate mismatches"
  (check (pair? (layout-mismatches (cons "GhosttyStyleColor" (cdr (layout GhosttyStyle size))))))
  (check (pair? (layout-mismatches (list "GhosttyStyle" (ftype-sizeof GhosttyStyle)
                                     (list (cons "size" 0) (cons "bold" 57))))))
  (check (pair? (layout-mismatches (list "GhosttyGridRef" 24
                                     (list (cons "size" 0) (cons "node" 8) (cons "x" 16)
                                           (cons "y" 20))))))
  (check (pair? (layout-mismatches (list "GhosttyNoSuchType" 8 '()))))
  (check (pair? (field-type-mismatches "GhosttyGridRef"
                  '(struct [size size_t] [node uptr] [x integer-16] [y unsigned-16]))))
  (check (pair? (field-type-mismatches "GhosttyGridRef"
                  '(struct [size size_t] [node uptr] [x unsigned-16] [y unsigned-8]))))
  (check (pair? (field-type-mismatches "GhosttyStyle"
                  '(struct [size size_t] [underline unsigned]))))
  (check (pair? (field-type-mismatches "GhosttyDeviceAttributesPrimary"
                  '(struct [features (array 64 unsigned-8)]))))
  (check (pair? (field-type-mismatches "GhosttyStyle" '(struct [fg_color GhosttyColorRgb]))))
  (let ([e (car (c-enums))])
    (check (pair? (enum-mismatches (make-c-enum (c-enum-c-type e) (c-enum-prefix e)
                                     (map (lambda (entry) (cons (car entry) (+ 1 (cdr entry))))
                                          (c-enum-entries e))
                                     #f #f))))
    (check (pair? (enum-mismatches (make-c-enum (c-enum-c-type e) "GHOSTTY_WRONG_"
                                     (c-enum-entries e) #f #f))))
    (check (pair? (enum-mismatches (make-c-enum (c-enum-c-type e) (c-enum-prefix e)
                                     (cdr (c-enum-entries e)) #f #f)))))
  (check (pair? (packed-mismatches '((style_id 27 16)) (manifest-cell-bits)))))

;; Modes are macros in the headers, outside the manifest; each table entry
;; is checked by setting the mode with its escape sequence. DECCOLM obeys
;; only after mode 40 allows it.
(test "every mode name answers to its escape sequence"
  (for-each
    (lambda (entry)
      (let ([t (make-terminal 10 3)])
        (terminal-write! t (string->utf8 "\x1b;[?40h"))
        (terminal-set-mode! t (car entry) #f)
        (terminal-write! t (string->utf8 (format "\x1b;[~a~ah" (if (cddr entry) "" "?") (cadr entry))))
        (check-equal? (cons (car entry) #t) (cons (car entry) (terminal-mode-enabled? t (car entry))))
        (terminal-write! t (string->utf8 (format "\x1b;[~a~al" (if (cddr entry) "" "?") (cadr entry))))
        (check (not (terminal-mode-enabled? t (car entry))))
        (terminal-close! t)))
    (let ([table '((kam 2 . #t) (insert 4 . #t) (srm 12 . #t) (linefeed 20 . #t) (decckm 1 . #f)
      (deccolm 3 . #f) (slow-scroll 4 . #f) (reverse-colors 5 . #f) (origin 6 . #f)
      (wraparound 7 . #f) (autorepeat 8 . #f) (x10-mouse 9 . #f) (cursor-blinking 12 . #f)
      (cursor-visible 25 . #f) (enable-mode-3 40 . #f) (reverse-wrap 45 . #f)
      (alt-screen-legacy 47 . #f) (keypad-keys 66 . #f) (backarrow-key-mode 67 . #f)
      (left-right-margin 69 . #f) (normal-mouse 1000 . #f) (button-mouse 1002 . #f)
      (any-mouse 1003 . #f) (focus-event 1004 . #f) (utf8-mouse 1005 . #f) (sgr-mouse 1006 . #f)
      (alt-scroll 1007 . #f) (urxvt-mouse 1015 . #f) (sgr-pixels-mouse 1016 . #f)
      (numlock-keypad 1035 . #f) (alt-esc-prefix 1036 . #f) (alt-sends-esc 1039 . #f)
      (reverse-wrap-ext 1045 . #f) (alt-screen 1047 . #f) (save-cursor 1048 . #f)
      (alt-screen-save 1049 . #f) (bracketed-paste 2004 . #f) (sync-output 2026 . #f)
      (grapheme-cluster 2027 . #f) (color-scheme-report 2031 . #f) (visibility-report 2033 . #f)
      (in-band-resize 2048 . #f) (paste-events 5522 . #f))])
      (check-equal? (terminal-modes) (map car table))
      table)))

(test-exit)
