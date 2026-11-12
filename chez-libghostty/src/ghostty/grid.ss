(define-value-record grid-point
  (fields space x y)
  (protocol
    (lambda (new)
      (lambda (space x y)
        (define who 'make-grid-point)
        (symbol->c-enum GhosttyPointTag space who)
        (check-u16 who x)
        (check-u32 who y)
        (new space x y)))))

(define (check-grid-point who p) (check who (grid-point? p) p "a grid-point"))

(define (point->c who p)
  (let ([bv (c-struct GhosttyPoint)])
    (c-set! GhosttyPoint (tag) bv (symbol->c-enum GhosttyPointTag (grid-point-space p) who))
    (c-set! GhosttyPoint (value coordinate x) bv (grid-point-x p))
    (c-set! GhosttyPoint (value coordinate y) bv (grid-point-y p))
    bv))

;; Grid references point into the terminal's pages and are valid only until
;; the terminal next changes, so they never leave the terminal's lock.
(define (grid-ref who pointer p)
  (let ([ref (c-sized-struct GhosttyGridRef)])
    (check-result who (ghostty-terminal-grid-ref pointer (c-ptr (point->c who p)) ref) p)
    ref))

(define (grid-ref->point who pointer ref space)
  (let* ([out (c-struct GhosttyPointCoordinate)]
         [code (ghostty-terminal-point-from-grid-ref pointer ref
                 (symbol->c-enum GhosttyPointTag space who) out)])
    (cond
      [(fx= code (GhosttyResult success))
       (make-grid-point space (c-ref GhosttyPointCoordinate (x) out)
         (c-ref GhosttyPointCoordinate (y) out))]
      [(fx= code (GhosttyResult no-value)) #f]
      [else (raise-result who code)])))

(define (terminal-point-convert t p space)
  (define who 'terminal-point-convert)
  (check-grid-point who p)
  (symbol->c-enum GhosttyPointTag space who)
  (with-terminal who t (pointer)
    (grid-ref->point who pointer (grid-ref who pointer p) space)))

(define-value-record grid-cell
  (fields codepoint text wide content has-text? has-styling? style-id style hyperlink protected?
    semantic-content background))

(define-value-record grid-row
  (fields wrap? wrap-continuation? grapheme? styled? hyperlink? semantic-prompt
    kitty-virtual-placeholder? dirty?))

(define (cell-field cell key size ref)
  (let ([out (make-bytevector size 0)])
    (ghostty-cell-get cell key out)
    (ref out)))

(define (read-sized who read unit)
  (let* ([len (make-bytevector 8 0)] [code (read #f 0 len)])
    (cond
      [(fx= code (GhosttyResult success)) (make-bytevector 0)]
      [(fx= code (GhosttyResult out-of-space))
       (let* ([n (bytevector-u64-native-ref len 0)] [buf (make-bytevector (* n unit) 0)])
         (check-result who (read buf n len))
         buf)]
      [(fx= code (GhosttyResult no-value)) #f]
      [else (raise-result who code)])))

(define (codepoints->string bv)
  (let* ([n (fxquotient (bytevector-length bv) 4)] [s (make-string n)])
    (do ([i 0 (fx+ i 1)]) ((fx= i n) s)
      (string-set! s i (codepoint->char (bytevector-u32-native-ref bv (fx* 4 i)))))))

(define (codepoint->char cp)
  (if (and (fx< cp #x110000) (not (fx<= #xD800 cp #xDFFF))) (integer->char cp) #\xFFFD))

(define (grid-ref-cell who ref)
  (let ([out (make-bytevector 8 0)])
    (check-result who (ghostty-grid-ref-cell ref out))
    (let* ([cell (bytevector-u64-native-ref out 0)]
           [content (cell-field cell (GhosttyCellData content-tag) 4 int-ref)]
           [graphemes (read-sized who
                        (lambda (buf n len) (ghostty-grid-ref-graphemes ref buf n len)) 4)]
           [link (read-sized who
                   (lambda (buf n len) (ghostty-grid-ref-hyperlink-uri ref buf n len)) 1)]
           [style (let ([s (c-sized-struct GhosttyStyle)])
                    (check-result who (ghostty-grid-ref-style ref s))
                    (style-at s))])
      (make-grid-cell
        (cell-field cell (GhosttyCellData codepoint) 4 u32-ref)
        (if graphemes (codepoints->string graphemes) "")
        (c-enum->symbol GhosttyCellWide (cell-field cell (GhosttyCellData wide) 4 int-ref))
        (c-enum->symbol GhosttyCellContentTag content)
        (cell-field cell (GhosttyCellData has-text) 1 bool-ref)
        (cell-field cell (GhosttyCellData has-styling) 1 bool-ref)
        (cell-field cell (GhosttyCellData style-id) 2 u16-ref)
        style
        (and link (fx> (bytevector-length link) 0) (utf8->string link))
        (cell-field cell (GhosttyCellData protected) 1 bool-ref)
        (c-enum->symbol GhosttyCellSemanticContent
          (cell-field cell (GhosttyCellData semantic-content) 4 int-ref))
        (cond
          [(fx= content (GhosttyCellContentTag bg-color-palette))
           (cell-field cell (GhosttyCellData color-palette) 1 (lambda (bv) (bytevector-u8-ref bv 0)))]
          [(fx= content (GhosttyCellContentTag bg-color-rgb))
           (cell-field cell (GhosttyCellData color-rgb) 3 (lambda (bv) (rgb-at bv 0)))]
          [else #f])))))

(define (row-field row key size ref)
  (let ([out (make-bytevector size 0)])
    (ghostty-row-get row key out)
    (ref out)))

(define (grid-ref-row who ref)
  (let ([out (make-bytevector 8 0)])
    (check-result who (ghostty-grid-ref-row ref out))
    (let ([row (bytevector-u64-native-ref out 0)])
      (make-grid-row
        (row-field row (GhosttyRowData wrap) 1 bool-ref)
        (row-field row (GhosttyRowData wrap-continuation) 1 bool-ref)
        (row-field row (GhosttyRowData grapheme) 1 bool-ref)
        (row-field row (GhosttyRowData styled) 1 bool-ref)
        (row-field row (GhosttyRowData hyperlink) 1 bool-ref)
        (c-enum->symbol GhosttyRowSemanticPrompt
          (row-field row (GhosttyRowData semantic-prompt) 4 int-ref))
        (row-field row (GhosttyRowData kitty-virtual-placeholder) 1 bool-ref)
        (row-field row (GhosttyRowData dirty) 1 bool-ref)))))

(define (terminal-cell t p)
  (define who 'terminal-cell)
  (check-grid-point who p)
  (with-terminal who t (pointer) (grid-ref-cell who (grid-ref who pointer p))))

(define (terminal-row t p)
  (define who 'terminal-row)
  (check-grid-point who p)
  (with-terminal who t (pointer) (grid-ref-row who (grid-ref who pointer p))))

(define-record-type (tracked-point new-tracked-point tracked-point?)
  (nongenerative)
  (sealed #t)
  (opaque #t)
  (fields native terminal))

(define (check-tracked-point who tp) (check who (tracked-point? tp) tp "a tracked-point"))

(define-syntax with-tracked-point
  (syntax-rules ()
    [(_ who tp-expr (pointer terminal-pointer) body ...)
     (let ([tp tp-expr])
       (check-tracked-point who tp)
       (with-native who (tracked-point-native tp) "tracked point" (pointer)
         (let ([terminal-pointer (native-pointer (terminal-native (tracked-point-terminal tp)))])
           body ...)))]))

(define (check-not-in-callback who t)
  (when (lock-busy? (terminal-lock t))
    (assertion-violationf who "cannot change a terminal inside one of its callbacks")))

(define (make-tracked-point t p)
  (define who 'make-tracked-point)
  (check-grid-point who p)
  (release-dropped-natives!)
  (with-terminal-change who t (pointer)
    (let ([out (pointer-out)])
      (check-result who (ghostty-terminal-grid-ref-track pointer (c-ptr (point->c who p)) out) p)
      (let ([native (make-native (pointer-in out) ghostty-tracked-grid-ref-free (terminal-lock t))])
        (register-native! (new-tracked-point native t) native)))))

(define (tracked-point-close! tp)
  (check-tracked-point 'tracked-point-close! tp)
  (close-native! 'tracked-point-close! (tracked-point-native tp) "tracked point"))

(define (tracked-point-closed? tp)
  (check-tracked-point 'tracked-point-closed? tp)
  (native-closed? (tracked-point-native tp)))

(define (tracked-point-valid? tp)
  (with-tracked-point 'tracked-point-valid? tp (pointer terminal-pointer)
    (ghostty-tracked-grid-ref-has-value pointer)))

(define (tracked-point-ref tp space)
  (define who 'tracked-point-ref)
  (symbol->c-enum GhosttyPointTag space who)
  (with-tracked-point who tp (pointer terminal-pointer)
    (let* ([out (c-struct GhosttyPointCoordinate)]
           [code (ghostty-tracked-grid-ref-point pointer (symbol->c-enum GhosttyPointTag space who)
                   out)])
      (cond
        [(fx= code (GhosttyResult success))
         (make-grid-point space (c-ref GhosttyPointCoordinate (x) out)
           (c-ref GhosttyPointCoordinate (y) out))]
        [(fx= code (GhosttyResult no-value)) #f]
        [else (raise-result who code)]))))

(define (tracked-point-move! tp p)
  (define who 'tracked-point-move!)
  (check-grid-point who p)
  (with-tracked-point who tp (pointer terminal-pointer)
    (when (eqv? terminal-pointer 0) (assertion-violationf who "the terminal is closed"))
    (check-not-in-callback who (tracked-point-terminal tp))
    (check-result who (ghostty-tracked-grid-ref-set pointer terminal-pointer (c-ptr (point->c who p)))
      p)))

(define (tracked-point-snapshot who tp read)
  (with-tracked-point who tp (pointer terminal-pointer)
    (let* ([ref (c-sized-struct GhosttyGridRef)]
           [code (ghostty-tracked-grid-ref-snapshot pointer ref)])
      (cond
        [(fx= code (GhosttyResult success)) (read who ref)]
        [(fx= code (GhosttyResult no-value)) #f]
        [else (raise-result who code)]))))

(define (tracked-point-cell tp) (tracked-point-snapshot 'tracked-point-cell tp grid-ref-cell))
(define (tracked-point-row tp) (tracked-point-snapshot 'tracked-point-row tp grid-ref-row))

(define-value-record selection (fields start end rectangle?))

(define (selection-from-c who pointer bv)
  (let ([ref-size (ftype-sizeof GhosttyGridRef)])
    (let ([start (make-bytevector ref-size)] [end (make-bytevector ref-size)])
      (bytevector-copy! bv (ftype-sizeof size_t) start 0 ref-size)
      (bytevector-copy! bv (fx+ (ftype-sizeof size_t) ref-size) end 0 ref-size)
      (let ([start (grid-ref->point who pointer start 'screen)]
            [end (grid-ref->point who pointer end 'screen)])
        (and start end
             (make-selection start end (c-ref GhosttySelection (rectangle) bv)))))))

(define (selection->c who pointer start end rectangle?)
  (let ([bv (c-sized-struct GhosttySelection)] [ref-size (ftype-sizeof GhosttyGridRef)])
    (bytevector-copy! (grid-ref who pointer start) 0 bv (ftype-sizeof size_t) ref-size)
    (bytevector-copy! (grid-ref who pointer end) 0 bv (fx+ (ftype-sizeof size_t) ref-size) ref-size)
    (c-set! GhosttySelection (rectangle) bv rectangle?)
    bv))

(define (current-selection-c who pointer)
  (let* ([bv (c-sized-struct GhosttySelection)]
         [code (ghostty-terminal-get pointer (GhosttyTerminalData selection) bv)])
    (cond
      [(fx= code (GhosttyResult success)) bv]
      [(fx= code (GhosttyResult no-value)) #f]
      [else (raise-result who code)])))

(define (install-selection! who pointer bv)
  (check-result who (ghostty-terminal-set-value pointer (GhosttyTerminalOption selection) bv)))

(define (terminal-selection t)
  (define who 'terminal-selection)
  (with-terminal who t (pointer)
    (let ([bv (current-selection-c who pointer)])
      (and bv (selection-from-c who pointer bv)))))

(define (select! who t start end rectangle?)
  (check-grid-point who start)
  (check-grid-point who end)
  (with-terminal-change who t (pointer)
    (install-selection! who pointer (selection->c who pointer start end rectangle?))))

(define (terminal-select! t start end) (select! 'terminal-select! t start end #f))
(define (terminal-select-rectangle! t start end)
  (select! 'terminal-select-rectangle! t start end #t))

(define (terminal-clear-selection! t)
  (with-terminal-change 'terminal-clear-selection! t (pointer)
    (check-result 'terminal-clear-selection!
      (ghostty-terminal-set pointer (GhosttyTerminalOption selection) 0))))

(define (derive-selection! who t derive)
  (with-terminal-change who t (pointer)
    (let* ([out (c-sized-struct GhosttySelection)] [code (derive pointer out)])
      (cond
        [(fx= code (GhosttyResult success))
         (install-selection! who pointer out)
         (selection-from-c who pointer out)]
        [(fx= code (GhosttyResult no-value)) #f]
        [else (raise-result who code)]))))

(define (terminal-select-all! t)
  (derive-selection! 'terminal-select-all! t
    (lambda (pointer out) (ghostty-terminal-select-all pointer out))))

(define (boundary-codepoints who chars)
  (check who (or (not chars) (string? chars)) chars "a string or #f")
  (and chars (codepoints->immobile who chars)))

(define terminal-select-word!
  (case-lambda
    [(t p) (terminal-select-word! t p #f)]
    [(t p boundary)
     (define who 'terminal-select-word!)
     (check-grid-point who p)
     (let ([codepoints (boundary-codepoints who boundary)])
       (let ([result
              (derive-selection! who t
                (lambda (pointer out)
                  (let ([options (c-sized-struct GhosttyTerminalSelectWordOptions)])
                    (bytevector-copy! (grid-ref who pointer p) 0 options (ftype-sizeof size_t)
                      (ftype-sizeof GhosttyGridRef))
                    (when codepoints
                      (c-set! GhosttyTerminalSelectWordOptions (boundary_codepoints) options
                        (object->reference-address codepoints))
                      (c-set! GhosttyTerminalSelectWordOptions (boundary_codepoints_len) options
                        (string-length boundary)))
                    (ghostty-terminal-select-word pointer options out))))])
         (keep-live codepoints)
         result))]))

(define terminal-select-word-between!
  (case-lambda
    [(t start end) (terminal-select-word-between! t start end #f)]
    [(t start end boundary)
     (define who 'terminal-select-word-between!)
     (check-grid-point who start)
     (check-grid-point who end)
     (let ([codepoints (boundary-codepoints who boundary)])
       (let ([result
              (derive-selection! who t
                (lambda (pointer out)
                  (let ([options (c-sized-struct GhosttyTerminalSelectWordBetweenOptions)]
                        [ref-size (ftype-sizeof GhosttyGridRef)])
                    (bytevector-copy! (grid-ref who pointer start) 0 options (ftype-sizeof size_t)
                      ref-size)
                    (bytevector-copy! (grid-ref who pointer end) 0 options
                      (fx+ (ftype-sizeof size_t) ref-size) ref-size)
                    (when codepoints
                      (c-set! GhosttyTerminalSelectWordBetweenOptions (boundary_codepoints) options
                        (object->reference-address codepoints))
                      (c-set! GhosttyTerminalSelectWordBetweenOptions (boundary_codepoints_len)
                        options (string-length boundary)))
                    (ghostty-terminal-select-word-between pointer options out))))])
         (keep-live codepoints)
         result))]))

(define (select-line! who t p whitespace prompt-boundary?)
  (check-grid-point who p)
  (let ([codepoints (boundary-codepoints who whitespace)])
    (let ([result
           (derive-selection! who t
             (lambda (pointer out)
               (let ([options (c-sized-struct GhosttyTerminalSelectLineOptions)])
                 (bytevector-copy! (grid-ref who pointer p) 0 options (ftype-sizeof size_t)
                   (ftype-sizeof GhosttyGridRef))
                 (when codepoints
                   (c-set! GhosttyTerminalSelectLineOptions (whitespace) options
                     (object->reference-address codepoints))
                   (c-set! GhosttyTerminalSelectLineOptions (whitespace_len) options
                     (string-length whitespace)))
                 (c-set! GhosttyTerminalSelectLineOptions (semantic_prompt_boundary) options
                   prompt-boundary?)
                 (ghostty-terminal-select-line pointer options out))))])
      (keep-live codepoints)
      result)))

(define terminal-select-line!
  (case-lambda
    [(t p) (terminal-select-line! t p #f)]
    [(t p whitespace) (select-line! 'terminal-select-line! t p whitespace #f)]))

(define terminal-select-semantic-line!
  (case-lambda
    [(t p) (terminal-select-semantic-line! t p #f)]
    [(t p whitespace) (select-line! 'terminal-select-semantic-line! t p whitespace #t)]))

(define (terminal-select-output! t p)
  (define who 'terminal-select-output!)
  (check-grid-point who p)
  (derive-selection! who t
    (lambda (pointer out)
      (ghostty-terminal-select-output pointer (c-ptr (grid-ref who pointer p)) out))))

(define (terminal-selection-adjust! t adjustment)
  (define who 'terminal-selection-adjust!)
  (let ([adjust (symbol->c-enum GhosttySelectionAdjust adjustment who)])
    (with-terminal-change who t (pointer)
      (let ([bv (current-selection-c who pointer)])
        (and bv
             (let ([code (ghostty-terminal-selection-adjust pointer bv adjust)])
               (cond
                 [(fx= code (GhosttyResult success))
                  (install-selection! who pointer bv)
                  (selection-from-c who pointer bv)]
                 [(fx= code (GhosttyResult no-value)) #f]
                 [else (raise-result who code)])))))))

(define (terminal-selection-order t)
  (define who 'terminal-selection-order)
  (with-terminal who t (pointer)
    (let ([bv (current-selection-c who pointer)])
      (and bv
           (let ([out (make-bytevector 4 0)])
             (check-result who (ghostty-terminal-selection-order pointer bv out))
             (c-enum->symbol GhosttySelectionOrder (int-ref out)))))))

(define (terminal-selection-contains? t p)
  (define who 'terminal-selection-contains?)
  (check-grid-point who p)
  (with-terminal who t (pointer)
    (let ([bv (current-selection-c who pointer)])
      (and bv
           (let ([out (make-bytevector 1 0)])
             (check-result who
               (ghostty-terminal-selection-contains pointer bv (c-ptr (point->c who p)) out))
             (bool-ref out))))))

(define (terminal-format-selection t kind options)
  (define who 'terminal-format-selection)
  (check-format-options who options)
  (let ([opts (c-sized-struct GhosttyTerminalSelectionFormatOptions)])
    (c-set! GhosttyTerminalSelectionFormatOptions (emit) opts (format-kind who kind))
    (c-set! GhosttyTerminalSelectionFormatOptions (unwrap) opts (enum-set-member? 'unwrap options))
    (c-set! GhosttyTerminalSelectionFormatOptions (trim) opts (enum-set-member? 'trim options))
    (with-terminal who t (pointer)
      (let* ([out-ptr (pointer-out)] [out-len (pointer-out)]
             [code (ghostty-terminal-selection-format-alloc pointer 0 (c-ptr opts) out-ptr out-len)])
        (cond
          [(fx= code (GhosttyResult success)) (utf8->string (take-allocated-bytes out-ptr out-len))]
          [(fx= code (GhosttyResult no-value)) #f]
          [else (raise-result who code)])))))

(define (terminal-selection->string t)
  (terminal-format-selection t 'plain (format-options unwrap trim)))
