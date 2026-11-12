(define-record-type (render-state new-render-state render-state?)
  (nongenerative)
  (sealed #t)
  (opaque #t)
  (fields native row-iterator row-cells scratch (mutable links)))

(define (check-render-state who rs) (check who (render-state? rs) rs "a render-state"))

(define-syntax with-render-state
  (syntax-rules ()
    [(_ who rs-expr (pointer) body ...)
     (let ([rs rs-expr])
       (check-render-state who rs)
       (with-native who (render-state-native rs) "render state" (pointer) body ...))]))

(define (make-render-state)
  (define who 'make-render-state)
  (release-dropped-natives!)
  (let ([state (pointer-out)] [rows (pointer-out)] [cells (pointer-out)])
    (check-result who (ghostty-render-state-new 0 state))
    (let ([code (ghostty-render-state-row-iterator-new 0 rows)])
      (unless (fx= code 0)
        (ghostty-render-state-free (pointer-in state))
        (raise-result who code)))
    (let ([code (ghostty-render-state-row-cells-new 0 cells)])
      (unless (fx= code 0)
        (ghostty-render-state-row-iterator-free (pointer-in rows))
        (ghostty-render-state-free (pointer-in state))
        (raise-result who code)))
    (let* ([rows (pointer-in rows)]
           [cells (pointer-in cells)]
           [native (make-native (pointer-in state)
                     (lambda (pointer)
                       (ghostty-render-state-row-cells-free cells)
                       (ghostty-render-state-row-iterator-free rows)
                       (ghostty-render-state-free pointer))
                     (make-lock #f))])
      (register-native! (new-render-state native rows cells (make-scratch) #f) native))))

(define (render-state-close! rs)
  (check-render-state 'render-state-close! rs)
  (close-native! 'render-state-close! (render-state-native rs) "render state"))

(define (render-state-closed? rs)
  (check-render-state 'render-state-closed? rs)
  (native-closed? (render-state-native rs)))

;; Allowed inside the terminal's callbacks: a render-hold handler updates
;; here to capture the frame a program wants left on screen.
(define (render-state-update! rs t)
  (define who 'render-state-update!)
  (check-render-state who rs)
  (with-terminal who t (terminal-pointer)
    (with-native who (render-state-native rs) "render state" (pointer)
      (check-result who (ghostty-render-state-update pointer terminal-pointer))
      (render-state-links-set! rs (read-links who rs pointer terminal-pointer))
      (render-dirty pointer))))

;; The render state holds no link targets, and the terminal can only be
;; read during the update. Overscan rows lie outside the viewport, so cells
;; are found by screen row.
(define (read-links who rs pointer terminal-pointer)
  (populate-rows! who rs pointer)
  (let ([rows (render-state-row-iterator rs)] [scratch (render-state-scratch rs)] [links #f]
        [viewport-top #f])
    (let loop ()
      (when (ghostty-render-state-row-iterator-next rows)
        (let ([y (bytevector-s32-native-ref
                   (row-get who rows (GhosttyRenderStateRowData viewport-y) (scratch-i32 scratch)) 0)]
              [row (u64-ref (row-get who rows (GhosttyRenderStateRowData raw) (scratch-raw scratch)))])
          (when (row-field row (GhosttyRowData hyperlink) 1 bool-ref)
            (unless viewport-top (set! viewport-top (terminal-viewport-top who terminal-pointer)))
            (let* ([view (row-get who rows (GhosttyRenderStateRowData cells-raw) (scratch-view scratch))]
                   [cells (make-ftype-pointer cell-array (c-ref GhosttyCellsView (ptr) view))])
              (do ([x 0 (fx+ x 1)]) ((fx= x (c-ref GhosttyCellsView (len) view)))
                (when (cell-hyperlink? (ftype-ref cell-array (x) cells))
                  (let* ([ref (grid-ref who terminal-pointer
                                (make-grid-point 'screen x (+ viewport-top y)))]
                         [uri (read-sized who
                                (lambda (buf n len) (ghostty-grid-ref-hyperlink-uri ref buf n len)) 1)])
                    (when (and uri (fx> (bytevector-length uri) 0))
                      (unless links (set! links (make-hashtable equal-hash equal?)))
                      (hashtable-set! links (cons y x) (utf8->string uri)))))))))
        (loop)))
    links))

(define (terminal-viewport-top who terminal-pointer)
  (let ([bv (c-struct GhosttyTerminalScrollbar)])
    (check-result who
      (ghostty-terminal-get terminal-pointer (GhosttyTerminalData scrollbar) bv))
    (c-ref GhosttyTerminalScrollbar (offset) bv)))

(define (render-get who pointer key out)
  (check-result who (ghostty-render-state-get pointer key out))
  out)

(define (render-dirty pointer)
  (let ([out (make-bytevector 4 0)])
    (render-get 'render-state-dirty pointer (GhosttyRenderStateData dirty) out)
    (let ([dirty (int-ref out)])
      (cond
        [(fx= dirty (GhosttyRenderStateDirty false)) 'clean]
        [(fx= dirty (GhosttyRenderStateDirty partial)) 'partial]
        [else 'full]))))

(define (render-state-dirty rs)
  (with-render-state 'render-state-dirty rs (pointer) (render-dirty pointer)))

(define (render-state-columns rs)
  (with-render-state 'render-state-columns rs (pointer)
    (u16-ref (render-get 'render-state-columns pointer (GhosttyRenderStateData cols)
               (make-bytevector 2 0)))))

(define (render-state-row-count rs)
  (with-render-state 'render-state-row-count rs (pointer)
    (u16-ref (render-get 'render-state-row-count pointer (GhosttyRenderStateData rows)
               (make-bytevector 2 0)))))

(define (render-state-clean! rs)
  (with-render-state 'render-state-clean! rs (pointer)
    (check-result 'render-state-clean! (ghostty-render-state-clean pointer))))

(define (render-state-set-overscan! rs above below)
  (define who 'render-state-set-overscan!)
  (check-u16 who above)
  (check-u16 who below)
  (let ([bv (c-struct GhosttyRenderStateOverscan)])
    (c-set! GhosttyRenderStateOverscan (above) bv above)
    (c-set! GhosttyRenderStateOverscan (below) bv below)
    (with-render-state who rs (pointer)
      (check-result who (ghostty-render-state-set pointer (GhosttyRenderStateOption overscan) bv)))))

(define (render-state-overscan rs)
  (define who 'render-state-overscan)
  (with-render-state who rs (pointer)
    (let ([bv (render-get who pointer (GhosttyRenderStateData overscan)
                (c-struct GhosttyRenderStateOverscan))])
      (values (c-ref GhosttyRenderStateOverscan (above) bv)
              (c-ref GhosttyRenderStateOverscan (below) bv)))))

(define-value-record render-colors (fields background foreground cursor palette))

(define (render-state-colors rs)
  (define who 'render-state-colors)
  (with-render-state who rs (pointer)
    (let ([bv (render-get who pointer (GhosttyRenderStateData colors)
                (c-sized-struct GhosttyRenderStateColors))])
      (make-render-colors
        (rgb-at bv (ftype-offset GhosttyRenderStateColors background))
        (rgb-at bv (ftype-offset GhosttyRenderStateColors foreground))
        (and (c-ref GhosttyRenderStateColors (cursor_has_value) bv)
             (rgb-at bv (ftype-offset GhosttyRenderStateColors cursor)))
        (palette-at bv (ftype-offset GhosttyRenderStateColors palette))))))

(define-value-record render-cursor
  (fields style visible? blinking? password-input? column row wide-tail?))

(define (render-state-cursor rs)
  (define who 'render-state-cursor)
  (with-render-state who rs (pointer)
    (let* ([bv (render-get who pointer (GhosttyRenderStateData cursor)
                 (c-sized-struct GhosttyRenderStateCursor))]
           [in-view? (c-ref GhosttyRenderStateCursor (viewport_has_value) bv)])
      (make-render-cursor
        (c-enum->symbol GhosttyRenderStateCursorVisualStyle
          (c-ref GhosttyRenderStateCursor (visual_style) bv))
        (c-ref GhosttyRenderStateCursor (visible) bv)
        (c-ref GhosttyRenderStateCursor (blinking) bv)
        (c-ref GhosttyRenderStateCursor (password_input) bv)
        (and in-view? (c-ref GhosttyRenderStateCursor (viewport_x) bv))
        (and in-view? (c-ref GhosttyRenderStateCursor (viewport_y) bv))
        (and in-view? (c-ref GhosttyRenderStateCursor (wide_tail) bv))))))

(define-value-record render-row
  (fields y id dirty? wrap? wrap-continuation? semantic-prompt selection-start selection-end runs))

(define-value-record render-run
  (fields column width text style selected? hyperlink semantic-content))

;; Reused across reads, which the render state's lock keeps apart.
(define-record-type scratch
  (nongenerative)
  (sealed #t)
  (fields handle view row-selection style u8 i32 id raw (mutable chars) (mutable codepoints))
  (protocol
    (lambda (new)
      (lambda ()
        (new (make-bytevector 8 0) (c-struct GhosttyCellsView)
          (c-sized-struct GhosttyRenderStateRowSelection) (c-sized-struct GhosttyStyle)
          (make-bytevector 1 0) (make-bytevector 4 0) (c-struct GhosttyRenderStateRowId)
          (make-bytevector 8 0) (make-string 256) (make-bytevector 64 0))))))

;; foreign-ref would dispatch on its type for every cell.
(define-ftype cell-array (array 65536 unsigned-64))

(define-syntax cell-content-tag (syntax-rules () [(_ c) (GhosttyCell content_tag c)]))
(define-syntax cell-codepoint (syntax-rules () [(_ c) (GhosttyCell content.codepoint c)]))
(define-syntax cell-palette-index (syntax-rules () [(_ c) (GhosttyCell content.index c)]))
(define-syntax cell-rgb (syntax-rules () [(_ c) (GhosttyCell content.rgb c)]))
(define-syntax cell-style-id (syntax-rules () [(_ c) (GhosttyCell style_id c)]))
(define-syntax cell-wide (syntax-rules () [(_ c) (GhosttyCell wide c)]))
(define-syntax cell-hyperlink? (syntax-rules () [(_ c) (fx= 1 (GhosttyCell hyperlink c))]))
(define-syntax cell-semantic (syntax-rules () [(_ c) (GhosttyCell semantic_content c)]))

(define default-style (make-style #f #f #f #f #f #f #f #f #f #f #f 'none))

(define (style-with-background style color)
  (make-style (style-foreground style) color (style-underline-color style) (style-bold? style)
    (style-italic? style) (style-faint? style) (style-blink? style) (style-inverse? style)
    (style-invisible? style) (style-strikethrough? style) (style-overline? style)
    (style-underline style)))

(define semantic-names (vector 'output 'input 'prompt 'output))

(define (row-get who rows key out)
  (check-result who (ghostty-render-state-row-get rows key out))
  out)

(define (read-row who rs)
  (let* ([rows (render-state-row-iterator rs)]
         [cells (render-state-row-cells rs)]
         [scratch (render-state-scratch rs)]
         [i32 (scratch-i32 scratch)]
         [u8 (scratch-u8 scratch)]
         [raw (scratch-raw scratch)])
    (let* ([y (bytevector-s32-native-ref
                (row-get who rows (GhosttyRenderStateRowData viewport-y) i32) 0)]
           [id (let ([bv (row-get who rows (GhosttyRenderStateRowData id) (scratch-id scratch))])
                 (+ (bytevector-u64-native-ref bv 0) (ash (bytevector-u64-native-ref bv 8) 64)))]
           [dirty? (bool-ref (row-get who rows (GhosttyRenderStateRowData dirty) u8))]
           [row (u64-ref (row-get who rows (GhosttyRenderStateRowData raw) raw))]
           [selection (scratch-row-selection scratch)]
           [selected? (fx= (ghostty-render-state-row-get rows (GhosttyRenderStateRowData selection)
                             selection)
                           (GhosttyResult success))]
           [selection-start (and selected? (c-ref GhosttyRenderStateRowSelection (start_x) selection))]
           [selection-end (and selected? (c-ref GhosttyRenderStateRowSelection (end_x) selection))])
      (make-render-row y id dirty?
        (row-field row (GhosttyRowData wrap) 1 bool-ref)
        (row-field row (GhosttyRowData wrap-continuation) 1 bool-ref)
        (c-enum->symbol GhosttyRowSemanticPrompt
          (row-field row (GhosttyRowData semantic-prompt) 4 int-ref))
        selection-start selection-end
        (read-runs who rows cells scratch selection-start selection-end
          (let ([links (render-state-links rs)])
            (and links (lambda (x) (hashtable-ref links (cons y x) #f)))))))))

(define (append-char! scratch n codepoint)
  (when (fx= n (string-length (scratch-chars scratch)))
    (let ([bigger (make-string (fx* 2 n))])
      (string-copy! (scratch-chars scratch) 0 bigger 0 n)
      (scratch-chars-set! scratch bigger)))
  (string-set! (scratch-chars scratch) n (codepoint->char codepoint))
  (fx+ n 1))

(define (cell-run-key cell color-only? selected?)
  (fxior (if color-only?
             (fxior (fxsll 1 31) (fxsll (cell-content-tag cell) 24) (cell-rgb cell))
             (cell-style-id cell))
         (if selected? (fxsll 1 32) 0)
         (fxsll (cell-semantic cell) 34)))

(define (run-key-selected? key) (fxbit-set? key 32))

(define (run-key-semantic-content key) (vector-ref semantic-names (fxand (fxsrl key 34) 3)))

(define (add-run runs scratch start end n style key link)
  (if key
      (cons (make-render-run start (fx- end start) (substring (scratch-chars scratch) 0 n) style
              (run-key-selected? key) link (run-key-semantic-content key))
            runs)
      runs))

(define (cell-background cell)
  (style-with-background default-style
    (if (fx= (cell-content-tag cell) (GhosttyCellContentTag bg-color-palette))
        (cell-palette-index cell)
        (let ([c (cell-rgb cell)])
          (make-rgb (fxand c 255) (fxand (fxsrl c 8) 255) (fxand (fxsrl c 16) 255))))))

(define (read-runs who rows cells scratch selection-start selection-end link-at)
  (let* ([view (row-get who rows (GhosttyRenderStateRowData cells-raw) (scratch-view scratch))]
         [raw (make-ftype-pointer cell-array (c-ref GhosttyCellsView (ptr) view))]
         [width (c-ref GhosttyCellsView (len) view)]
         [cells-ready? #f]
         [styles '()])
    (define (ensure-cells!)
      (unless cells-ready?
        (bytevector-u64-native-set! (scratch-handle scratch) 0 cells)
        (row-get who rows (GhosttyRenderStateRowData cells) (scratch-handle scratch))
        (set! cells-ready? #t)))
    (define (select-cell! x)
      (ensure-cells!)
      (check-result who (ghostty-render-state-row-cells-select cells x)))
    ;; The style of an id is read once per row; ids belong to a page, and a
    ;; row lies within one page.
    (define (style-of id x)
      (cond
        [(fx= id 0) default-style]
        [(assv id styles) => cdr]
        [else
         (select-cell! x)
         (let ([bv (scratch-style scratch)])
           (check-result who
             (ghostty-render-state-row-cells-get cells (GhosttyRenderStateRowCellsData style) bv))
           (let ([style (style-at bv)])
             (set! styles (cons (cons id style) styles))
             style))]))
    (define (append-grapheme! x n)
      (select-cell! x)
      (let ([len (scratch-i32 scratch)])
        (check-result who
          (ghostty-render-state-row-cells-get cells (GhosttyRenderStateRowCellsData graphemes-len) len))
        (let ([count (bytevector-u32-native-ref len 0)])
          (when (fx> (fx* 4 count) (bytevector-length (scratch-codepoints scratch)))
            (scratch-codepoints-set! scratch (make-bytevector (fx* 8 count) 0)))
          (let ([buf (scratch-codepoints scratch)])
            (check-result who
              (ghostty-render-state-row-cells-get cells (GhosttyRenderStateRowCellsData graphemes-buf)
                buf))
            (do ([i 0 (fx+ i 1)] [n n (append-char! scratch n (bytevector-u32-native-ref buf (fx* 4 i)))])
                ((fx= i count) n))))))
    (define (cell-at x) (ftype-ref cell-array (x) raw))
    (define (in-selection? x) (and selection-start (fx<= selection-start x selection-end)))
    (define (blank? x)
      (let ([cell (cell-at x)])
        (and (fx= (cell-codepoint cell) 0)
             (fx<= (cell-content-tag cell) (GhosttyCellContentTag codepoint-grapheme))
             (fx= (cell-style-id cell) 0)
             (not (cell-hyperlink? cell))
             (not (in-selection? x)))))
    (define end
      (let loop ([x (fx- width 1)])
        (cond
          [(fx< x 0) 0]
          [(blank? x) (loop (fx- x 1))]
          [else (fx+ x 1)])))
    (let loop ([x 0] [runs '()] [run-start 0] [run-key #f] [run-style #f] [run-link #f] [n 0])
      (if (fx>= x end)
          (list->vector (reverse (add-run runs scratch run-start x n run-style run-key run-link)))
          (let* ([cell (cell-at x)] [tag (cell-content-tag cell)] [wide (cell-wide cell)])
            (if (fx= wide (GhosttyCellWide spacer-tail))
                (loop (fx+ x 1) runs run-start run-key run-style run-link n)
                (let* ([color-only? (fx>= tag (GhosttyCellContentTag bg-color-palette))]
                       [key (cell-run-key cell color-only? (in-selection? x))]
                       [link (and link-at (cell-hyperlink? cell) (link-at x))]
                       [next (fx+ x (if (fx= wide (GhosttyCellWide wide)) 2 1))])
                  (if (and (eqv? key run-key) (equal? link run-link))
                      (loop next runs run-start run-key run-style run-link
                        (cond
                          [(fx= tag (GhosttyCellContentTag codepoint-grapheme)) (append-grapheme! x n)]
                          [(or color-only? (fx= (cell-codepoint cell) 0)) (append-char! scratch n 32)]
                          [else (append-char! scratch n (cell-codepoint cell))]))
                      (let ([runs (add-run runs scratch run-start x n run-style run-key run-link)])
                        (loop next runs x key
                          (if color-only? (cell-background cell) (style-of (cell-style-id cell) x))
                          link
                          (cond
                            [(fx= tag (GhosttyCellContentTag codepoint-grapheme)) (append-grapheme! x 0)]
                            [(or color-only? (fx= (cell-codepoint cell) 0)) (append-char! scratch 0 32)]
                            [else (append-char! scratch 0 (cell-codepoint cell))])))))))))))

(define (populate-rows! who rs pointer)
  (bytevector-u64-native-set! (scratch-handle (render-state-scratch rs)) 0
    (render-state-row-iterator rs))
  (render-get who pointer (GhosttyRenderStateData row-iterator)
    (scratch-handle (render-state-scratch rs))))

(define (render-state-rows rs)
  (define who 'render-state-rows)
  (with-render-state who rs (pointer)
    (populate-rows! who rs pointer)
    (let loop ([acc '()])
      (if (ghostty-render-state-row-iterator-next (render-state-row-iterator rs))
          (loop (cons (read-row who rs) acc))
          (reverse acc)))))

(define (render-state-dirty-rows rs)
  (define who 'render-state-dirty-rows)
  (with-render-state who rs (pointer)
    (populate-rows! who rs pointer)
    (let ([y (make-bytevector 2 0)])
      (let loop ([acc '()])
        (if (ghostty-render-state-row-iterator-next-dirty (render-state-row-iterator rs) y)
            (loop (cons (read-row who rs) acc))
            (reverse acc))))))
