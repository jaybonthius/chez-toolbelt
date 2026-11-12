(define-enumeration key-modifier
  (shift ctrl alt super caps-lock num-lock right-shift right-ctrl right-alt right-super)
  key-modifiers)

(define (check-modifiers who mods)
  (check who (enum-set-of? mods (key-modifiers))
    mods "a set of key modifiers"))

;; libghostty-vt requires control characters and platform function-key
;; codes to be left to the key itself.
(define (key-text? text)
  (and (string? text)
       (let loop ([i 0])
         (or (fx= i (string-length text))
             (let ([n (char->integer (string-ref text i))])
               (and (not (or (fx< n #x20) (fx= n #x7f) (fx<= #xF700 n #xF8FF)))
                    (loop (fx+ i 1))))))))

(define-value-record key-event
  (fields action key modifiers text unshifted composing? consumed-modifiers)
  (protocol
    (lambda (new)
      (define (make action key modifiers text unshifted composing? consumed)
        (define who 'make-key-event)
        (symbol->c-enum GhosttyKeyAction action who)
        (symbol->c-enum GhosttyKey key who)
        (check-modifiers who modifiers)
        (check who (or (not text) (key-text? text)) text "#f or text without control characters")
        (check who (or (not unshifted) (char? unshifted)) unshifted "a character or #f")
        (check-boolean who composing?)
        (check-modifiers who consumed)
        (new action key modifiers text unshifted composing? consumed))
      (case-lambda
        [(action key) (make action key (key-modifiers) #f #f #f (key-modifiers))]
        [(action key modifiers) (make action key modifiers #f #f #f (key-modifiers))]
        [(action key modifiers text) (make action key modifiers text #f #f (key-modifiers))]
        [(action key modifiers text unshifted)
         (make action key modifiers text unshifted #f (key-modifiers))]
        [(action key modifiers text unshifted composing?)
         (make action key modifiers text unshifted composing? (key-modifiers))]
        [(action key modifiers text unshifted composing? consumed)
         (make action key modifiers text unshifted composing? consumed)]))))

(define-value-record mouse-event
  (fields action button x y modifiers)
  (protocol
    (lambda (new)
      (define (make action button x y modifiers)
        (define who 'make-mouse-event)
        (symbol->c-enum GhosttyMouseAction action who)
        (when button (symbol->c-enum GhosttyMouseButton button who))
        (check-position who x y)
        (check-modifiers who modifiers)
        (new action button (inexact x) (inexact y) modifiers))
      (case-lambda
        [(action button x y) (make action button x y (key-modifiers))]
        [(action button x y modifiers) (make action button x y modifiers)]))))

(define-value-record mouse-geometry
  (fields screen-width screen-height cell-width cell-height
    padding-top padding-bottom padding-right padding-left)
  (protocol
    (lambda (new)
      (lambda (screen-width screen-height cell-width cell-height
               padding-top padding-bottom padding-right padding-left)
        (define who 'make-mouse-geometry)
        (for-each (lambda (n) (check-u32 who n))
          (list screen-width screen-height padding-top padding-bottom padding-right padding-left))
        (check who (and (u32? cell-width) (> cell-width 0)) cell-width "a positive 32-bit integer")
        (check who (and (u32? cell-height) (> cell-height 0)) cell-height
          "a positive 32-bit integer")
        (check who (u32? (+ padding-left padding-right)) padding-right
          "a padding whose sum with the opposite padding fits 32 bits")
        (check who (u32? (+ padding-top padding-bottom)) padding-bottom
          "a padding whose sum with the opposite padding fits 32 bits")
        (new screen-width screen-height cell-width cell-height
          padding-top padding-bottom padding-right padding-left)))))

(define (make-native-pair who new-encoder free-encoder new-event free-event)
  (let ([encoder (pointer-out)] [event (pointer-out)])
    (check-result who (new-encoder 0 encoder))
    (let ([code (new-event 0 event)])
      (unless (fx= code 0)
        (free-encoder (pointer-in encoder))
        (raise-result who code)))
    (let ([event (pointer-in event)])
      (values
        (make-native (pointer-in encoder)
          (lambda (pointer) (free-event event) (free-encoder pointer))
          (make-lock #f))
        event))))

(define (encode-with who encode)
  (let* ([len (make-bytevector 8 0)] [buf (make-bytevector 128)] [code (encode buf 128 len)])
    (cond
      [(fx= code (GhosttyResult success))
       (let ([n (bytevector-u64-native-ref len 0)])
         (let ([out (make-bytevector n)]) (bytevector-copy! buf 0 out 0 n) out))]
      [(fx= code (GhosttyResult out-of-space))
       (let* ([n (bytevector-u64-native-ref len 0)] [buf (make-bytevector n)])
         (check-result who (encode buf n len))
         (let ([written (bytevector-u64-native-ref len 0)])
           (if (fx= written n)
               buf
               (let ([out (make-bytevector written)])
                 (bytevector-copy! buf 0 out 0 written)
                 out))))]
      [else (raise-result who code)])))

(define-record-type (key-encoder new-key-encoder key-encoder?)
  (nongenerative)
  (sealed #t)
  (opaque #t)
  (fields native event))

(define (check-key-encoder who e) (check who (key-encoder? e) e "a key-encoder"))

(define-syntax with-key-encoder
  (syntax-rules ()
    [(_ who e-expr (pointer) body ...)
     (let ([e e-expr])
       (check-key-encoder who e)
       (with-native who (key-encoder-native e) "key encoder" (pointer) body ...))]))

(define (make-key-encoder)
  (release-dropped-natives!)
  (let-values ([(native event)
                (make-native-pair 'make-key-encoder ghostty-key-encoder-new ghostty-key-encoder-free
                  ghostty-key-event-new ghostty-key-event-free)])
    (register-native! (new-key-encoder native event) native)))

(define (key-encoder-close! e)
  (check-key-encoder 'key-encoder-close! e)
  (close-native! 'key-encoder-close! (key-encoder-native e) "key encoder"))

(define (key-encoder-closed? e)
  (check-key-encoder 'key-encoder-closed? e)
  (native-closed? (key-encoder-native e)))

(define (key-encoder-sync! e t)
  (define who 'key-encoder-sync!)
  (check-key-encoder who e)
  (with-terminal who t (terminal-pointer)
    (with-native who (key-encoder-native e) "key encoder" (pointer)
      (ghostty-key-encoder-setopt-from-terminal pointer terminal-pointer))))

(define (set-key-option! who e option value)
  (with-key-encoder who e (pointer) (ghostty-key-encoder-setopt pointer option value)))

(define-syntax define-key-flag-setter
  (syntax-rules ()
    [(_ name option)
     (define (name e enabled?)
       (check-boolean 'name enabled?)
       (set-key-option! 'name e (GhosttyKeyEncoderOption option) (bytevector (if enabled? 1 0))))]))

(define-key-flag-setter key-encoder-set-cursor-key-application! cursor-key-application)
(define-key-flag-setter key-encoder-set-keypad-key-application! keypad-key-application)
(define-key-flag-setter key-encoder-set-ignore-keypad-with-numlock! ignore-keypad-with-numlock)
(define-key-flag-setter key-encoder-set-alt-esc-prefix! alt-esc-prefix)
(define-key-flag-setter key-encoder-set-modify-other-keys! modify-other-keys-state-2)
(define-key-flag-setter key-encoder-set-backarrow-key-mode! backarrow-key-mode)

(define (key-encoder-set-kitty-flags! e flags)
  (define who 'key-encoder-set-kitty-flags!)
  (check who (enum-set-of? flags (kitty-key-flags)) flags
    "a set of Kitty keyboard flags")
  (set-key-option! who e (GhosttyKeyEncoderOption kitty-flags) (bytevector (enum-set->bits flags))))

(define (key-encoder-set-option-as-alt! e which)
  (define who 'key-encoder-set-option-as-alt!)
  (let ([value (case which
                 [(#f) (GhosttyOptionAsAlt false)]
                 [(#t) (GhosttyOptionAsAlt true)]
                 [(left) (GhosttyOptionAsAlt left)]
                 [(right) (GhosttyOptionAsAlt right)]
                 [else (assertion-violationf who "~s is not #f, #t, left or right" which)])])
    (let ([bv (make-bytevector 4)])
      (bytevector-s32-native-set! bv 0 value)
      (set-key-option! who e (GhosttyKeyEncoderOption macos-option-as-alt) bv))))

;; The event keeps a pointer to the text, not a copy.
(define (key-encoder-encode e event)
  (define who 'key-encoder-encode)
  (check who (key-event? event) event "a key-event")
  (with-key-encoder who e (pointer)
    (let ([native-event (key-encoder-event e)]
          [text (and (key-event-text event) (immobile-copy (string->utf8 (key-event-text event))))])
      (ghostty-key-event-set-action native-event
        (symbol->c-enum GhosttyKeyAction (key-event-action event) who))
      (ghostty-key-event-set-key native-event (symbol->c-enum GhosttyKey (key-event-key event) who))
      (ghostty-key-event-set-mods native-event (enum-set->bits (key-event-modifiers event)))
      (ghostty-key-event-set-consumed-mods native-event
        (enum-set->bits (key-event-consumed-modifiers event)))
      (ghostty-key-event-set-composing native-event (key-event-composing? event))
      (ghostty-key-event-set-unshifted-codepoint native-event
        (let ([c (key-event-unshifted event)]) (if c (char->integer c) 0)))
      (if text
          (ghostty-key-event-set-utf8 native-event (immobile-address text) (bytevector-length text))
          (ghostty-key-event-set-utf8 native-event 0 0))
      (let ([bytes (encode-with who
                     (lambda (buf n len) (ghostty-key-encoder-encode pointer native-event buf n len)))])
        (ghostty-key-event-set-utf8 native-event 0 0)
        (keep-live text)
        bytes))))

(define-record-type (mouse-encoder new-mouse-encoder mouse-encoder?)
  (nongenerative)
  (sealed #t)
  (opaque #t)
  (fields native event (mutable geometry) (mutable utf8-possible?)))

(define (check-mouse-encoder who e) (check who (mouse-encoder? e) e "a mouse-encoder"))

(define-syntax with-mouse-encoder
  (syntax-rules ()
    [(_ who e-expr (pointer) body ...)
     (let ([e e-expr])
       (check-mouse-encoder who e)
       (with-native who (mouse-encoder-native e) "mouse encoder" (pointer) body ...))]))

(define (make-mouse-encoder)
  (release-dropped-natives!)
  (let-values ([(native event)
                (make-native-pair 'make-mouse-encoder ghostty-mouse-encoder-new
                  ghostty-mouse-encoder-free ghostty-mouse-event-new ghostty-mouse-event-free)])
    (register-native! (new-mouse-encoder native event #f #f) native)))

(define (mouse-encoder-close! e)
  (check-mouse-encoder 'mouse-encoder-close! e)
  (close-native! 'mouse-encoder-close! (mouse-encoder-native e) "mouse encoder"))

(define (mouse-encoder-closed? e)
  (check-mouse-encoder 'mouse-encoder-closed? e)
  (native-closed? (mouse-encoder-native e)))

;; UTF-8 counts as in use whenever its mode is set: guessing which format
;; mode takes precedence could miss it.
(define (mouse-encoder-sync! e t)
  (define who 'mouse-encoder-sync!)
  (check-mouse-encoder who e)
  (with-terminal who t (terminal-pointer)
    (with-native who (mouse-encoder-native e) "mouse encoder" (pointer)
      (ghostty-mouse-encoder-setopt-from-terminal pointer terminal-pointer)
      (let ([config (mode-config who 'utf8-mouse #f)])
        (check-result who
          (ghostty-terminal-get terminal-pointer (GhosttyTerminalData mode) config))
        (mouse-encoder-utf8-possible?-set! e (c-ref GhosttyTerminalModeConfig (value) config))))))

(define (set-mouse-option! who e option value)
  (with-mouse-encoder who e (pointer) (ghostty-mouse-encoder-setopt pointer option value)))

(define (enum-bytes value)
  (let ([bv (make-bytevector 4)]) (bytevector-s32-native-set! bv 0 value) bv))

(define (mouse-encoder-set-tracking! e mode)
  (define who 'mouse-encoder-set-tracking!)
  (set-mouse-option! who e (GhosttyMouseEncoderOption event)
    (enum-bytes (symbol->c-enum GhosttyMouseTrackingMode mode who))))

(define (mouse-encoder-set-format! e format)
  (define who 'mouse-encoder-set-format!)
  (let ([value (enum-bytes (symbol->c-enum GhosttyMouseFormat format who))])
    (with-mouse-encoder who e (pointer)
      (ghostty-mouse-encoder-setopt pointer (GhosttyMouseEncoderOption format) value)
      (mouse-encoder-utf8-possible?-set! e (eq? format 'utf8)))))

(define (mouse-encoder-set-geometry! e geometry)
  (define who 'mouse-encoder-set-geometry!)
  (check who (mouse-geometry? geometry) geometry "a mouse-geometry")
  (let ([bv (c-sized-struct GhosttyMouseEncoderSize)])
    (c-set! GhosttyMouseEncoderSize (screen_width) bv (mouse-geometry-screen-width geometry))
    (c-set! GhosttyMouseEncoderSize (screen_height) bv (mouse-geometry-screen-height geometry))
    (c-set! GhosttyMouseEncoderSize (cell_width) bv (mouse-geometry-cell-width geometry))
    (c-set! GhosttyMouseEncoderSize (cell_height) bv (mouse-geometry-cell-height geometry))
    (c-set! GhosttyMouseEncoderSize (padding_top) bv (mouse-geometry-padding-top geometry))
    (c-set! GhosttyMouseEncoderSize (padding_bottom) bv (mouse-geometry-padding-bottom geometry))
    (c-set! GhosttyMouseEncoderSize (padding_right) bv (mouse-geometry-padding-right geometry))
    (c-set! GhosttyMouseEncoderSize (padding_left) bv (mouse-geometry-padding-left geometry))
    (with-mouse-encoder who e (pointer)
      (ghostty-mouse-encoder-setopt pointer (GhosttyMouseEncoderOption size) bv)
      (mouse-encoder-geometry-set! e geometry))))

(define (mouse-encoder-set-any-button-pressed! e pressed?)
  (check-boolean 'mouse-encoder-set-any-button-pressed! pressed?)
  (set-mouse-option! 'mouse-encoder-set-any-button-pressed! e
    (GhosttyMouseEncoderOption any-button-pressed) (bytevector (if pressed? 1 0))))

(define (mouse-encoder-set-track-last-cell! e track?)
  (check-boolean 'mouse-encoder-set-track-last-cell! track?)
  (set-mouse-option! 'mouse-encoder-set-track-last-cell! e
    (GhosttyMouseEncoderOption track-last-cell) (bytevector (if track? 1 0))))

(define (mouse-encoder-reset! e)
  (with-mouse-encoder 'mouse-encoder-reset! e (pointer) (ghostty-mouse-encoder-reset pointer)))

(define (single-float x)
  (let ([bv (make-bytevector 4)])
    (bytevector-ieee-single-native-set! bv 0 x)
    (bytevector-ieee-single-native-ref bv 0)))

;; libghostty-vt converts positions to cells and pixels with unchecked
;; integer casts, adds the UTF-8 format's offset to a row in 16 bits and
;; encodes the result without checking for surrogates; out of range, each
;; is undefined behaviour in the release build. A position is encoded only
;; if its pixel coordinate fits 32 bits and its cell fits 16 bits on both
;; axes and, when UTF-8 may be in use, the cell plus 33 is a Unicode scalar
;; that also fits 16 bits for the row.
(define (axis-safe? value padding opposite screen cell utf8-possible? row?)
  (let* ([coordinate (- (single-float value) padding)]
         [quotient (/ (max coordinate 0.0) cell)]
         [extent (single-float (/ (single-float (inexact (max (- screen (+ padding opposite)) 0)))
                                  (single-float (inexact cell))))])
    (and (<= -2147483648.0 (round coordinate) 2147483647.0)
         (< quotient 65536.0)
         (< extent 65536.0)
         (or (not utf8-possible?)
             (let* ([count (max 1 (exact (floor extent)))]
                    [scalar (+ (min (exact (floor quotient)) (- count 1)) 33)])
               (and (not (<= #xD800 scalar #xDFFF)) (or (not row?) (<= scalar #xFFFF))))))))

(define (position-safe? geometry utf8-possible? x y)
  (and (axis-safe? x (mouse-geometry-padding-left geometry) (mouse-geometry-padding-right geometry)
         (mouse-geometry-screen-width geometry) (mouse-geometry-cell-width geometry)
         utf8-possible? #f)
       (axis-safe? y (mouse-geometry-padding-top geometry) (mouse-geometry-padding-bottom geometry)
         (mouse-geometry-screen-height geometry) (mouse-geometry-cell-height geometry)
         utf8-possible? #t)))

(define (mouse-encoder-encode e event)
  (define who 'mouse-encoder-encode)
  (check who (mouse-event? event) event "a mouse-event")
  (with-mouse-encoder who e (pointer)
    (let ([geometry (mouse-encoder-geometry e)] [native-event (mouse-encoder-event e)])
      (unless geometry (assertion-violationf who "the mouse encoder has no geometry yet"))
      (unless (position-safe? geometry (mouse-encoder-utf8-possible? e) (mouse-event-x event)
                (mouse-event-y event))
        (assertion-violationf who "position ~s, ~s is outside what the encoder can represent"
          (mouse-event-x event) (mouse-event-y event)))
      (ghostty-mouse-event-set-action native-event
        (symbol->c-enum GhosttyMouseAction (mouse-event-action event) who))
      (if (mouse-event-button event)
          (ghostty-mouse-event-set-button native-event
            (symbol->c-enum GhosttyMouseButton (mouse-event-button event) who))
          (ghostty-mouse-event-clear-button native-event))
      (ghostty-mouse-event-set-mods native-event (enum-set->bits (mouse-event-modifiers event)))
      (let ([position (c-struct GhosttyMousePosition)])
        (c-set! GhosttyMousePosition (x) position (mouse-event-x event))
        (c-set! GhosttyMousePosition (y) position (mouse-event-y event))
        (ghostty-mouse-event-set-position native-event (c-ptr position)))
      (encode-with who
        (lambda (buf n len) (ghostty-mouse-encoder-encode pointer native-event buf n len))))))
