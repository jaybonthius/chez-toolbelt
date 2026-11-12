(define-record-type (search new-search search?)
  (nongenerative)
  (sealed #t)
  (opaque #t)
  (fields native terminal))

(define (check-search who s) (check who (search? s) s "a search"))

(define-syntax with-search
  (syntax-rules ()
    [(_ who s-expr (pointer) body ...)
     (let ([s s-expr])
       (check-search who s)
       (with-native who (search-native s) "search" (pointer) body ...))]))

;; Operations that read the terminal also change the tracked state the
;; search keeps inside it.
(define-syntax with-search-change
  (syntax-rules ()
    [(_ who s-expr (pointer) body ...)
     (let ([s s-expr])
       (with-search who s (pointer)
         (search-terminal-pointer who s)
         (check-not-in-callback who (search-terminal s))
         body ...))]))

(define (make-search t)
  (define who 'make-search)
  (release-dropped-natives!)
  (with-terminal-change who t (pointer)
    (let ([out (pointer-out)])
      (check-result who (ghostty-search-new 0 out pointer))
      (let ([native (make-native (pointer-in out) ghostty-search-free (terminal-lock t))])
        (register-native! (new-search native t) native)))))

(define (search-close! s)
  (check-search 'search-close! s)
  (close-native! 'search-close! (search-native s) "search"))

(define (search-closed? s)
  (check-search 'search-closed? s)
  (native-closed? (search-native s)))

(define (search-set-needle! s needle)
  (define who 'search-set-needle!)
  (check who (or (not needle) (string? needle)) needle "a string or #f")
  (with-search-change who s (pointer)
    (if (and needle (not (string=? needle "")))
        (let ([bytes (immobile-copy (string->utf8 needle))] [str (c-struct GhosttyString)])
          (c-set! GhosttyString (ptr) str (immobile-address bytes))
          (c-set! GhosttyString (len) str (bytevector-length bytes))
          (check-result who (ghostty-search-set pointer (GhosttySearchOption needle) str))
          (keep-live bytes))
        (check-result who (ghostty-search-set pointer (GhosttySearchOption needle) #f)))))

(define (search-get who pointer key size ref)
  (let* ([out (make-bytevector size 0)] [code (ghostty-search-get pointer key out)])
    (cond
      [(fx= code (GhosttyResult success)) (ref out)]
      [(fx= code (GhosttyResult no-value)) #f]
      [else (raise-result who code)])))

(define (status-ref bv) (c-enum->symbol GhosttySearchStatus (int-ref bv)))

(define (search-status s)
  (with-search 'search-status s (pointer)
    (search-get 'search-status pointer (GhosttySearchData status) 4 status-ref)))

(define (search-feed! s)
  (with-search-change 'search-feed! s (pointer)
    (check-result 'search-feed! (ghostty-search-feed pointer))
    (search-get 'search-feed! pointer (GhosttySearchData status) 4 status-ref)))

(define (search-tick! s)
  (with-search 'search-tick! s (pointer)
    (let ([out (make-bytevector 4 0)])
      (check-result 'search-tick! (ghostty-search-tick pointer out))
      (status-ref out))))

(define (search-run! s)
  (with-search-change 'search-run! s (pointer)
    (check-result 'search-run! (ghostty-search-run pointer))))

(define (search-total s)
  (with-search 'search-total s (pointer)
    (search-get 'search-total pointer (GhosttySearchData total-matches) 8 u64-ref)))

(define (search-selected-index s)
  (with-search 'search-selected-index s (pointer)
    (search-get 'search-selected-index pointer (GhosttySearchData selected-index) 8 u64-ref)))

(define (search-terminal-pointer who s)
  (let ([pointer (native-pointer (terminal-native (search-terminal s)))])
    (when (eqv? pointer 0) (assertion-violationf who "the terminal is closed"))
    pointer))

(define (selected-match who s pointer)
  (let* ([bv (c-sized-struct GhosttySelection)]
         [code (ghostty-search-get pointer (GhosttySearchData selected-match) bv)])
    (cond
      [(fx= code (GhosttyResult success)) (selection-from-c who (search-terminal-pointer who s) bv)]
      [(fx= code (GhosttyResult no-value)) #f]
      [else (raise-result who code)])))

(define (search-selected-match s)
  (with-search 'search-selected-match s (pointer)
    (selected-match 'search-selected-match s pointer)))

(define (read-matches who s key)
  (with-search who s (pointer)
    (let ([buffer (c-struct GhosttySelectionBuffer #t)] [size (ftype-sizeof GhosttySelection)])
      (let ([code (ghostty-search-get pointer key buffer)])
        (unless (or (fx= code (GhosttyResult success)) (fx= code (GhosttyResult out-of-space)))
          (raise-result who code)))
      (let* ([n (c-ref GhosttySelectionBuffer (len) buffer)]
             [array (make-immobile-bytevector (fx* n size) 0)])
        (do ([i 0 (fx+ i 1)]) ((fx= i n))
          (bytevector-u64-native-set! array (fx* i size) size))
        (c-set! GhosttySelectionBuffer (ptr) buffer (immobile-address array))
        (c-set! GhosttySelectionBuffer (cap) buffer n)
        (check-result who (ghostty-search-get pointer key buffer))
        (let ([terminal-pointer (search-terminal-pointer who s)] [n (c-ref GhosttySelectionBuffer (len) buffer)])
          (let loop ([i (fx- n 1)] [acc '()])
            (if (fx< i 0)
                acc
                (let ([one (make-bytevector size)])
                  (bytevector-copy! array (fx* i size) one 0 size)
                  (let ([match (selection-from-c who terminal-pointer one)])
                    (loop (fx- i 1) (if match (cons match acc) acc)))))))))))

(define (search-matches s)
  (read-matches 'search-matches s (GhosttySearchData matches)))

(define (search-viewport-matches s)
  (read-matches 'search-viewport-matches s (GhosttySearchData viewport-matches)))

(define (select-match! who s option)
  (with-search-change who s (pointer)
    (let ([code (ghostty-search-set pointer option #f)])
      (cond
        [(fx= code (GhosttyResult success)) (selected-match who s pointer)]
        [(fx= code (GhosttyResult no-value)) #f]
        [else (raise-result who code)]))))

(define (search-select-next! s)
  (select-match! 'search-select-next! s (GhosttySearchOption select-next)))

(define (search-select-previous! s)
  (select-match! 'search-select-previous! s (GhosttySearchOption select-prev)))

(define (search-set-scroll! s policy)
  (define who 'search-set-scroll!)
  (let ([bv (make-bytevector 4)])
    (bytevector-s32-native-set! bv 0 (symbol->c-enum GhosttySearchScroll policy who))
    (with-search who s (pointer)
      (check-result who (ghostty-search-set pointer (GhosttySearchOption select-scroll) bv)))))

(define-record-type (selection-gesture new-selection-gesture selection-gesture?)
  (nongenerative)
  (sealed #t)
  (opaque #t)
  (fields native terminal
    (mutable behaviors) (mutable word-boundaries) (mutable repeat-distance)
    (mutable repeat-interval)))

(define-value-record gesture-geometry
  (fields columns cell-width padding-left screen-height))

(define-value-record gesture-state (fields click-count dragged? autoscroll behavior anchor))

(define (check-selection-gesture who g)
  (check who (selection-gesture? g) g "a selection-gesture"))

(define-syntax with-gesture
  (syntax-rules ()
    [(_ who g-expr (pointer terminal-pointer) body ...)
     (let ([g g-expr])
       (check-selection-gesture who g)
       (with-native who (selection-gesture-native g) "selection gesture" (pointer)
         (let ([terminal-pointer (native-pointer (terminal-native (selection-gesture-terminal g)))])
           (when (eqv? terminal-pointer 0) (assertion-violationf who "the terminal is closed"))
           body ...)))]))

(define (make-selection-gesture t)
  (define who 'make-selection-gesture)
  (check-terminal who t)
  (release-dropped-natives!)
  (let ([out (pointer-out)])
    (check-result who (ghostty-selection-gesture-new 0 out))
    (let* ([terminal-native* (terminal-native t)]
           [native (make-native (pointer-in out)
                     ;; After the terminal is freed its pages have already
                     ;; dropped the gesture's references, and NULL says so.
                     (lambda (pointer)
                       (ghostty-selection-gesture-free pointer (native-pointer terminal-native*)))
                     (terminal-lock t))])
      (register-native! (new-selection-gesture native t #f #f #f #f) native))))

(define (selection-gesture-close! g)
  (check-selection-gesture 'selection-gesture-close! g)
  (close-native! 'selection-gesture-close! (selection-gesture-native g) "selection gesture"))

(define (selection-gesture-closed? g)
  (check-selection-gesture 'selection-gesture-closed? g)
  (native-closed? (selection-gesture-native g)))

(define (selection-gesture-set-behaviors! g single double triple)
  (define who 'selection-gesture-set-behaviors!)
  (check-selection-gesture who g)
  (for-each (lambda (b) (symbol->c-enum GhosttySelectionGestureBehavior b who))
    (list single double triple))
  (selection-gesture-behaviors-set! g (list single double triple)))

(define (selection-gesture-set-word-boundaries! g chars)
  (define who 'selection-gesture-set-word-boundaries!)
  (check-selection-gesture who g)
  (check who (or (not chars) (string? chars)) chars "a string or #f")
  (selection-gesture-word-boundaries-set! g chars))

(define (selection-gesture-set-repeat-limits! g distance interval)
  (define who 'selection-gesture-set-repeat-limits!)
  (check-selection-gesture who g)
  (check who (and (real? distance) (>= distance 0)) distance "a nonnegative real number")
  (check who (u64? interval) interval "an unsigned 64-bit integer")
  (selection-gesture-repeat-distance-set! g (inexact distance))
  (selection-gesture-repeat-interval-set! g interval))

(define (check-geometry who geometry)
  (check who (gesture-geometry? geometry) geometry "a gesture-geometry")
  (check who (and (u32? (gesture-geometry-columns geometry))
                  (> (gesture-geometry-columns geometry) 0)
                  (u32? (gesture-geometry-cell-width geometry))
                  (> (gesture-geometry-cell-width geometry) 0)
                  (u32? (gesture-geometry-padding-left geometry))
                  (u32? (gesture-geometry-screen-height geometry))
                  (> (gesture-geometry-screen-height geometry) 0))
    geometry "a geometry of unsigned 32-bit sizes, none of columns, cell width and height zero"))

(define (check-position who x y)
  (check who (and (real? x) (finite? x)) x "a finite real number")
  (check who (and (real? y) (finite? y)) y "a finite real number"))

;; The event copies what it is given, so its option buffers need not
;; outlive the call.
(define (apply-gesture-event! who g type set-options!)
  (with-gesture who g (pointer terminal-pointer)
    (check-not-in-callback who (selection-gesture-terminal g))
    (let ([out (pointer-out)])
      (check-result who (ghostty-selection-gesture-event-new 0 out type))
      (let ([event (pointer-in out)])
        (dynamic-wind
          void
          (lambda ()
            (set-options!
              (lambda (option value)
                (check-result who (ghostty-selection-gesture-event-set event option value)))
              terminal-pointer)
            (let* ([selection (c-sized-struct GhosttySelection)]
                   [code (ghostty-selection-gesture-event pointer terminal-pointer event selection)])
              (cond
                [(fx= code (GhosttyResult success))
                 (install-selection! who terminal-pointer selection)
                 (selection-from-c who terminal-pointer selection)]
                [(fx= code (GhosttyResult no-value)) #f]
                [else (raise-result who code)])))
          (lambda () (ghostty-selection-gesture-event-free event)))))))

(define (gesture-option-position x y)
  (let ([bv (c-struct GhosttySurfacePosition)])
    (c-set! GhosttySurfacePosition (x) bv (inexact x))
    (c-set! GhosttySurfacePosition (y) bv (inexact y))
    bv))

(define (gesture-option-geometry geometry)
  (let ([bv (c-struct GhosttySelectionGestureGeometry)])
    (c-set! GhosttySelectionGestureGeometry (columns) bv (gesture-geometry-columns geometry))
    (c-set! GhosttySelectionGestureGeometry (cell_width) bv (gesture-geometry-cell-width geometry))
    (c-set! GhosttySelectionGestureGeometry (padding_left) bv (gesture-geometry-padding-left geometry))
    (c-set! GhosttySelectionGestureGeometry (screen_height) bv
      (gesture-geometry-screen-height geometry))
    bv))

(define (set-word-boundaries! who g set!)
  (let ([chars (selection-gesture-word-boundaries g)])
    (when chars
      (let ([codepoints (codepoints->immobile who chars)] [bv (c-struct GhosttyCodepoints)])
        (c-set! GhosttyCodepoints (ptr) bv (object->reference-address codepoints))
        (c-set! GhosttyCodepoints (len) bv (string-length chars))
        (set! (GhosttySelectionGestureEventOption word-boundary-codepoints) bv)
        (keep-live codepoints)))))

(define (u64-bytes n) (let ([bv (make-bytevector 8)]) (bytevector-u64-native-set! bv 0 n) bv))

(define (selection-gesture-press! g p x y time)
  (define who 'selection-gesture-press!)
  (check-selection-gesture who g)
  (check-grid-point who p)
  (check-position who x y)
  (check who (or (not time) (u64? time)) time "an unsigned 64-bit integer or #f")
  (apply-gesture-event! who g (GhosttySelectionGestureEventType press)
    (lambda (set! terminal-pointer)
      (set! (GhosttySelectionGestureEventOption ref) (grid-ref who terminal-pointer p))
      (set! (GhosttySelectionGestureEventOption position) (gesture-option-position x y))
      (when time (set! (GhosttySelectionGestureEventOption time-ns) (u64-bytes time)))
      (when (selection-gesture-repeat-distance g)
        (let ([bv (make-bytevector 8)])
          (bytevector-ieee-double-native-set! bv 0 (selection-gesture-repeat-distance g))
          (set! (GhosttySelectionGestureEventOption repeat-distance) bv))
        (set! (GhosttySelectionGestureEventOption repeat-interval-ns)
          (u64-bytes (selection-gesture-repeat-interval g))))
      (let ([behaviors (selection-gesture-behaviors g)])
        (when behaviors
          (let ([bv (c-struct GhosttySelectionGestureBehaviors)])
            (c-set! GhosttySelectionGestureBehaviors (single_click) bv
              (symbol->c-enum GhosttySelectionGestureBehavior (car behaviors) who))
            (c-set! GhosttySelectionGestureBehaviors (double_click) bv
              (symbol->c-enum GhosttySelectionGestureBehavior (cadr behaviors) who))
            (c-set! GhosttySelectionGestureBehaviors (triple_click) bv
              (symbol->c-enum GhosttySelectionGestureBehavior (caddr behaviors) who))
            (set! (GhosttySelectionGestureEventOption behaviors) bv))))
      (set-word-boundaries! who g set!))))

(define (drag-event! who g type x y geometry rectangle? set-target!)
  (check-position who x y)
  (check-geometry who geometry)
  (apply-gesture-event! who g type
    (lambda (set! terminal-pointer)
      (set-target! set! terminal-pointer)
      (set! (GhosttySelectionGestureEventOption position) (gesture-option-position x y))
      (set! (GhosttySelectionGestureEventOption geometry) (gesture-option-geometry geometry))
      (set! (GhosttySelectionGestureEventOption rectangle) (bytevector (if rectangle? 1 0)))
      (set-word-boundaries! who g set!))))

(define (drag! who g p x y geometry rectangle?)
  (check-selection-gesture who g)
  (check-grid-point who p)
  (drag-event! who g (GhosttySelectionGestureEventType drag) x y geometry rectangle?
    (lambda (set! terminal-pointer)
      (set! (GhosttySelectionGestureEventOption ref) (grid-ref who terminal-pointer p)))))

(define (selection-gesture-drag! g p x y geometry)
  (drag! 'selection-gesture-drag! g p x y geometry #f))

(define (selection-gesture-drag-rectangle! g p x y geometry)
  (drag! 'selection-gesture-drag-rectangle! g p x y geometry #t))

(define (autoscroll-tick! who g column row x y geometry rectangle?)
  (check-selection-gesture who g)
  (check-u16 who column)
  (check-u32 who row)
  (drag-event! who g (GhosttySelectionGestureEventType autoscroll-tick) x y geometry rectangle?
    (lambda (set! terminal-pointer)
      (let ([bv (c-struct GhosttyPointCoordinate)])
        (c-set! GhosttyPointCoordinate (x) bv column)
        (c-set! GhosttyPointCoordinate (y) bv row)
        (set! (GhosttySelectionGestureEventOption viewport) bv)))))

(define (selection-gesture-autoscroll-tick! g column row x y geometry)
  (autoscroll-tick! 'selection-gesture-autoscroll-tick! g column row x y geometry #f))

(define (selection-gesture-autoscroll-tick-rectangle! g column row x y geometry)
  (autoscroll-tick! 'selection-gesture-autoscroll-tick-rectangle! g column row x y geometry #t))

(define (selection-gesture-release! g p)
  (define who 'selection-gesture-release!)
  (check-selection-gesture who g)
  (when p (check-grid-point who p))
  (apply-gesture-event! who g (GhosttySelectionGestureEventType release)
    (lambda (set! terminal-pointer)
      (when p
        (set! (GhosttySelectionGestureEventOption ref) (grid-ref who terminal-pointer p))))))

(define (selection-gesture-deep-press! g)
  (define who 'selection-gesture-deep-press!)
  (check-selection-gesture who g)
  (apply-gesture-event! who g (GhosttySelectionGestureEventType deep-press)
    (lambda (set! terminal-pointer) (set-word-boundaries! who g set!))))

(define (selection-gesture-reset! g)
  (define who 'selection-gesture-reset!)
  (with-gesture who g (pointer terminal-pointer)
    (check-not-in-callback who (selection-gesture-terminal g))
    (ghostty-selection-gesture-reset pointer terminal-pointer)))

(define (selection-gesture-state g)
  (define who 'selection-gesture-state)
  (with-gesture who g (pointer terminal-pointer)
    (let ([get (lambda (key size ref)
                 (let* ([out (make-bytevector size 0)]
                        [code (ghostty-selection-gesture-get pointer terminal-pointer key out)])
                   (cond
                     [(fx= code (GhosttyResult success)) (ref out)]
                     [(fx= code (GhosttyResult no-value)) #f]
                     [else (raise-result who code)])))])
      (make-gesture-state
        (get (GhosttySelectionGestureData click-count) 1 (lambda (bv) (bytevector-u8-ref bv 0)))
        (get (GhosttySelectionGestureData dragged) 1 bool-ref)
        (get (GhosttySelectionGestureData autoscroll) 4
          (lambda (bv) (c-enum->symbol GhosttySelectionGestureAutoscroll (int-ref bv))))
        (get (GhosttySelectionGestureData behavior) 4
          (lambda (bv) (c-enum->symbol GhosttySelectionGestureBehavior (int-ref bv))))
        (let* ([ref (c-sized-struct GhosttyGridRef)]
               [code (ghostty-selection-gesture-get pointer terminal-pointer
                       (GhosttySelectionGestureData anchor) ref)])
          (cond
            [(fx= code (GhosttyResult success)) (grid-ref->point who terminal-pointer ref 'screen)]
            [(fx= code (GhosttyResult no-value)) #f]
            [else (raise-result who code)]))))))
