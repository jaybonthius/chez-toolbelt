(define-value-record kitty-placement
  (fields image-id placement-id virtual? x-offset y-offset z layer pixel-width pixel-height
    columns rows viewport-column viewport-row source-x source-y source-width source-height))

(define-value-record kitty-image (fields id number width height format generation data))

(define (kitty-graphics who pointer)
  (let* ([out (pointer-out)] [code (ghostty-terminal-get pointer (GhosttyTerminalData kitty-graphics) out)])
    (cond
      [(fx= code (GhosttyResult success)) (pointer-in out)]
      [(fx= code (GhosttyResult no-value)) #f]
      [else (raise-result who code)])))

(define (kitty-layer z)
  (cond
    [(< z -1073741824) 'below-background]
    [(< z 0) 'below-text]
    [else 'above-text]))

(define (terminal-kitty-generation t)
  (define who 'terminal-kitty-generation)
  (with-terminal who t (pointer)
    (let ([graphics (kitty-graphics who pointer)])
      (and graphics
           (let ([out (make-bytevector 8 0)])
             (check-result who
               (ghostty-kitty-graphics-get graphics (GhosttyKittyGraphicsData generation) out))
             (u64-ref out))))))

(define (terminal-kitty-placements t)
  (define who 'terminal-kitty-placements)
  (with-terminal who t (pointer)
    (let ([graphics (kitty-graphics who pointer)])
      (if (not graphics)
          '()
          (let ([iterator-handle (pointer-out)])
            (check-result who (ghostty-kitty-graphics-placement-iterator-new 0 iterator-handle))
            (let ([iterator (pointer-in iterator-handle)])
              (dynamic-wind
                void
                (lambda ()
                  (check-result who
                    (ghostty-kitty-graphics-get graphics
                      (GhosttyKittyGraphicsData placement-iterator) iterator-handle))
                  (let loop ([acc '()])
                    (if (ghostty-kitty-graphics-placement-next iterator)
                        (loop (cons (read-placement who pointer graphics iterator) acc))
                        (reverse acc))))
                (lambda () (ghostty-kitty-graphics-placement-iterator-free iterator)))))))))

(define (read-placement who pointer graphics iterator)
  (let* ([get (lambda (key size ref)
                (let ([out (make-bytevector size 0)])
                  (check-result who (ghostty-kitty-graphics-placement-get iterator key out))
                  (ref out)))]
         [image-id (get (GhosttyKittyGraphicsPlacementData image-id) 4 u32-ref)]
         [z (get (GhosttyKittyGraphicsPlacementData z) 4 int-ref)]
         [image (ghostty-kitty-graphics-image graphics image-id)]
         [info (c-sized-struct GhosttyKittyGraphicsPlacementRenderInfo)]
         [has-info? (and (not (eqv? image 0))
                         (fx= (ghostty-kitty-graphics-placement-render-info iterator image pointer info)
                              (GhosttyResult success)))]
         [visible? (and has-info? (c-ref GhosttyKittyGraphicsPlacementRenderInfo (viewport_visible) info))])
    (let-syntax ([info-ref (syntax-rules ()
                             [(_ name) (and has-info?
                                            (c-ref GhosttyKittyGraphicsPlacementRenderInfo (name) info))])])
      (make-kitty-placement
        image-id
        (get (GhosttyKittyGraphicsPlacementData placement-id) 4 u32-ref)
        (get (GhosttyKittyGraphicsPlacementData is-virtual) 1 bool-ref)
        (get (GhosttyKittyGraphicsPlacementData x-offset) 4 u32-ref)
        (get (GhosttyKittyGraphicsPlacementData y-offset) 4 u32-ref)
        z
        (kitty-layer z)
        (info-ref pixel_width)
        (info-ref pixel_height)
        (info-ref grid_cols)
        (info-ref grid_rows)
        (and visible? (info-ref viewport_col))
        (and visible? (info-ref viewport_row))
        (info-ref source_x)
        (info-ref source_y)
        (info-ref source_width)
        (info-ref source_height)))))

(define (terminal-kitty-image t id)
  (define who 'terminal-kitty-image)
  (check-u32 who id)
  (with-terminal who t (pointer)
    (let* ([graphics (kitty-graphics who pointer)]
           [image (if graphics (ghostty-kitty-graphics-image graphics id) 0)])
      (and (not (eqv? image 0))
           (let ([get (lambda (key size ref)
                        (let ([out (make-bytevector size 0)])
                          (check-result who (ghostty-kitty-graphics-image-get image key out))
                          (ref out)))])
             (make-kitty-image
               (get (GhosttyKittyGraphicsImageData id) 4 u32-ref)
               (get (GhosttyKittyGraphicsImageData number) 4 u32-ref)
               (get (GhosttyKittyGraphicsImageData width) 4 u32-ref)
               (get (GhosttyKittyGraphicsImageData height) 4 u32-ref)
               (get (GhosttyKittyGraphicsImageData format) 4
                 (lambda (bv) (c-enum->symbol GhosttyKittyImageFormat (int-ref bv))))
               (get (GhosttyKittyGraphicsImageData generation) 8 u64-ref)
               (address->bytevector
                 (get (GhosttyKittyGraphicsImageData data-ptr) 8 u64-ref)
                 (get (GhosttyKittyGraphicsImageData data-len) 8 u64-ref))))))))
