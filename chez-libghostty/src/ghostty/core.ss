(define-condition-type &ghostty &error
  make-ghostty-condition ghostty-error?
  (result ghostty-error-result)
  (code ghostty-error-code))

(define (raise-result who code . irritants)
  (let ([result (or (c-enum->symbol GhosttyResult code) 'unknown)])
    (raise
      (condition
        (make-ghostty-condition result code)
        (make-who-condition who)
        (make-message-condition (format "libghostty-vt failed: ~a" result))
        (make-irritants-condition irritants)))))

(define-syntax check-result
  (syntax-rules ()
    [(_ who expr irritant ...)
     (let ([code expr])
       (unless (fx= code 0) (raise-result who code irritant ...)))]))

(define (check who ok? value what)
  (unless ok? (assertion-violationf who "~s is not ~a" value what)))

(define (u8? x) (and (fixnum? x) (fx<= 0 x 255)))
(define (u16? x) (and (fixnum? x) (fx<= 0 x 65535)))
(define (u32? x) (and (fixnum? x) (fx<= 0 x #xFFFFFFFF)))
(define (u64? x) (and (integer? x) (exact? x) (<= 0 x #xFFFFFFFFFFFFFFFF)))
(define (size? x) (u64? x))
(define (s64? x)
  (and (integer? x) (exact? x) (<= #x-8000000000000000 x #x7FFFFFFFFFFFFFFF)))

(define (check-u8 who x) (check who (u8? x) x "an integer from 0 to 255"))
(define (check-u16 who x) (check who (u16? x) x "an integer from 0 to 65535"))
(define (check-u32 who x) (check who (u32? x) x "an unsigned 32-bit integer"))
(define (check-size who x) (check who (size? x) x "an unsigned 64-bit integer"))
(define (check-boolean who x) (check who (boolean? x) x "a boolean"))
(define (enum-set-of? set example)
  (and (enum-set? set) (enum-set=? (enum-set-universe set) (enum-set-universe example))))

(define (check-procedure-or-false who x)
  (check who (or (not x) (procedure? x)) x "a procedure or #f"))

(define (check-slice who bv start end)
  (check who (bytevector? bv) bv "a bytevector")
  (unless (and (fixnum? start) (fixnum? end) (fx<= 0 start end (bytevector-length bv)))
    (assertion-violationf who "~s and ~s are not a range of a bytevector of length ~s"
      start end (bytevector-length bv))))

(define (text->bytevector who text)
  (cond
    [(string? text) (string->utf8 text)]
    [(bytevector? text) text]
    [else (assertion-violationf who "~s is not a string or bytevector" text)]))

;; Bytes whose address goes into a structure must not move while C holds
;; the address, so they are copied into an immobile bytevector.
(define (immobile-copy bv)
  (let ([copy (make-immobile-bytevector (bytevector-length bv))])
    (bytevector-copy! bv 0 copy 0 (bytevector-length bv))
    copy))

(define (immobile-address bv)
  (if (fx= (bytevector-length bv) 0) 0 (object->reference-address bv)))

;; Never empty, so that an empty string still passes a non-NULL pointer:
;; NULL asks libghostty-vt for its defaults.
(define (codepoints->immobile who chars)
  (let ([bv (make-immobile-bytevector (fx* 4 (fxmax 1 (string-length chars))) 0)])
    (do ([i 0 (fx+ i 1)]) ((fx= i (string-length chars)) bv)
      (bytevector-u32-native-set! bv (fx* 4 i) (char->integer (string-ref chars i))))))

;; C lends these only until its next call on the same object.
(define (borrowed-bytes bv offset)
  (address->bytevector
    (bytevector-u64-native-ref bv offset)
    (bytevector-u64-native-ref bv (fx+ offset 8))))

(define (borrowed-string-at address)
  (let ([p (make-ftype-pointer GhosttyString address)])
    (utf8->string
      (address->bytevector (ftype-ref GhosttyString (ptr) p) (ftype-ref GhosttyString (len) p)))))

(define (borrowed-bytes-at address)
  (let ([p (make-ftype-pointer GhosttyString address)])
    (address->bytevector (ftype-ref GhosttyString (ptr) p) (ftype-ref GhosttyString (len) p))))

;; Memory the library allocated goes back through its own allocator.
(define (take-allocated-bytes out-ptr out-len)
  (let* ([ptr (bytevector-u64-native-ref out-ptr 0)]
         [len (bytevector-u64-native-ref out-len 0)]
         [bv (address->bytevector ptr len)])
    (unless (eqv? ptr 0) (ghostty-free 0 ptr len))
    bv))

;; Records otherwise compare by identity under equal?, and enumeration sets
;; inside them by identity too.

(define (structural-equality! rtd)
  (let ([accessors
         (let ([n (vector-length (record-type-field-names rtd))])
           (let loop ([i (fx- n 1)] [acc '()])
             (if (fx< i 0) acc (loop (fx- i 1) (cons (record-accessor rtd i) acc)))))])
    (record-type-equal-procedure rtd
      (lambda (a b equal?)
        (for-all
          (lambda (field)
            (let ([x (field a)] [y (field b)])
              (if (and (enum-set? x) (enum-set? y))
                  (and (enum-set-of? x y) (enum-set=? x y))
                  (equal? x y))))
          accessors)))
    (record-type-hash-procedure rtd
      (lambda (a hash)
        (fold-left
          (lambda (h field)
            (let ([x (field a)])
              (fxlogxor (fx* 31 (fxand h #xffffff))
                (if (enum-set? x) (hash (enum-set->list x)) (hash x)))))
          17 accessors)))))

(define-syntax define-value-record
  (lambda (x)
    (syntax-case x (fields)
      [(_ name (fields field ...) clause ...)
       (with-syntax ([(installed) (generate-temporaries '(installed))])
         #'(begin
             (define-record-type name (nongenerative) (sealed #t) (fields field ...) clause ...)
             (define installed (structural-equality! (record-type-descriptor name)))))])))

(define-value-record rgb
  (fields red green blue)
  (protocol
    (lambda (new)
      (lambda (red green blue)
        (check-u8 'make-rgb red)
        (check-u8 'make-rgb green)
        (check-u8 'make-rgb blue)
        (new red green blue)))))

(define (check-rgb who x) (check who (rgb? x) x "an rgb color"))

(define (rgb-at bv offset)
  (make-rgb (bytevector-u8-ref bv offset)
            (bytevector-u8-ref bv (fx+ offset 1))
            (bytevector-u8-ref bv (fx+ offset 2))))

(define (rgb-set! bv offset color)
  (bytevector-u8-set! bv offset (rgb-red color))
  (bytevector-u8-set! bv (fx+ offset 1) (rgb-green color))
  (bytevector-u8-set! bv (fx+ offset 2) (rgb-blue color)))

(define (rgb->c color)
  (let ([bv (c-struct GhosttyColorRgb)]) (rgb-set! bv 0 color) bv))

(define (palette-at bv offset)
  (let ([palette (make-vector 256)])
    (do ([i 0 (fx+ i 1)]) ((fx= i 256) palette)
      (vector-set! palette i (rgb-at bv (fx+ offset (fx* 3 i)))))))

(define (check-palette who palette)
  (check who (and (vector? palette) (fx= (vector-length palette) 256)
                  (for-all rgb? (vector->list palette)))
    palette "a vector of 256 rgb colors"))

(define (palette->c palette)
  (let ([bv (make-bytevector (fx* 3 256))])
    (do ([i 0 (fx+ i 1)]) ((fx= i 256) bv)
      (rgb-set! bv (fx* 3 i) (vector-ref palette i)))))

(define-value-record style
  (fields foreground background underline-color bold? italic? faint? blink? inverse? invisible?
    strikethrough? overline? underline))

(define-syntax style-color-ref
  (syntax-rules ()
    [(_ T (field ...) bv)
     (let ([tag (c-ref T (field ... tag) bv)])
       (cond
         [(fx= tag (GhosttyStyleColorTag palette)) (c-ref T (field ... value palette) bv)]
         [(fx= tag (GhosttyStyleColorTag rgb))
          (make-rgb (c-ref T (field ... value rgb r) bv)
                    (c-ref T (field ... value rgb g) bv)
                    (c-ref T (field ... value rgb b) bv))]
         [else #f]))]))

(define (style-at bv)
  (make-style
    (style-color-ref GhosttyStyle (fg_color) bv)
    (style-color-ref GhosttyStyle (bg_color) bv)
    (style-color-ref GhosttyStyle (underline_color) bv)
    (c-ref GhosttyStyle (bold) bv)
    (c-ref GhosttyStyle (italic) bv)
    (c-ref GhosttyStyle (faint) bv)
    (c-ref GhosttyStyle (blink) bv)
    (c-ref GhosttyStyle (inverse) bv)
    (c-ref GhosttyStyle (invisible) bv)
    (c-ref GhosttyStyle (strikethrough) bv)
    (c-ref GhosttyStyle (overline) bv)
    (or (c-enum->symbol GhosttySgrUnderline (c-ref GhosttyStyle (underline) bv)) 'single)))

;; A terminal and the objects that hold references into it (its searches,
;; tracked points and gestures) share one lock, because freeing or using
;; any of them touches the terminal's state.
(define-record-type lock
  (nongenerative)
  (sealed #t)
  (fields mutex runs-callbacks? (mutable busy?) (mutable poisoned?))
  (protocol (lambda (new) (lambda (runs-callbacks?) (new (make-mutex) runs-callbacks? #f #f)))))

;; Inside a terminal's callbacks this thread holds that terminal's lock, so
;; waiting there for another terminal's lock can deadlock: the thread
;; holding it may be inside a callback waiting for ours. Every other lock is
;; taken last and held briefly, so waiting for it is safe. The mutex is
;; recursive, so a lock this thread already holds is still taken at once.
(define in-terminal-callback? (make-thread-parameter #f))

(define (acquire-lock! who lock what)
  (if (and (in-terminal-callback?) (lock-runs-callbacks? lock))
      (unless (mutex-acquire (lock-mutex lock) #f)
        (assertion-violationf who
          "cannot wait inside a terminal's callback for a ~a another thread is using" what))
      (mutex-acquire (lock-mutex lock))))

(define-syntax with-lock
  (syntax-rules ()
    [(_ who lock-expr what body ...)
     (let ([lock lock-expr])
       (dynamic-wind
         (lambda () (acquire-lock! who lock what))
         (lambda () body ...)
         (lambda () (mutex-release (lock-mutex lock)))))]))

;; The representative a guardian returns for a dropped owner: it holds what
;; freeing needs and never the owner itself, or the owner would stay
;; reachable through the guardian.
(define-record-type native
  (nongenerative)
  (sealed #t)
  (fields (mutable pointer) free lock (mutable borrowers))
  (protocol (lambda (new) (lambda (pointer free lock) (new pointer free lock '())))))

(define native-guardian (make-guardian))
(define deferred-mutex (make-mutex))
(define deferred-natives '())

;; The caller holds the native's lock. A borrower, such as a snapshot
;; decoder restoring history into a terminal, would use freed memory if it
;; outlived what it borrows.
(define (release-native! n)
  (let ([pointer (native-pointer n)])
    (unless (eqv? pointer 0)
      (for-each
        (lambda (borrower)
          (with-mutex (lock-mutex (native-lock borrower)) (release-native! borrower)))
        (native-borrowers n))
      (native-borrowers-set! n '())
      (native-pointer-set! n 0)
      ((native-free n) pointer))))

(define (register-native! owner n)
  (native-guardian owner n)
  owner)

;; Freeing inside a callback would pull state from under libghostty-vt, so
;; a dropped object whose terminal is busy waits for a later call. A
;; poisoned object is never freed, since its native state is unknown.
(define (try-release! n)
  (let ([lock (native-lock n)])
    (and (mutex-acquire (lock-mutex lock) #f)
         (dynamic-wind
           void
           (lambda ()
             (let ([free? (not (lock-busy? lock))])
               (when (and free? (not (lock-poisoned? lock))) (release-native! n))
               free?))
           (lambda () (mutex-release (lock-mutex lock)))))))

(define (release-dropped-natives!)
  (let ([candidates
         (with-mutex deferred-mutex
           (let loop ([acc deferred-natives])
             (let ([n (native-guardian)])
               (if n
                   (loop (cons n acc))
                   (begin (set! deferred-natives '()) acc)))))])
    (let ([waiting (filter (lambda (n) (not (try-release! n))) candidates)])
      (unless (null? waiting)
        (with-mutex deferred-mutex
          (set! deferred-natives (append waiting deferred-natives)))))))

(define (check-usable who lock what)
  (when (lock-poisoned? lock)
    (assertion-violationf who "the ~a was left unusable by a callback that escaped" what)))

(define-syntax with-native
  (syntax-rules ()
    [(_ who n what (pointer) body ...)
     (let ([native n])
       (let ([lock (native-lock native)])
         (with-lock who lock what
           (check-usable who lock what)
           (let ([pointer (native-pointer native)])
             (when (eqv? pointer 0) (assertion-violationf who "the ~a is closed" what))
             body ...))))]))

(define (close-native! who n what)
  (let ([lock (native-lock n)])
    (with-lock who lock what
      (when (lock-busy? lock)
        (assertion-violationf who "cannot close a ~a inside one of its callbacks" what))
      (when (lock-poisoned? lock)
        (assertion-violationf who
          "the ~a was left unusable by a callback that escaped and cannot be freed" what))
      (release-native! n))))

(define (native-closed? n) (eqv? (native-pointer n) 0))

(define (pointer-out) (make-bytevector 8 0))
(define (pointer-in bv) (bytevector-u64-native-ref bv 0))

;; C keeps these entry points for the life of the process.
(define (make-callable code)
  (lock-object code)
  (foreign-callable-entry-point code))
