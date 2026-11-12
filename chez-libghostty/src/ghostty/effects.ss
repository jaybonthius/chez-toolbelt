(define-record-type effects
  (nongenerative)
  (sealed #t)
  (fields lock (mutable failure) (mutable replies)
    (mutable pty-write) (mutable bell) (mutable enquiry) (mutable xtversion)
    (mutable title-changed) (mutable size) (mutable color-scheme) (mutable device-attributes)
    (mutable pwd-changed) (mutable clipboard-write) (mutable clipboard-read)
    (mutable desktop-notification) (mutable progress-report) (mutable unknown-sequence)
    (mutable semantic-prompt) (mutable program-status) (mutable resize-window) (mutable reset)
    (mutable render-hold))
  (protocol
    (lambda (new)
      (lambda (lock)
        (new lock #f '() #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f)))))

;; The table holds handlers weakly, or it would keep a dropped terminal,
;; and handlers closing over it, alive.
(define effects-mutex (make-mutex))
(define effects-by-id (make-eqv-hashtable))
(define next-effects-id 1)

(define (register-effects! e)
  (with-mutex effects-mutex
    (let ([id next-effects-id])
      (set! next-effects-id (+ id 1))
      (hashtable-set! effects-by-id id (weak-cons e #f))
      id)))

(define (unregister-effects! id)
  (with-mutex effects-mutex (hashtable-delete! effects-by-id id)))

(define (lookup-effects id)
  (with-mutex effects-mutex
    (let ([entry (hashtable-ref effects-by-id id #f)])
      (and entry (effects? (car entry)) (car entry)))))

(define (free-effect-replies! e)
  (for-each foreign-free (effects-replies e))
  (effects-replies-set! e '()))

;; A continuation that escapes a handler skips the rest of libghostty-vt's
;; work, leaving the terminal in an unknown state, so the terminal becomes
;; unusable.
(define (call-handler e default thunk)
  (let ([escaped? #t])
    (dynamic-wind
      void
      (lambda ()
        (let ([value (guard (c [#t (unless (effects-failure e) (effects-failure-set! e (list c)))
                                   default])
                       (thunk))])
          (set! escaped? #f)
          value))
      (lambda () (when escaped? (lock-poisoned?-set! (effects-lock e) #t))))))

(define (dispatch-effect userdata handler-ref default proc)
  (let* ([e (lookup-effects userdata)] [handler (and e (handler-ref e))])
    (if handler
        (call-handler e default (lambda () (proc handler)))
        default)))

(define-syntax define-effect
  (syntax-rules ()
    [(_ callable handler-ref default (param ...) (type ...) result body)
     (define callable
       (make-callable
         (foreign-callable
           (lambda (terminal userdata param ...)
             (dispatch-effect userdata handler-ref default
               (lambda (handler) (body handler terminal))))
           (uptr uptr type ...)
           result)))]))

(define-value-record clipboard-content
  (fields mime data)
  (protocol
    (lambda (new)
      (lambda (mime data)
        (check 'make-clipboard-content (string? mime) mime "a string")
        (check 'make-clipboard-content (bytevector? data) data "a bytevector")
        (new mime data)))))

(define-value-record clipboard-write-request
  (fields location contents name granted? can-remember?))
(define-value-record clipboard-read-request
  (fields location mimes list? name granted? can-remember?))
(define-value-record desktop-notification (fields title body))
(define-value-record progress-report (fields state progress))
(define-value-record unknown-sequence (fields kind content truncated? terminator))
(define-value-record semantic-prompt (fields kind prompt-kind exit-code command error))
(define-value-record program-status (fields state kind progress id app title message))
(define-value-record terminal-size (fields rows columns cell-width cell-height))

(define-value-record device-attributes
  (fields conformance-level features device-type firmware-version rom-cartridge unit-id)
  (protocol
    (lambda (new)
      (lambda (conformance-level features device-type firmware-version rom-cartridge unit-id)
        (define who 'make-device-attributes)
        (check-u16 who conformance-level)
        (check who (and (list? features) (fx<= (length features) 64) (for-all u16? features))
          features "a list of at most 64 integers from 0 to 65535")
        (check-u16 who device-type)
        (check-u16 who firmware-version)
        (check-u16 who rom-cartridge)
        (check-u32 who unit-id)
        (new conformance-level features device-type firmware-version rom-cartridge unit-id)))))

(define (read-borrowed-string terminal key)
  (let ([out (make-bytevector (ftype-sizeof GhosttyString) 0)])
    (if (fx= (ghostty-terminal-get terminal key out) (GhosttyResult success))
        (utf8->string (borrowed-bytes out 0))
        "")))

(define-effect pty-write-callable effects-pty-write (void) (data len) (uptr size_t) void
  (lambda (handler terminal) (handler (address->bytevector data len))))

(define-effect bell-callable effects-bell (void) () () void
  (lambda (handler terminal) (handler)))

(define-effect title-changed-callable effects-title-changed (void) () () void
  (lambda (handler terminal) (handler (read-borrowed-string terminal (GhosttyTerminalData title)))))

(define-effect pwd-changed-callable effects-pwd-changed (void) () () void
  (lambda (handler terminal) (handler (read-borrowed-string terminal (GhosttyTerminalData pwd)))))

;; The reply must outlive the callback: libghostty-vt writes it out after
;; the callback returns. It lives in foreign memory until the operation
;; that triggered the callback returns.
(define (reply-callable handler-ref who)
  (make-callable
    (foreign-callable
      (lambda (out terminal userdata)
        (ftype-set! GhosttyString (ptr) out 0)
        (ftype-set! GhosttyString (len) out 0)
        (dispatch-effect userdata handler-ref (void)
          (lambda (handler)
            (let* ([bytes (text->bytevector who (handler))]
                   [n (bytevector-length bytes)])
              (unless (fx= n 0)
                (let ([address (foreign-alloc n)] [e (lookup-effects userdata)])
                  (memcpy-to address bytes n)
                  (effects-replies-set! e (cons address (effects-replies e)))
                  (ftype-set! GhosttyString (ptr) out address)
                  (ftype-set! GhosttyString (len) out n)))))))
      (uptr uptr)
      (& GhosttyString))))

(define enquiry-callable (reply-callable effects-enquiry 'terminal-set-enquiry-handler!))
(define xtversion-callable (reply-callable effects-xtversion 'terminal-set-xtversion-handler!))

(define-effect size-callable effects-size #f (out) (uptr) stdbool
  (lambda (handler terminal)
    (let ([size (handler)])
      (and size
           (let ([p (make-ftype-pointer GhosttySizeReportSize out)])
             (check-terminal-size 'terminal-set-size-handler! size)
             (ftype-set! GhosttySizeReportSize (rows) p (terminal-size-rows size))
             (ftype-set! GhosttySizeReportSize (columns) p (terminal-size-columns size))
             (ftype-set! GhosttySizeReportSize (cell_width) p (terminal-size-cell-width size))
             (ftype-set! GhosttySizeReportSize (cell_height) p (terminal-size-cell-height size))
             #t)))))

(define (check-terminal-size who size)
  (check who (terminal-size? size) size "a terminal-size")
  (check-u16 who (terminal-size-rows size))
  (check-u16 who (terminal-size-columns size))
  (check-u32 who (terminal-size-cell-width size))
  (check-u32 who (terminal-size-cell-height size)))

(define-effect color-scheme-callable effects-color-scheme #f (out) (uptr) stdbool
  (lambda (handler terminal)
    (let ([scheme (handler)])
      (and scheme
           (begin
             (foreign-set! 'int out 0
               (symbol->c-enum GhosttyColorScheme scheme 'terminal-set-color-scheme-handler!))
             #t)))))

(define-effect device-attributes-callable effects-device-attributes #f (out) (uptr) stdbool
  (lambda (handler terminal)
    (let ([attributes (handler)])
      (and attributes
           (let ([p (make-ftype-pointer GhosttyDeviceAttributes out)])
             (check 'terminal-set-device-attributes-handler! (device-attributes? attributes)
               attributes "device-attributes")
             (ftype-set! GhosttyDeviceAttributes (primary conformance_level) p
               (device-attributes-conformance-level attributes))
             (let loop ([features (device-attributes-features attributes)] [i 0])
               (if (null? features)
                   (ftype-set! GhosttyDeviceAttributes (primary num_features) p i)
                   (begin
                     (ftype-set! GhosttyDeviceAttributes (primary features i) p (car features))
                     (loop (cdr features) (fx+ i 1)))))
             (ftype-set! GhosttyDeviceAttributes (secondary device_type) p
               (device-attributes-device-type attributes))
             (ftype-set! GhosttyDeviceAttributes (secondary firmware_version) p
               (device-attributes-firmware-version attributes))
             (ftype-set! GhosttyDeviceAttributes (secondary rom_cartridge) p
               (device-attributes-rom-cartridge attributes))
             (ftype-set! GhosttyDeviceAttributes (tertiary unit_id) p
               (device-attributes-unit-id attributes))
             #t)))))

(define (clipboard-contents-at address count)
  (let loop ([i (fx- count 1)] [acc '()])
    (if (fx< i 0)
        acc
        (let ([entry (fx+ address (fx* i (ftype-sizeof GhosttyClipboardContent)))])
          (loop (fx- i 1)
            (cons (make-clipboard-content
                    (borrowed-string-at entry)
                    (borrowed-bytes-at (fx+ entry (ftype-sizeof GhosttyString))))
                  acc))))))

(define (strings-at address count)
  (let loop ([i (fx- count 1)] [acc '()])
    (if (fx< i 0)
        acc
        (loop (fx- i 1)
          (cons (borrowed-string-at (fx+ address (fx* i (ftype-sizeof GhosttyString)))) acc)))))

;; Reply functions can write to the pty and so call back into Scheme, which
;; may collect; what they read is immobile.
(define (call-reply-function address request reply)
  ((foreign-procedure address (uptr uptr) void) request (object->reference-address reply))
  (keep-live reply))

(define-effect clipboard-write-callable effects-clipboard-write (void) (request) (uptr) void
  (lambda (handler terminal)
    (let* ([p (make-ftype-pointer GhosttyClipboardWrite request)]
           [value (make-clipboard-write-request
                    (c-enum->symbol GhosttyClipboardLocation
                      (ftype-ref GhosttyClipboardWrite (location) p))
                    (clipboard-contents-at (ftype-ref GhosttyClipboardWrite (contents) p)
                      (ftype-ref GhosttyClipboardWrite (contents_len) p))
                    (borrowed-string-at
                      (ftype-pointer-address (ftype-&ref GhosttyClipboardWrite (name) p)))
                    (ftype-ref GhosttyClipboardWrite (granted) p)
                    (ftype-ref GhosttyClipboardWrite (can_remember) p))])
      (let-values ([(result remember?)
                    (call-with-values (lambda () (handler value))
                      (case-lambda
                        [(result) (values result #f)]
                        [(result remember?) (values result remember?)]))])
        (let ([reply (c-sized-struct GhosttyClipboardWriteReply #t)])
          (c-set! GhosttyClipboardWriteReply (result) reply
            (symbol->c-enum GhosttyClipboardWriteResult result 'terminal-set-clipboard-write-handler!))
          (c-set! GhosttyClipboardWriteReply (remember) reply (and remember? #t))
          (call-reply-function (ftype-ref GhosttyClipboardWrite (reply) p) request reply))))))

(define-effect clipboard-read-callable effects-clipboard-read (void) (request) (uptr) void
  (lambda (handler terminal)
    (define who 'terminal-set-clipboard-read-handler!)
    (let* ([p (make-ftype-pointer GhosttyClipboardRead request)]
           [value (make-clipboard-read-request
                    (c-enum->symbol GhosttyClipboardLocation
                      (ftype-ref GhosttyClipboardRead (location) p))
                    (strings-at (ftype-ref GhosttyClipboardRead (mimes) p)
                      (ftype-ref GhosttyClipboardRead (mimes_len) p))
                    (ftype-ref GhosttyClipboardRead (list) p)
                    (borrowed-string-at
                      (ftype-pointer-address (ftype-&ref GhosttyClipboardRead (name) p)))
                    (ftype-ref GhosttyClipboardRead (granted) p)
                    (ftype-ref GhosttyClipboardRead (can_remember) p))])
      (let-values ([(answer available remember?)
                    (call-with-values (lambda () (handler value))
                      (case-lambda
                        [(answer) (values answer '() #f)]
                        [(answer available) (values answer available #f)]
                        [(answer available remember?) (values answer available remember?)]))])
        (let ([reply (c-sized-struct GhosttyClipboardReadReply #t)])
          (if (symbol? answer)
              (begin
                (when (eq? answer 'success)
                  (assertion-violationf who "answer success with a list of clipboard contents"))
                (c-set! GhosttyClipboardReadReply (result) reply
                  (symbol->c-enum GhosttyClipboardReadResult answer who))
                (call-reply-function (ftype-ref GhosttyClipboardRead (reply) p) request reply))
              (begin
                (check who (and (list? answer) (for-all clipboard-content? answer)) answer
                  "a list of clipboard-content values or a result symbol")
                (check who (and (list? available) (for-all string? available)) available
                  "a list of strings")
                (let-values ([(contents keep) (clipboard-contents->c answer)]
                             [(mimes keep-mimes) (strings->c available)])
                  (c-set! GhosttyClipboardReadReply (result) reply
                    (GhosttyClipboardReadResult success))
                  (c-set! GhosttyClipboardReadReply (contents) reply (immobile-address contents))
                  (c-set! GhosttyClipboardReadReply (contents_len) reply (length answer))
                  (c-set! GhosttyClipboardReadReply (available) reply (immobile-address mimes))
                  (c-set! GhosttyClipboardReadReply (available_len) reply (length available))
                  (c-set! GhosttyClipboardReadReply (remember) reply (and remember? #t))
                  (call-reply-function (ftype-ref GhosttyClipboardRead (reply) p) request reply)
                  (keep-live (list contents keep mimes keep-mimes))))))))))

;; The second value holds what the array points to, which the caller keeps
;; live until C is done with the array.
(define (strings->c strings)
  (let ([array (make-immobile-bytevector (fx* (length strings) (ftype-sizeof GhosttyString)) 0)]
        [bytes (map (lambda (s) (immobile-copy (string->utf8 s))) strings)])
    (let loop ([bytes bytes] [offset 0])
      (unless (null? bytes)
        (bytevector-u64-native-set! array offset (immobile-address (car bytes)))
        (bytevector-u64-native-set! array (fx+ offset 8) (bytevector-length (car bytes)))
        (loop (cdr bytes) (fx+ offset (ftype-sizeof GhosttyString)))))
    (values array bytes)))

(define (clipboard-contents->c contents)
  (let ([size (ftype-sizeof GhosttyClipboardContent)])
    (let ([array (make-immobile-bytevector (fx* (length contents) size) 0)]
          [parts (map (lambda (c)
                        (cons (immobile-copy (string->utf8 (clipboard-content-mime c)))
                              (immobile-copy (clipboard-content-data c))))
                      contents)])
      (let loop ([parts parts] [offset 0])
        (unless (null? parts)
          (let ([mime (caar parts)] [data (cdar parts)])
            (bytevector-u64-native-set! array offset (immobile-address mime))
            (bytevector-u64-native-set! array (fx+ offset 8) (bytevector-length mime))
            (bytevector-u64-native-set! array (fx+ offset 16) (immobile-address data))
            (bytevector-u64-native-set! array (fx+ offset 24) (bytevector-length data)))
          (loop (cdr parts) (fx+ offset size))))
      (values array parts))))

(define-effect desktop-notification-callable effects-desktop-notification (void)
  (notification) (uptr) void
  (lambda (handler terminal)
    (let ([p (make-ftype-pointer GhosttyTerminalDesktopNotification notification)])
      (handler
        (make-desktop-notification
          (borrowed-string-at
            (ftype-pointer-address (ftype-&ref GhosttyTerminalDesktopNotification (title) p)))
          (borrowed-string-at
            (ftype-pointer-address (ftype-&ref GhosttyTerminalDesktopNotification (body) p))))))))

(define-effect progress-report-callable effects-progress-report (void) (report) (uptr) void
  (lambda (handler terminal)
    (let* ([p (make-ftype-pointer GhosttyTerminalProgressReport report)]
           [progress (ftype-ref GhosttyTerminalProgressReport (progress) p)])
      (handler
        (make-progress-report
          (c-enum->symbol GhosttyTerminalProgressState
            (ftype-ref GhosttyTerminalProgressReport (state) p))
          (and (fx>= progress 0) progress))))))

(define-effect unknown-sequence-callable effects-unknown-sequence (void) (sequence) (uptr) void
  (lambda (handler terminal)
    (let* ([p (make-ftype-pointer GhosttyTerminalUnknownSequence sequence)]
           [tag (ftype-ref GhosttyTerminalUnknownSequence (tag) p)])
      (cond
        [(fx= tag (GhosttyTerminalUnknownSequenceTag apc))
         (handler
           (make-unknown-sequence 'apc
             (borrowed-bytes-at
               (ftype-pointer-address
                 (ftype-&ref GhosttyTerminalUnknownSequence (value apc content) p)))
             (ftype-ref GhosttyTerminalUnknownSequence (value apc truncated) p)
             #f))]
        [(fx= tag (GhosttyTerminalUnknownSequenceTag osc))
         (handler
           (make-unknown-sequence 'osc
             (borrowed-bytes-at
               (ftype-pointer-address
                 (ftype-&ref GhosttyTerminalUnknownSequence (value osc content) p)))
             (ftype-ref GhosttyTerminalUnknownSequence (value osc truncated) p)
             (c-enum->symbol GhosttyOscTerminator
               (ftype-ref GhosttyTerminalUnknownSequence (value osc terminator) p))))]
        [else (void)]))))

;; A later library may add kinds that have no name here.
(define-effect semantic-prompt-callable effects-semantic-prompt (void) (event) (uptr) void
  (lambda (handler terminal)
    (let* ([p (make-ftype-pointer GhosttyTerminalSemanticPrompt event)]
           [kind (c-enum->symbol GhosttySemanticPromptKind
                   (ftype-ref GhosttyTerminalSemanticPrompt (kind) p))])
      (when (and kind (not (eq? kind 'invalid)))
        (handler
          (make-semantic-prompt kind
            (c-enum->symbol GhosttySemanticPromptPromptKind
              (ftype-ref GhosttyTerminalSemanticPrompt (prompt_kind) p))
            (and (ftype-ref GhosttyTerminalSemanticPrompt (has_exit_code) p)
                 (ftype-ref GhosttyTerminalSemanticPrompt (exit_code) p))
            (borrowed-string-at
              (ftype-pointer-address (ftype-&ref GhosttyTerminalSemanticPrompt (command) p)))
            (borrowed-string-at
              (ftype-pointer-address (ftype-&ref GhosttyTerminalSemanticPrompt (error) p)))))))))

(define-effect program-status-callable effects-program-status (void) (report) (uptr) void
  (lambda (handler terminal)
    (let* ([p (make-ftype-pointer GhosttyTerminalProgramStatus report)]
           [progress (ftype-ref GhosttyTerminalProgramStatus (progress) p)]
           [kind (c-enum->symbol GhosttyProgramStatusKind
                   (ftype-ref GhosttyTerminalProgramStatus (kind) p))]
           [string-field borrowed-string-at])
      (handler
        (make-program-status
          (c-enum->symbol GhosttyProgramStatusState (ftype-ref GhosttyTerminalProgramStatus (state) p))
          (and (not (eq? kind 'none)) kind)
          (and (fx>= progress 0) progress)
          (string-field (ftype-pointer-address (ftype-&ref GhosttyTerminalProgramStatus (id) p)))
          (string-field (ftype-pointer-address (ftype-&ref GhosttyTerminalProgramStatus (app) p)))
          (string-field (ftype-pointer-address (ftype-&ref GhosttyTerminalProgramStatus (title) p)))
          (string-field
            (ftype-pointer-address (ftype-&ref GhosttyTerminalProgramStatus (message) p))))))))

(define-effect resize-window-callable effects-resize-window (void) (request) (uptr) void
  (lambda (handler terminal)
    (let ([p (make-ftype-pointer GhosttyTerminalResizeWindow request)])
      (when (fx= (ftype-ref GhosttyTerminalResizeWindow (tag) p)
                 (GhosttyTerminalResizeWindowTag grid))
        (handler (ftype-ref GhosttyTerminalResizeWindow (value grid rows) p)
                 (ftype-ref GhosttyTerminalResizeWindow (value grid columns) p))))))

(define-effect reset-callable effects-reset (void) () () void
  (lambda (handler terminal) (handler)))

(define-effect render-hold-callable effects-render-hold (void) (held?) (stdbool) void
  (lambda (handler terminal) (handler held?)))

;; Handlers may be replaced inside a callback: the native side only swaps
;; a function pointer.
(define (set-handler! who t option set-field! callable handler)
  (check-procedure-or-false who handler)
  (with-terminal who t (pointer)
    (set-field! (terminal-effects t) handler)
    (check-result who (ghostty-terminal-set pointer option (if handler callable 0)))))

(define-syntax define-handler-setter
  (syntax-rules ()
    [(_ name option set-field! callable)
     (define (name t handler)
       (set-handler! 'name t (GhosttyTerminalOption option) set-field! callable handler))]))

(define-handler-setter terminal-set-pty-write-handler! write-pty effects-pty-write-set!
  pty-write-callable)
(define-handler-setter terminal-set-bell-handler! bell effects-bell-set! bell-callable)
(define-handler-setter terminal-set-enquiry-handler! enquiry effects-enquiry-set! enquiry-callable)
(define-handler-setter terminal-set-xtversion-handler! xtversion effects-xtversion-set!
  xtversion-callable)
(define-handler-setter terminal-set-title-changed-handler! title-changed
  effects-title-changed-set! title-changed-callable)
(define-handler-setter terminal-set-pwd-changed-handler! pwd-changed effects-pwd-changed-set!
  pwd-changed-callable)
(define-handler-setter terminal-set-size-handler! size effects-size-set! size-callable)
(define-handler-setter terminal-set-color-scheme-handler! color-scheme effects-color-scheme-set!
  color-scheme-callable)
(define-handler-setter terminal-set-device-attributes-handler! device-attributes
  effects-device-attributes-set! device-attributes-callable)
(define-handler-setter terminal-set-clipboard-write-handler! clipboard-write
  effects-clipboard-write-set! clipboard-write-callable)
(define-handler-setter terminal-set-clipboard-read-handler! clipboard-read
  effects-clipboard-read-set! clipboard-read-callable)
(define-handler-setter terminal-set-desktop-notification-handler! desktop-notification
  effects-desktop-notification-set! desktop-notification-callable)
(define-handler-setter terminal-set-progress-report-handler! progress-report
  effects-progress-report-set! progress-report-callable)
(define-handler-setter terminal-set-unknown-sequence-handler! unknown-sequence
  effects-unknown-sequence-set! unknown-sequence-callable)
(define-handler-setter terminal-set-semantic-prompt-handler! semantic-prompt
  effects-semantic-prompt-set! semantic-prompt-callable)
(define-handler-setter terminal-set-program-status-handler! program-status
  effects-program-status-set! program-status-callable)
(define-handler-setter terminal-set-resize-window-handler! resize-window
  effects-resize-window-set! resize-window-callable)
(define-handler-setter terminal-set-reset-handler! reset effects-reset-set! reset-callable)
(define-handler-setter terminal-set-render-hold-handler! render-hold effects-render-hold-set!
  render-hold-callable)

(define pastes-mutex (make-mutex))
(define pastes (make-eqv-hashtable))
(define next-paste-id 1)

(define mime-reader-callable
  (make-callable
    (foreign-callable
      (lambda (userdata mime writer)
        ;; Nothing here should raise, but a raise must not unwind through C.
        (guard (c [#t #f])
          (let ([contents (with-mutex pastes-mutex (hashtable-ref pastes userdata #f))]
                [wanted (address->bytevector (ftype-ref GhosttyString (ptr) mime)
                          (ftype-ref GhosttyString (len) mime))])
            (cond
              [(and contents (find (lambda (entry) (bytevector=? (car entry) wanted)) contents))
               => (lambda (entry)
                    (let ([data (cdr entry)])
                      ((foreign-procedure (ftype-ref GhosttyWriter (write) writer)
                         (uptr uptr size_t) stdbool)
                       (ftype-ref GhosttyWriter (userdata) writer)
                       (immobile-address data)
                       (bytevector-length data))))]
              [else #f]))))
      (uptr (& GhosttyString) (& GhosttyWriter))
      stdbool)))

(define (paste-contents who contents)
  (cond
    [(string? contents) (list (make-clipboard-content "text/plain" (string->utf8 contents)))]
    [(bytevector? contents) (list (make-clipboard-content "text/plain" contents))]
    [(and (list? contents) (for-all clipboard-content? contents)) contents]
    [else (assertion-violationf who "~s is not a string, bytevector or list of clipboard contents"
            contents)]))

(define (paste! who t contents location source allow-unsafe?)
  (let ([location (symbol->c-enum GhosttyClipboardLocation location who)]
        [source (symbol->c-enum GhosttyPasteSource source who)])
    (let-values ([(mimes mime-bytes) (strings->c (map clipboard-content-mime contents))])
      (let* ([data (map (lambda (c) (immobile-copy (clipboard-content-data c))) contents)]
             [paste (c-sized-struct GhosttyPaste #t)]
             [written (make-immobile-bytevector 1 0)]
             [id (with-mutex pastes-mutex
                   (let ([id next-paste-id])
                     (set! next-paste-id (+ id 1))
                     (hashtable-set! pastes id (map cons mime-bytes data))
                     id))])
        (c-set! GhosttyPaste (location) paste location)
        (c-set! GhosttyPaste (source) paste source)
        (c-set! GhosttyPaste (mimes) paste (immobile-address mimes))
        (c-set! GhosttyPaste (mimes_len) paste (length contents))
        (c-set! GhosttyPaste (reader read) paste mime-reader-callable)
        (c-set! GhosttyPaste (reader userdata) paste id)
        (c-set! GhosttyPaste (allow_unsafe) paste allow-unsafe?)
        (let ([code (dynamic-wind
                      void
                      (lambda ()
                        (call-with-terminal-effects who t
                          (lambda (pointer)
                            (ghostty-terminal-paste pointer (object->reference-address paste)
                              (object->reference-address written)))))
                      (lambda () (with-mutex pastes-mutex (hashtable-delete! pastes id))))])
          (keep-live (list paste mimes mime-bytes data written))
          (cond
            [(fx= code (GhosttyResult success)) (if (bool-ref written) 'written 'empty)]
            [(fx= code (GhosttyResult rejected)) 'rejected]
            [else (raise-result who code)]))))))

(define terminal-paste!
  (case-lambda
    [(t contents) (terminal-paste! t contents 'standard 'clipboard)]
    [(t contents location) (terminal-paste! t contents location 'clipboard)]
    [(t contents location source)
     (paste! 'terminal-paste! t (paste-contents 'terminal-paste! contents) location source #f)]))

(define terminal-paste-unsafe!
  (case-lambda
    [(t contents) (terminal-paste-unsafe! t contents 'standard 'clipboard)]
    [(t contents location) (terminal-paste-unsafe! t contents location 'clipboard)]
    [(t contents location source)
     (paste! 'terminal-paste-unsafe! t (paste-contents 'terminal-paste-unsafe! contents) location
       source #t)]))
