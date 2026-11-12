(define (model-stream-kind m)
  (define who 'model-stream-kind)
  (check-model who m)
  (with-handles who ([m #f])
    (let ([kind (capi-stream-kind (handle-address m))])
      (cond
        [(= kind stream-kind-cache-aware) 'cache-aware]
        [(= kind stream-kind-buffered) 'buffered]
        [else 'none]))))

(define (model-stream-eou-events? m)
  (define who 'model-stream-eou-events?)
  (check-model who m)
  (with-handles who ([m #f])
    (not (= 0 (fxand (capi-stream-caps (handle-address m)) stream-cap-eou-events)))))

(define (model-stream-presets-json m)
  (define who 'model-stream-presets-json)
  (check-model who m)
  (with-model-call who m address
    (string-result who (capi-stream-presets-json address) address)))

;; parakeet.cpp does not stop a stream from being fed after its end, which
;; its decoder does not support, so the binding does.
(define-record-type asr-stream
  (parent handle) (nongenerative) (sealed #t)
  (fields (mutable finished?))
  (protocol handle-protocol))

(define (check-asr-stream who s)
  (unless (asr-stream? s) (assertion-violationf who "~s is not a speech stream" s)))

(define-record-type stream-event
  (nongenerative) (sealed #t)
  (fields kind token frame time))

(define (write-stream-options options language-address opts)
  (capi-stream-opts-default opts)
  (foreign-set! 'uptr opts stream-opts-target-lang-offset language-address)
  (cond
    [(given? (asr-stream-options-record-left-frames options))
     (foreign-set! 'int opts stream-opts-preset-offset stream-preset-custom)
     (foreign-set! 'int opts stream-opts-left-frames-offset (asr-stream-options-record-left-frames options))
     (foreign-set! 'int opts stream-opts-chunk-frames-offset
       (asr-stream-options-record-chunk-frames options))
     (foreign-set! 'int opts stream-opts-right-frames-offset
       (asr-stream-options-record-right-frames options))]
    [(given? (asr-stream-options-record-preset options))
     (foreign-set! 'int opts stream-opts-preset-offset (asr-stream-options-record-preset options))]))

(define open-asr-stream
  (case-lambda
    [(m) (open-asr-stream m default-asr-stream-options)]
    [(m options)
     (define who 'open-asr-stream)
     (check-model who m)
     (unless (asr-stream-options-record? options)
       (assertion-violationf who "~s is not an asr-stream-options value" options))
     (let ([c-language (string->c who (asr-stream-options-record-language options))]
           [opts (foreign-alloc stream-opts-size)])
       (dynamic-wind
         void
         (lambda ()
           (with-locked (c-language)
             (write-stream-options options
               (if c-language (object->reference-address c-language) 0) opts)
             (with-model-call who m address
               (let ([s (capi-stream-begin-opts address opts)])
                 (when (eqv? s 0) (raise-parakeet-error who (context-error address)))
                 (register-handle! (make-asr-stream s (list m) capi-stream-free #f))))))
         (lambda () (foreign-free opts))))]))

(define (close-asr-stream s)
  (check-asr-stream 'close-asr-stream s)
  (close-handle! s))

(define-syntax with-asr-stream
  (syntax-rules ()
    [(_ who s active? (address error-address) e1 e2 ...)
     (begin
       (check-asr-stream who s)
       (let ([m (stream-model s)])
         (with-handles who ([s #t] [m (model-exclusive? m)])
           (when (and active? (asr-stream-finished? s))
             (assertion-violationf who "~s is finished; reset it to feed it again" s))
           (let ([address (handle-address s)] [error-address (handle-address m)])
             e1 e2 ...))))]))

(define (event-kinds mask)
  (append (if (= 0 (fxand mask event-eou)) '() '(eou))
          (if (= 0 (fxand mask event-eob)) '() '(eob))))

(define (asr-stream-feed s samples)
  (define who 'asr-stream-feed)
  (check-samples who samples)
  (let ([mask (make-out-pointers 1)])
    (with-asr-stream who s #t (address error-address)
      (let ([text (string-result who
                    (with-locked (samples mask)
                      (capi-stream-feed address samples (sample-count samples) mask))
                    error-address)])
        (values text (event-kinds (out-int-ref mask 0)))))))

(define (asr-stream-feed-json s samples)
  (define who 'asr-stream-feed-json)
  (check-samples who samples)
  (with-asr-stream who s #t (address error-address)
    (string-result who
      (with-locked (samples) (capi-stream-feed-json address samples (sample-count samples)))
      error-address)))

(define (asr-stream-finish s)
  (define who 'asr-stream-finish)
  (with-asr-stream who s #t (address error-address)
    (asr-stream-finished?-set! s #t)
    (string-result who (capi-stream-finalize address) error-address)))

(define (asr-stream-finish-json s)
  (define who 'asr-stream-finish-json)
  (with-asr-stream who s #t (address error-address)
    (asr-stream-finished?-set! s #t)
    (string-result who (capi-stream-finalize-json address) error-address)))

(define (asr-stream-drain-events s)
  (define who 'asr-stream-drain-events)
  (let ([out (make-out-pointers 1)])
    (with-asr-stream who s #f (address error-address)
      (let ([n (with-locked (out) (capi-stream-drain-events address out))])
        (when (< n 0) (raise-parakeet-error who (context-error error-address)))
        (let ([p (out-pointer-ref out 0)])
          (dynamic-wind
            void
            (lambda ()
              (let loop ([i (fx- n 1)] [events '()])
                (if (fx< i 0)
                    events
                    (let ([base (+ p (fx* i stream-event-size))])
                      (loop (fx- i 1)
                        (cons (make-stream-event
                                (if (= 0 (foreign-ref 'int base stream-event-is-eob-offset)) 'eou 'eob)
                                (foreign-ref 'int base stream-event-token-offset)
                                (foreign-ref 'int base stream-event-encoder-frame-offset)
                                (foreign-ref 'float base stream-event-time-sec-offset))
                              events))))))
            (lambda () (capi-free-events p))))))))

(define (asr-stream-reset! s)
  (define who 'asr-stream-reset!)
  (with-asr-stream who s #f (address error-address)
    (unless (= 0 (capi-stream-reset address))
      (raise-parakeet-error who (context-error error-address)))
    (asr-stream-finished?-set! s #f)))

(define (asr-stream-info-json s)
  (define who 'asr-stream-info-json)
  (with-asr-stream who s #f (address error-address)
    (string-result who (capi-stream-info-json address) error-address)))

(define-record-type vad-stream
  (parent handle) (nongenerative) (sealed #t)
  (protocol handle-protocol))

(define (check-vad-stream who s)
  (unless (vad-stream? s) (assertion-violationf who "~s is not a VAD stream" s)))

(define open-vad-stream
  (case-lambda
    [(m) (open-vad-stream m 16000 default-vad-options)]
    [(m rate) (open-vad-stream m rate default-vad-options)]
    [(m rate options)
     (define who 'open-vad-stream)
     (check-model who m)
     (check-sample-rate who rate)
     (check-vad-options who options)
     (let ([c-options (string->c who (json-object (vad-options-record-entries options)))])
       (with-model-call who m address
         (let ([s (with-locked (c-options) (capi-vad-stream-begin address rate c-options))])
           (when (eqv? s 0) (raise-parakeet-error who (context-error address)))
           (register-handle! (make-vad-stream s (list m) capi-vad-stream-free)))))]))

(define (close-vad-stream s)
  (check-vad-stream 'close-vad-stream s)
  (close-handle! s))

(define (vad-stream-call who s samples last?)
  (check-vad-stream who s)
  (check-samples who samples)
  (let ([m (stream-model s)])
    (with-handles who ([s #t] [m (model-exclusive? m)])
      (string-result who
        (with-locked (samples)
          (capi-vad-stream-feed-json (handle-address s) samples (sample-count samples)
            (if last? 1 0)))
        (handle-address m)))))

(define (vad-stream-feed-json s samples)
  (vad-stream-call 'vad-stream-feed-json s samples #f))

(define vad-stream-finish-json
  (case-lambda
    [(s) (vad-stream-finish-json s (bytevector))]
    [(s samples) (vad-stream-call 'vad-stream-finish-json s samples #t)]))

(define (vad-stream-reset! s)
  (define who 'vad-stream-reset!)
  (check-vad-stream who s)
  (let ([m (stream-model s)])
    (with-handles who ([s #t] [m (model-exclusive? m)])
      (unless (= 0 (capi-vad-stream-reset (handle-address s)))
        (raise-parakeet-error who (context-error (handle-address m)))))))
