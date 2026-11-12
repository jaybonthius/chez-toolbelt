(define-record-type grammar
  (parent handle) (nongenerative) (sealed #t)
  (protocol handle-protocol))

(define (grammar-model g) (car (handle-parents g)))

(define (check-grammar who g m)
  (unless (grammar? g) (assertion-violationf who "~s is not a grammar" g))
  (unless (eq? (grammar-model g) m)
    (assertion-violationf who "~s was compiled for another model than ~s" g m)))

(define (compile-grammar m rules)
  (define who 'compile-grammar)
  (check-model who m)
  (let ([c-rules (string->c who rules)])
    (unless c-rules (assertion-violationf who "~s is not a string" rules))
    (with-model-call who m address
      (let ([g (with-locked (c-rules) (capi-grammar-create address c-rules))])
        (when (eqv? g 0) (raise-parakeet-error who (context-error address)))
        (register-handle! (make-grammar g (list m) capi-grammar-free))))))

(define (close-grammar g)
  (unless (grammar? g) (assertion-violationf 'close-grammar "~s is not a grammar" g))
  (close-handle! g))

(define (grammar-result who status out address)
  (cond
    [(= status grammar-ok) (take-string (out-pointer-ref out 0))]
    [(= status grammar-no-match) #f]
    [else (raise-parakeet-error who (context-error address))]))

(define (grammar-pcm who call m g samples rate options)
  (check-transcribe who m options '(decoder beam-size language))
  (check-grammar who g m)
  (check-samples who samples)
  (check-sample-rate who rate)
  (let ([c-language (string->c who (decode-options-record-language options))]
        [out (make-out-pointers 1)])
    (with-handles who ([m (model-exclusive? m)] [g #f])
      (let ([address (handle-address m)])
        (grammar-result who
          (with-locked (samples c-language out)
            (call address (handle-address g) samples (sample-count samples) rate
              (decoder-code (decode-options-record-decoder options))
              (decode-options-record-beam-size options) c-language out))
          out address)))))

(define (grammar-file who call m g path options)
  (check-transcribe who m options '(decoder beam-size language))
  (check-grammar who g m)
  (check-path who path)
  (let ([c-path (string->c who path)]
        [c-language (string->c who (decode-options-record-language options))]
        [out (make-out-pointers 1)])
    (with-handles who ([m (model-exclusive? m)] [g #f])
      (let ([address (handle-address m)])
        (grammar-result who
          (with-locked (c-path c-language out)
            (call address (handle-address g) c-path
              (decoder-code (decode-options-record-decoder options))
              (decode-options-record-beam-size options) c-language out))
          out address)))))

(define transcribe-pcm-grammar
  (case-lambda
    [(m g samples) (transcribe-pcm-grammar m g samples 16000 default-decode-options)]
    [(m g samples rate) (transcribe-pcm-grammar m g samples rate default-decode-options)]
    [(m g samples rate options)
     (grammar-pcm 'transcribe-pcm-grammar capi-transcribe-pcm-grammar m g samples rate options)]))

(define transcribe-pcm-grammar-json
  (case-lambda
    [(m g samples) (transcribe-pcm-grammar-json m g samples 16000 default-decode-options)]
    [(m g samples rate) (transcribe-pcm-grammar-json m g samples rate default-decode-options)]
    [(m g samples rate options)
     (grammar-pcm 'transcribe-pcm-grammar-json capi-transcribe-pcm-grammar-json
       m g samples rate options)]))

(define transcribe-file-grammar
  (case-lambda
    [(m g path) (transcribe-file-grammar m g path default-decode-options)]
    [(m g path options)
     (grammar-file 'transcribe-file-grammar capi-transcribe-path-grammar m g path options)]))

(define transcribe-file-grammar-json
  (case-lambda
    [(m g path) (transcribe-file-grammar-json m g path default-decode-options)]
    [(m g path options)
     (grammar-file 'transcribe-file-grammar-json capi-transcribe-path-grammar-json
       m g path options)]))

;; The stream keeps the compiled grammar itself, so the grammar
;; may be closed once the stream is open.

(define-record-type grammar-stream
  (parent handle) (nongenerative) (sealed #t)
  (protocol handle-protocol))

(define (stream-model s) (car (handle-parents s)))

(define open-grammar-stream
  (case-lambda
    [(m g) (open-grammar-stream m g default-decode-options)]
    [(m g options)
     (define who 'open-grammar-stream)
     (check-transcribe who m options '(beam-size language))
     (check-grammar who g m)
     (let ([c-language (string->c who (decode-options-record-language options))])
       (with-handles who ([m (model-exclusive? m)] [g #f])
         (let* ([address (handle-address m)]
                [s (with-locked (c-language)
                     (capi-grammar-stream-begin address (handle-address g)
                       (decode-options-record-beam-size options) c-language))])
           (when (eqv? s 0) (raise-parakeet-error who (context-error address)))
           (register-handle! (make-grammar-stream s (list m) capi-grammar-stream-free)))))]))

(define (check-grammar-stream who s)
  (unless (grammar-stream? s) (assertion-violationf who "~s is not a grammar stream" s)))

(define (close-grammar-stream s)
  (check-grammar-stream 'close-grammar-stream s)
  (close-handle! s))

(define (grammar-stream-output who s call)
  (let ([m (stream-model s)] [out (make-out-pointers 1)])
    (with-handles who ([s #t] [m (model-exclusive? m)])
      (let ([status (with-locked (out) (call (handle-address s) out))])
        (if (or (= status grammar-ok) (= status grammar-no-match))
            (values (take-string (out-pointer-ref out 0)) (= status grammar-ok))
            (raise-parakeet-error who (context-error (handle-address m))))))))

(define (grammar-stream-feed-json s samples)
  (define who 'grammar-stream-feed-json)
  (check-grammar-stream who s)
  (check-samples who samples)
  (with-locked (samples)
    (grammar-stream-output who s
      (lambda (address out)
        (capi-grammar-stream-feed-json address samples (sample-count samples) out)))))

(define (grammar-stream-finish-json s)
  (define who 'grammar-stream-finish-json)
  (check-grammar-stream who s)
  (grammar-stream-output who s capi-grammar-stream-finalize-json))

(define (grammar-stream-reset! s)
  (define who 'grammar-stream-reset!)
  (check-grammar-stream who s)
  (let ([m (stream-model s)])
    (with-handles who ([s #t] [m (model-exclusive? m)])
      (unless (= (capi-grammar-stream-reset (handle-address s)) grammar-ok)
        (raise-parakeet-error who (context-error (handle-address m)))))))
