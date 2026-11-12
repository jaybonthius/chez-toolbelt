(define-record-type model
  (parent handle) (nongenerative) (sealed #t)
  (fields kind)
  (protocol handle-protocol))

(define (kind-symbol code)
  (cond
    [(= code model-kind-asr) 'asr]
    [(= code model-kind-diarization) 'diarization]
    [(= code model-kind-sound) 'sound]
    [(= code model-kind-speaker) 'speaker]
    [(= code model-kind-vad) 'vad]
    [else 'none]))

(define (check-model who m)
  (unless (model? m) (assertion-violationf who "~s is not a model" m)))

(define (check-model-or-false who m)
  (unless (or (not m) (model? m)) (assertion-violationf who "~s is neither a model nor #f" m)))

;; parakeet.cpp runs concurrent requests on one speech or Silero model, and
;; forbids them on the other kinds.
(define (model-exclusive? m) (not (memq (model-kind m) '(asr vad))))

(define (model-address-or-0 m) (if m (handle-address m) 0))

(define load-model
  (case-lambda
    [(path) (load-model path #f)]
    [(path component)
     (check-path 'load-model path)
     (let ([c-path (string->c 'load-model path)] [c-component (string->c 'load-model component)])
       (let ([address (with-locked (c-path c-component)
                        (if c-component
                            (capi-load-component c-path c-component)
                            (capi-load c-path)))])
         (when (eqv? address 0) (raise-parakeet-error 'load-model (load-error)))
         (register-handle!
           (make-model address '() capi-free (kind-symbol (capi-model-kind address))))))]))

(define (close-model m)
  (check-model 'close-model m)
  (close-handle! m))

(define (model-open? m)
  (check-model 'model-open? m)
  (handle-open? m))

(define (bundle-components-json path)
  (check-path 'bundle-components-json path)
  (let ([c-path (string->c 'bundle-components-json path)])
    (or (take-string (with-locked (c-path) (capi-bundle-components-json c-path)))
        (raise-parakeet-error 'bundle-components-json (load-error)))))

(define (abi-version) (capi-abi-version))

(define model-set-concurrency!
  (case-lambda
    [(m backends) (model-set-concurrency! m backends 0)]
    [(m backends threads-each)
     (define who 'model-set-concurrency!)
     (check-model who m)
     (check-int who "backend count" backends 1)
     (check-int who "thread count" threads-each 0)
     (with-handles who ([m #t])
       (let ([n (capi-set-concurrency (handle-address m) backends threads-each)])
         (when (= n 0) (raise-parakeet-error who (context-error (handle-address m))))
         n))]))

(define-syntax with-model-call
  (syntax-rules ()
    [(_ who m address e1 e2 ...)
     (with-handles who ([m (model-exclusive? m)])
       (let ([address (handle-address m)]) e1 e2 ...))]))

(define (string-result who result error-address)
  (or (take-string result) (raise-parakeet-error who (context-error error-address))))

(define (check-transcribe who m options uses)
  (check-model who m)
  (check-decode-options who options uses))

(define transcribe-file
  (case-lambda
    [(m path) (transcribe-file m path default-decode-options)]
    [(m path options)
     (define who 'transcribe-file)
     (check-transcribe who m options '(decoder language))
     (check-path who path)
     (let ([c-path (string->c who path)]
           [c-language (string->c who (decode-options-record-language options))])
       (with-model-call who m address
         (string-result who
           (with-locked (c-path c-language)
             (capi-transcribe-path-lang address c-path
               (decoder-code (decode-options-record-decoder options)) c-language))
           address)))]))

(define transcribe-pcm
  (case-lambda
    [(m samples) (transcribe-pcm m samples 16000 default-decode-options)]
    [(m samples rate) (transcribe-pcm m samples rate default-decode-options)]
    [(m samples rate options)
     (define who 'transcribe-pcm)
     (check-transcribe who m options '(decoder language))
     (check-samples who samples)
     (check-sample-rate who rate)
     (let ([c-language (string->c who (decode-options-record-language options))])
       (with-model-call who m address
         (string-result who
           (with-locked (samples c-language)
             (capi-transcribe-pcm-lang address samples (sample-count samples) rate
               (decoder-code (decode-options-record-decoder options)) c-language))
           address)))]))

(define transcribe-file-json
  (case-lambda
    [(m path) (transcribe-file-json m path default-decode-options #f)]
    [(m path options) (transcribe-file-json m path options #f)]
    [(m path options filter)
     (define who 'transcribe-file-json)
     (check-transcribe who m options '(decoder))
     (check-path who path)
     (when filter (check-word-filter who filter))
     (let ([c-path (string->c who path)]
           [c-filter (string->c who (and filter (json-object (word-filter-record-entries filter))))]
           [code (decoder-code (decode-options-record-decoder options))])
       (with-model-call who m address
         (string-result who
           (with-locked (c-path c-filter)
             (if c-filter
                 (capi-transcribe-path-json-with address c-path code c-filter)
                 (capi-transcribe-path-json address c-path code)))
           address)))]))

(define transcribe-file-vad-json
  (case-lambda
    [(m path) (transcribe-file-vad-json m path #f default-vad-options default-decode-options #f)]
    [(m path vad) (transcribe-file-vad-json m path vad default-vad-options default-decode-options #f)]
    [(m path vad segmenter)
     (transcribe-file-vad-json m path vad segmenter default-decode-options #f)]
    [(m path vad segmenter options)
     (transcribe-file-vad-json m path vad segmenter options #f)]
    [(m path vad segmenter options filter)
     (define who 'transcribe-file-vad-json)
     (check-transcribe who m options '(decoder))
     (check-path who path)
     (check-model-or-false who vad)
     (check-vad-options who segmenter)
     (when filter (check-word-filter who filter))
     (let ([c-path (string->c who path)]
           [c-options (string->c who
                        (json-object
                          (append (vad-options-record-entries segmenter)
                                  (if filter (word-filter-record-entries filter) '()))))]
           [code (decoder-code (decode-options-record-decoder options))])
       (call-with-handles who
         (cons (cons m (model-exclusive? m)) (if vad (list (cons vad (model-exclusive? vad))) '()))
         (lambda ()
           (let ([address (handle-address m)])
             (string-result who
               (with-locked (c-path c-options)
                 (capi-transcribe-path-json-vad-with address (model-address-or-0 vad) c-path code
                   c-options))
               address)))))]))

;; One native copy serves both batch calls: one takes the clips concatenated,
;; the other an array of pointers, which bytevectors could fill only by
;; locking each clip and taking its address.
(define (call-with-clips who clips proc)
  (unless (and (vector? clips)
               (for-all (lambda (clip)
                          (check-samples who clip)
                          #t)
                        (vector->list clips)))
    (assertion-violationf who "~s is not a vector of sample bytevectors" clips))
  (let* ([n (vector-length clips)]
         [total (fold-left (lambda (sum clip) (+ sum (bytevector-length clip))) 0
                  (vector->list clips))]
         [counts (make-bytevector (fx* 4 (fxmax n 1)) 0)]
         [pointers (make-bytevector (fx* 8 (fxmax n 1)) 0)]
         [block (foreign-alloc (max total 1))])
    (dynamic-wind
      void
      (lambda ()
        (let loop ([i 0] [offset 0])
          (when (fx< i n)
            (let ([clip (vector-ref clips i)])
              (c-memcpy-to (+ block offset) clip (bytevector-length clip))
              (bytevector-s32-native-set! counts (fx* 4 i) (sample-count clip))
              (bytevector-u64-native-set! pointers (fx* 8 i) (+ block offset))
              (loop (fx+ i 1) (+ offset (bytevector-length clip))))))
        (with-locked (counts pointers) (proc n block counts pointers)))
      (lambda () (foreign-free block)))))

(define transcribe-pcm-batch
  (case-lambda
    [(m clips) (transcribe-pcm-batch m clips 16000 default-decode-options)]
    [(m clips rate) (transcribe-pcm-batch m clips rate default-decode-options)]
    [(m clips rate options)
     (define who 'transcribe-pcm-batch)
     (check-transcribe who m options '(decoder language))
     (check-sample-rate who rate)
     (let ([c-language (string->c who (decode-options-record-language options))])
       (call-with-clips who clips
         (lambda (n block counts pointers)
           (with-model-call who m address
             (if (fx= n 0)
                 (vector)
                 (let ([out (foreign-alloc (fx* 8 n))])
                   (do ([i 0 (fx+ i 1)]) ((fx= i n)) (foreign-set! 'uptr out (fx* 8 i) 0))
                   (dynamic-wind
                     void
                     (lambda ()
                       (let ([status (with-locked (c-language)
                                       (capi-transcribe-pcm-batch-lang address
                                         pointers counts n rate
                                         (decoder-code (decode-options-record-decoder options))
                                         c-language out))])
                         (unless (= status 0) (raise-parakeet-error who (context-error address)))
                         (let ([texts (make-vector n "")])
                           (do ([i 0 (fx+ i 1)]) ((fx= i n))
                             (let ([p (foreign-ref 'uptr out (fx* 8 i))])
                               (foreign-set! 'uptr out (fx* 8 i) 0)
                               (vector-set! texts i (take-string p))))
                           texts)))
                     (lambda ()
                       (do ([i 0 (fx+ i 1)]) ((fx= i n))
                         (capi-free-string (foreign-ref 'uptr out (fx* 8 i))))
                       (foreign-free out)))))))))]))

(define transcribe-pcm-batch-json
  (case-lambda
    [(m clips) (transcribe-pcm-batch-json m clips 16000 default-decode-options)]
    [(m clips rate) (transcribe-pcm-batch-json m clips rate default-decode-options)]
    [(m clips rate options)
     (define who 'transcribe-pcm-batch-json)
     (check-transcribe who m options '(decoder language))
     (check-sample-rate who rate)
     (let ([c-language (string->c who (decode-options-record-language options))])
       (call-with-clips who clips
         (lambda (n block counts pointers)
           (with-model-call who m address
             (if (fx= n 0)
                 "[]"
                 (string-result who
                   (with-locked (c-language)
                     (capi-transcribe-pcm-batch-json-lang address block counts n rate
                       (decoder-code (decode-options-record-decoder options)) c-language))
                   address))))))]))

(define (check-nbest who count options)
  (check-int who "hypothesis count" count 1)
  (unless (<= count (decode-options-record-beam-size options))
    (assertion-violationf who "~s hypotheses do not fit a beam of ~s" count
      (decode-options-record-beam-size options))))

(define transcribe-file-nbest-json
  (case-lambda
    [(m path count) (transcribe-file-nbest-json m path count default-decode-options)]
    [(m path count options)
     (define who 'transcribe-file-nbest-json)
     (check-transcribe who m options '(beam-size normalized-ranking language))
     (check-path who path)
     (check-nbest who count options)
     (let ([c-path (string->c who path)]
           [c-language (string->c who (decode-options-record-language options))])
       (with-model-call who m address
         (string-result who
           (with-locked (c-path c-language)
             (capi-transcribe-path-nbest-json address c-path
               (decode-options-record-beam-size options) count
               (if (decode-options-record-normalized-ranking options) 1 0) c-language))
           address)))]))

(define transcribe-pcm-nbest-json
  (case-lambda
    [(m samples count) (transcribe-pcm-nbest-json m samples count 16000 default-decode-options)]
    [(m samples count rate) (transcribe-pcm-nbest-json m samples count rate default-decode-options)]
    [(m samples count rate options)
     (define who 'transcribe-pcm-nbest-json)
     (check-transcribe who m options '(beam-size normalized-ranking language))
     (check-samples who samples)
     (check-sample-rate who rate)
     (check-nbest who count options)
     (let ([c-language (string->c who (decode-options-record-language options))])
       (with-model-call who m address
         (string-result who
           (with-locked (samples c-language)
             (capi-transcribe-pcm-nbest-json address samples (sample-count samples) rate
               (decode-options-record-beam-size options) count
               (if (decode-options-record-normalized-ranking options) 1 0) c-language))
           address)))]))

(define-record-type ctc-logits
  (nongenerative) (sealed #t)
  (fields frames vocabulary-size values))

(define transcribe-pcm-logits
  (case-lambda
    [(m samples) (transcribe-pcm-logits m samples 16000)]
    [(m samples rate)
     (define who 'transcribe-pcm-logits)
     (check-model who m)
     (check-samples who samples)
     (check-sample-rate who rate)
     (let ([out-logits (make-out-pointers 1)] [out-frames (make-out-pointers 1)]
           [out-vocabulary (make-out-pointers 1)])
       (with-model-call who m address
         (let ([status (with-locked (samples out-logits out-frames out-vocabulary)
                         (capi-transcribe-pcm-logits address samples (sample-count samples) rate
                           out-logits out-frames out-vocabulary))])
           (unless (= status 0) (raise-parakeet-error who (context-error address)))
           (let ([p (out-pointer-ref out-logits 0)]
                 [frames (out-int-ref out-frames 0)]
                 [vocabulary (out-int-ref out-vocabulary 0)])
             (dynamic-wind
               void
               (lambda ()
                 (let ([matrix (make-bytevector (* 4 frames vocabulary))])
                   (c-memcpy-from matrix p (bytevector-length matrix))
                   (make-ctc-logits frames vocabulary matrix)))
               (lambda () (capi-free-logits p)))))))]))

(define vad-pcm-json
  (case-lambda
    [(m samples) (vad-pcm-json m samples 16000 default-vad-options)]
    [(m samples rate) (vad-pcm-json m samples rate default-vad-options)]
    [(m samples rate options)
     (define who 'vad-pcm-json)
     (check-model who m)
     (check-samples who samples)
     (check-sample-rate who rate)
     (check-vad-options who options)
     (let ([c-options (string->c who (json-object (vad-options-record-entries options)))])
       (with-model-call who m address
         (string-result who
           (with-locked (samples c-options)
             (capi-vad-pcm-json address samples (sample-count samples) rate c-options))
           address)))]))

(define vad-file-json
  (case-lambda
    [(m path) (vad-file-json m path default-vad-options)]
    [(m path options)
     (define who 'vad-file-json)
     (check-model who m)
     (check-path who path)
     (check-vad-options who options)
     (let ([c-path (string->c who path)]
           [c-options (string->c who (json-object (vad-options-record-entries options)))])
       (with-model-call who m address
         (string-result who
           (with-locked (c-path c-options) (capi-vad-path-json address c-path c-options))
           address)))]))
