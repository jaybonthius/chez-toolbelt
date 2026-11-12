;; Some calls report a failure on whichever of their models failed, without
;; clearing the others, so every message is reported, named by model.
(define (models-error labelled)
  (let ([messages
         (filter (lambda (entry) (not (string=? (cdr entry) "")))
           (map (lambda (entry)
                  (cons (car entry)
                    (if (cdr entry) (context-error (handle-address (cdr entry))) "")))
                labelled))])
    (cond
      [(null? messages) ""]
      [(null? (cdr messages)) (cdar messages)]
      [else
       (call-with-string-output-port
         (lambda (p)
           (let loop ([messages messages] [first? #t])
             (unless (null? messages)
               (unless first? (put-string p "; "))
               (fprintf p "~a model: ~a" (caar messages) (cdar messages))
               (loop (cdr messages) #f)))))])))

(define (model-uses . models)
  (map (lambda (m) (cons m (model-exclusive? m))) (filter values models)))

(define (diarize-file-json m path)
  (define who 'diarize-file-json)
  (check-model who m)
  (check-path who path)
  (let ([c-path (string->c who path)])
    (with-model-call who m address
      (string-result who (with-locked (c-path) (capi-diarize-path address c-path)) address))))

(define diarize-pcm-json
  (case-lambda
    [(m samples) (diarize-pcm-json m samples 16000)]
    [(m samples rate)
     (define who 'diarize-pcm-json)
     (check-model who m)
     (check-samples who samples)
     (check-sample-rate who rate)
     (with-model-call who m address
       (string-result who
         (with-locked (samples) (capi-diarize-pcm address samples (sample-count samples) rate))
         address))]))

(define (named-diarization who call diarizer speaker r samples rate options)
  (check-model who diarizer)
  (check-model who speaker)
  (when r (check-registry who r))
  (check-samples who samples)
  (check-sample-rate who rate)
  (check-speaker-options who options)
  (let ([threshold (speaker-options-record-accept-threshold options)]
        [margin (speaker-options-record-margin options)])
    (call-with-handles who
      (append (model-uses diarizer speaker) (if r (list (cons r #f)) '()))
      (lambda ()
        (or (take-string
              (with-locked (samples)
                (call (handle-address diarizer) (handle-address speaker)
                  (if r (handle-address r) 0) samples (sample-count samples) rate
                  (if (given? threshold) (inexact threshold) 0.0)
                  (if (given? margin) (inexact margin) 0.0))))
            (raise-parakeet-error who
              (models-error (list (cons "diarization" diarizer) (cons "speaker" speaker)))))))))

(define diarize-named-pcm-json
  (case-lambda
    [(diarizer speaker r samples)
     (diarize-named-pcm-json diarizer speaker r samples 16000 default-speaker-options)]
    [(diarizer speaker r samples rate)
     (diarize-named-pcm-json diarizer speaker r samples rate default-speaker-options)]
    [(diarizer speaker r samples rate options)
     (check-registry 'diarize-named-pcm-json r)
     (named-diarization 'diarize-named-pcm-json capi-diarize-named-pcm-json
       diarizer speaker r samples rate options)]))

(define diarize-profiles-pcm-json
  (case-lambda
    [(diarizer speaker r samples)
     (diarize-profiles-pcm-json diarizer speaker r samples 16000 default-speaker-options)]
    [(diarizer speaker r samples rate)
     (diarize-profiles-pcm-json diarizer speaker r samples rate default-speaker-options)]
    [(diarizer speaker r samples rate options)
     (named-diarization 'diarize-profiles-pcm-json capi-diarize-profiles-pcm-json
       diarizer speaker r samples rate options)]))

(define-record-type attributed-utterance
  (nongenerative) (sealed #t)
  (fields speaker text start end confidence))

(define (take-utterances p n)
  (dynamic-wind
    void
    (lambda ()
      (let loop ([i (fx- n 1)] [utterances '()])
        (if (fx< i 0)
            utterances
            (let ([base (+ p (fx* i sas-result-size))])
              (loop (fx- i 1)
                (cons (make-attributed-utterance
                        (let ([speaker (foreign-ref 'int base sas-result-speaker-offset)])
                          (and (>= speaker 0) speaker))
                        (c-string->string (foreign-ref 'uptr base sas-result-text-offset))
                        (foreign-ref 'float base sas-result-start-offset)
                        (foreign-ref 'float base sas-result-end-offset)
                        (foreign-ref 'float base sas-result-conf-offset))
                      utterances))))))
    (lambda () (capi-free-sas-results p n))))

(define (speech-and-diarization-error asr diarizer)
  (models-error (list (cons "speech" asr) (cons "diarization" diarizer))))

(define transcribe-and-diarize
  (case-lambda
    [(asr diarizer samples) (transcribe-and-diarize asr diarizer samples 16000)]
    [(asr diarizer samples rate)
     (define who 'transcribe-and-diarize)
     (check-model who asr)
     (check-model who diarizer)
     (check-samples who samples)
     (check-sample-rate who rate)
     (let ([out (make-out-pointers 1)] [count (make-out-pointers 1)])
       (call-with-handles who (model-uses asr diarizer)
         (lambda ()
           (unless (= 0 (with-locked (samples out count)
                          (capi-transcribe-and-diarize (handle-address asr)
                            (handle-address diarizer) samples (sample-count samples) rate
                            out count)))
             (raise-parakeet-error who (speech-and-diarization-error asr diarizer)))
           (take-utterances (out-pointer-ref out 0) (out-int-ref count 0)))))]))

(define transcribe-and-diarize-json
  (case-lambda
    [(asr diarizer samples) (transcribe-and-diarize-json asr diarizer samples 16000)]
    [(asr diarizer samples rate)
     (define who 'transcribe-and-diarize-json)
     (check-model who asr)
     (check-model who diarizer)
     (check-samples who samples)
     (check-sample-rate who rate)
     (call-with-handles who (model-uses asr diarizer)
       (lambda ()
         (or (take-string
               (with-locked (samples)
                 (capi-transcribe-and-diarize-json (handle-address asr) (handle-address diarizer)
                   samples (sample-count samples) rate)))
             (raise-parakeet-error who (speech-and-diarization-error asr diarizer)))))]))

(define transcribe-and-diarize-named-json
  (case-lambda
    [(asr diarizer speaker r samples)
     (transcribe-and-diarize-named-json asr diarizer speaker r samples 16000)]
    [(asr diarizer speaker r samples rate)
     (define who 'transcribe-and-diarize-named-json)
     (check-model who asr)
     (check-model who diarizer)
     (check-model who speaker)
     (check-registry who r)
     (check-samples who samples)
     (check-sample-rate who rate)
     (call-with-handles who (cons (cons r #f) (model-uses asr diarizer speaker))
       (lambda ()
         (or (take-string
               (with-locked (samples)
                 (capi-transcribe-and-diarize-named-json (handle-address asr)
                   (handle-address diarizer) (handle-address speaker) (handle-address r)
                   samples (sample-count samples) rate)))
             (raise-parakeet-error who
               (models-error (list (cons "speech" asr) (cons "diarization" diarizer)
                               (cons "speaker" speaker)))))))]))

;; A stream's calls compute with its models, so they hold them too.

(define (check-stream who s stream? what)
  (unless (stream? s) (assertion-violationf who "~s is not a ~a" s what)))

(define (stream-uses s)
  (cons (cons s #t)
        (map (lambda (p) (cons p (and (model? p) (model-exclusive? p)))) (handle-parents s))))

(define-record-type diarization-segment
  (nongenerative) (sealed #t)
  (fields speaker start end))

(define-record-type diarization-stream
  (parent handle) (nongenerative) (sealed #t)
  (protocol handle-protocol))

(define (check-diarization-stream who s)
  (check-stream who s diarization-stream? "diarization stream"))

(define open-diarization-stream
  (case-lambda
    [(m) (open-diarization-stream m 'model)]
    [(m latency)
     (define who 'open-diarization-stream)
     (check-model who m)
     (unless (diarization-latency? latency)
       (assertion-violationf who "~s is not a diarization latency" latency))
     (with-model-call who m address
       (let ([s (capi-diarize-stream-begin-latency address (diarization-latency-code latency))])
         (when (eqv? s 0) (raise-parakeet-error who (context-error address)))
         (register-handle! (make-diarization-stream s (list m) capi-diarize-stream-free))))]))

(define (close-diarization-stream s)
  (check-diarization-stream 'close-diarization-stream s)
  (close-handle! s))

(define (take-diarization-segments p n)
  (dynamic-wind
    void
    (lambda ()
      (let loop ([i (fx- n 1)] [segments '()])
        (if (fx< i 0)
            segments
            (let ([base (+ p (fx* i diar-segment-size))])
              (loop (fx- i 1)
                (cons (make-diarization-segment
                        (foreign-ref 'int base diar-segment-speaker-offset)
                        (foreign-ref 'float base diar-segment-start-offset)
                        (foreign-ref 'float base diar-segment-end-offset))
                      segments))))))
    (lambda () (capi-free-diar-segments p))))

(define (diarization-stream-call who s samples last?)
  (check-diarization-stream who s)
  (check-samples who samples)
  (let ([out (make-out-pointers 1)] [count (make-out-pointers 1)])
    (call-with-handles who (stream-uses s)
      (lambda ()
        (unless (= 0 (with-locked (samples out count)
                       (capi-diarize-stream-feed (handle-address s) samples (sample-count samples)
                         (if last? 1 0) out count)))
          (raise-parakeet-error who (context-error (handle-address (stream-model s)))))
        (take-diarization-segments (out-pointer-ref out 0) (out-int-ref count 0))))))

(define (diarization-stream-feed s samples)
  (diarization-stream-call 'diarization-stream-feed s samples #f))

(define diarization-stream-finish
  (case-lambda
    [(s) (diarization-stream-finish s (bytevector))]
    [(s samples) (diarization-stream-call 'diarization-stream-finish s samples #t)]))

(define (diarization-stream-active s)
  (define who 'diarization-stream-active)
  (check-diarization-stream who s)
  (let ([out (make-out-pointers 1)] [count (make-out-pointers 1)])
    (call-with-handles who (stream-uses s)
      (lambda ()
        (unless (= 0 (with-locked (out count)
                       (capi-diarize-stream-active (handle-address s) out count)))
          (raise-parakeet-error who (context-error (handle-address (stream-model s)))))
        (take-diarization-segments (out-pointer-ref out 0) (out-int-ref count 0))))))

(define (diarization-stream-time s)
  (define who 'diarization-stream-time)
  (check-diarization-stream who s)
  (with-handles who ([s #t]) (capi-diarize-stream-time (handle-address s))))

(define (diarization-stream-chunk-samples s)
  (define who 'diarization-stream-chunk-samples)
  (check-diarization-stream who s)
  (with-handles who ([s #t]) (capi-diarize-stream-chunk-samples (handle-address s))))

(define-record-type attribution-stream
  (parent handle) (nongenerative) (sealed #t)
  (protocol handle-protocol))

(define (check-attribution-stream who s)
  (check-stream who s attribution-stream? "attribution stream"))

(define open-attribution-stream
  (case-lambda
    [(asr diarizer) (open-attribution-stream asr diarizer 'model)]
    [(asr diarizer latency)
     (define who 'open-attribution-stream)
     (check-model who asr)
     (check-model who diarizer)
     (unless (diarization-latency? latency)
       (assertion-violationf who "~s is not a diarization latency" latency))
     (call-with-handles who (model-uses asr diarizer)
       (lambda ()
         (let ([s (capi-sas-stream-begin-latency (handle-address asr) (handle-address diarizer)
                    (diarization-latency-code latency))])
           (when (eqv? s 0) (raise-parakeet-error who (speech-and-diarization-error asr diarizer)))
           (register-handle!
             (make-attribution-stream s (list asr diarizer) capi-sas-stream-free)))))]))

(define (close-attribution-stream s)
  (check-attribution-stream 'close-attribution-stream s)
  (close-handle! s))

(define (attribution-stream-call who s samples last?)
  (check-attribution-stream who s)
  (check-samples who samples)
  (let ([out (make-out-pointers 1)] [count (make-out-pointers 1)])
    (call-with-handles who (stream-uses s)
      (lambda ()
        (unless (= 0 (with-locked (samples out count)
                       (capi-sas-stream-feed (handle-address s) samples (sample-count samples)
                         (if last? 1 0) out count)))
          (raise-parakeet-error who
            (apply speech-and-diarization-error (handle-parents s))))
        (take-utterances (out-pointer-ref out 0) (out-int-ref count 0))))))

(define (attribution-stream-feed s samples)
  (attribution-stream-call 'attribution-stream-feed s samples #f))

(define attribution-stream-finish
  (case-lambda
    [(s) (attribution-stream-finish s (bytevector))]
    [(s samples) (attribution-stream-call 'attribution-stream-finish s samples #t)]))

(define (model-sound-labels m)
  (define who 'model-sound-labels)
  (check-model who m)
  (with-handles who ([m #f])
    (let* ([address (handle-address m)] [n (capi-num-classes address)])
      (when (< n 0) (assertion-violationf who "~s is not a sound model" m))
      (let ([labels (make-vector n)])
        (do ([i 0 (fx+ i 1)]) ((fx= i n) labels)
          (vector-set! labels i (borrowed-string (capi-class-label address i))))))))

(define-record-type sound-segment
  (nongenerative) (sealed #t)
  (fields class label start end peak))

(define-record-type sound-stream
  (parent handle) (nongenerative) (sealed #t)
  (protocol handle-protocol))

(define (check-sound-stream who s)
  (check-stream who s sound-stream? "sound stream"))

(define (write-sound-options options address)
  (define (set-float! offset value)
    (when (given? value) (foreign-set! 'float address offset (inexact value))))
  (capi-sound-opts-default address)
  (set-float! sound-opts-window-sec-offset (sound-options-record-window options))
  (set-float! sound-opts-hop-sec-offset (sound-options-record-hop options))
  (set-float! sound-opts-on-threshold-offset (sound-options-record-on-threshold options))
  (set-float! sound-opts-off-threshold-offset (sound-options-record-off-threshold options))
  (set-float! sound-opts-min-duration-sec-offset (sound-options-record-min-duration options))
  (when (given? (sound-options-record-top-k options))
    (foreign-set! 'int address sound-opts-top-k-offset (sound-options-record-top-k options))))

(define (call-with-foreign size proc)
  (let ([address (foreign-alloc size)])
    (dynamic-wind void (lambda () (proc address)) (lambda () (foreign-free address)))))

(define open-sound-stream
  (case-lambda
    [(m) (open-sound-stream m default-sound-options)]
    [(m options)
     (define who 'open-sound-stream)
     (check-model who m)
     (check-sound-options who options)
     (call-with-foreign sound-opts-size
       (lambda (opts)
         (write-sound-options options opts)
         (with-model-call who m address
           (let ([s (capi-sound-stream-begin address opts)])
             (when (eqv? s 0) (raise-parakeet-error who (context-error address)))
             (register-handle! (make-sound-stream s (list m) capi-sound-stream-free))))))]))

(define (close-sound-stream s)
  (check-sound-stream 'close-sound-stream s)
  (close-handle! s))

;; Labels belong to the model, which the stream holds while they are copied.
(define (take-sound-segments p n)
  (dynamic-wind
    void
    (lambda ()
      (let loop ([i (fx- n 1)] [segments '()])
        (if (fx< i 0)
            segments
            (let ([base (+ p (fx* i sound-segment-size))])
              (loop (fx- i 1)
                (cons (make-sound-segment
                        (foreign-ref 'int base sound-segment-class-index-offset)
                        (borrowed-string (foreign-ref 'uptr base sound-segment-label-offset))
                        (foreign-ref 'float base sound-segment-start-offset)
                        (foreign-ref 'float base sound-segment-end-offset)
                        (foreign-ref 'float base sound-segment-peak-offset))
                      segments))))))
    (lambda () (capi-free-sound-segments p))))

(define (sound-stream-call who s samples last?)
  (check-sound-stream who s)
  (check-samples who samples)
  (let ([out (make-out-pointers 1)] [count (make-out-pointers 1)])
    (call-with-handles who (stream-uses s)
      (lambda ()
        (unless (= 0 (with-locked (samples out count)
                       (capi-sound-stream-feed (handle-address s) samples (sample-count samples)
                         (if last? 1 0) out count)))
          (raise-parakeet-error who (context-error (handle-address (stream-model s)))))
        (take-sound-segments (out-pointer-ref out 0) (out-int-ref count 0))))))

(define (sound-stream-feed s samples)
  (sound-stream-call 'sound-stream-feed s samples #f))

(define sound-stream-finish
  (case-lambda
    [(s) (sound-stream-finish s (bytevector))]
    [(s samples) (sound-stream-call 'sound-stream-finish s samples #t)]))

(define (sound-stream-active s)
  (define who 'sound-stream-active)
  (check-sound-stream who s)
  (let ([out (make-out-pointers 1)] [count (make-out-pointers 1)])
    (call-with-handles who (stream-uses s)
      (lambda ()
        (unless (= 0 (with-locked (out count)
                       (capi-sound-stream-active (handle-address s) out count)))
          (raise-parakeet-error who (context-error (handle-address (stream-model s)))))
        (take-sound-segments (out-pointer-ref out 0) (out-int-ref count 0))))))

(define (sound-stream-drain-scores-json s)
  (define who 'sound-stream-drain-scores-json)
  (check-sound-stream who s)
  (call-with-handles who (stream-uses s)
    (lambda ()
      (string-result who (capi-sound-stream-drain-scores-json (handle-address s))
        (handle-address (stream-model s))))))

(define-record-type scene-stream
  (parent handle) (nongenerative) (sealed #t)
  (protocol handle-protocol))

(define (check-scene-stream who s)
  (check-stream who s scene-stream? "scene stream"))

(define (write-scene-options options address)
  (define (set-float! offset value)
    (when (given? value) (foreign-set! 'float address offset (inexact value))))
  (let ([speaker (scene-options-record-speaker options)])
    (capi-scene-opts-default address)
    (foreign-set! 'int address scene-opts-diar-latency-offset
      (diarization-latency-code (scene-options-record-diarization-latency options)))
    (write-sound-options (scene-options-record-sound options) (+ address scene-opts-sound-offset))
    (set-float! scene-opts-speaker-accept-threshold-offset (speaker-options-record-accept-threshold speaker))
    (set-float! scene-opts-speaker-margin-offset (speaker-options-record-margin speaker))
    (set-float! scene-opts-speaker-min-voice-sec-offset (speaker-options-record-min-voice speaker))
    (set-float! scene-opts-speaker-refresh-sec-offset (speaker-options-record-refresh speaker))
    (set-float! scene-opts-speaker-max-voice-sec-offset (speaker-options-record-max-voice speaker))))

(define open-scene-stream
  (case-lambda
    [(asr diarizer tagger) (open-scene-stream asr diarizer tagger #f #f default-scene-options)]
    [(asr diarizer tagger options)
     (open-scene-stream asr diarizer tagger #f #f options)]
    [(asr diarizer tagger speaker r)
     (open-scene-stream asr diarizer tagger speaker r default-scene-options)]
    [(asr diarizer tagger speaker r options)
     (define who 'open-scene-stream)
     (for-each (lambda (m) (check-model-or-false who m)) (list asr diarizer tagger speaker))
     (unless (or asr diarizer tagger) (assertion-violationf who "a scene needs a model"))
     (when (or speaker r)
       (unless (and speaker diarizer) (assertion-violationf who "naming speakers needs a speaker and a diarization model"))
       (check-registry who r))
     (check-scene-options who options)
     (let ([parents (filter values (list asr diarizer tagger speaker r))])
       (call-with-foreign scene-opts-size
         (lambda (opts)
           (write-scene-options options opts)
           (call-with-handles who
             (append (model-uses asr diarizer tagger speaker) (if r (list (cons r #f)) '()))
             (lambda ()
               (let ([s (capi-scene-stream-begin-speaker (model-address-or-0 asr)
                          (model-address-or-0 diarizer) (model-address-or-0 tagger)
                          (model-address-or-0 speaker) (if r (handle-address r) 0) opts)])
                 (when (eqv? s 0)
                   (raise-parakeet-error who
                     (models-error (list (cons "speech" asr) (cons "diarization" diarizer)
                                     (cons "sound" tagger) (cons "speaker" speaker)))))
                 (register-handle! (make-scene-stream s parents capi-scene-stream-free))))))))]))

(define (close-scene-stream s)
  (check-scene-stream 'close-scene-stream s)
  (close-handle! s))

(define (scene-stream-call who s samples last?)
  (check-scene-stream who s)
  (check-samples who samples)
  (call-with-handles who (stream-uses s)
    (lambda ()
      (or (take-string
            (with-locked (samples)
              (capi-scene-stream-feed-json (handle-address s) samples (sample-count samples)
                (if last? 1 0))))
          (raise-parakeet-error who
            (c-string->string (capi-scene-stream-last-error (handle-address s))))))))

(define (scene-stream-feed-json s samples)
  (scene-stream-call 'scene-stream-feed-json s samples #f))

(define scene-stream-finish-json
  (case-lambda
    [(s) (scene-stream-finish-json s (bytevector))]
    [(s samples) (scene-stream-call 'scene-stream-finish-json s samples #t)]))

(define (scene-stream-drain-scores-json s)
  (define who 'scene-stream-drain-scores-json)
  (check-scene-stream who s)
  (call-with-handles who (stream-uses s)
    (lambda ()
      (or (take-string (capi-scene-stream-drain-scores-json (handle-address s)))
          (raise-parakeet-error who
            (c-string->string (capi-scene-stream-last-error (handle-address s))))))))
