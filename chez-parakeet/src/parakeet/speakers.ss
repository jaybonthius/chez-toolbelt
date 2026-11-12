(define (borrowed-string address)
  (if (eqv? address 0) #f (c-string->string address)))

(define (empty->false s) (if (or (not s) (string=? s "")) #f s))

(define (speaker-model-string who m getter)
  (check-model who m)
  (with-handles who ([m #f])
    (empty->false (borrowed-string (getter (handle-address m))))))

(define (model-speaker-dimension m)
  (define who 'model-speaker-dimension)
  (check-model who m)
  (with-handles who ([m #f])
    (let ([n (capi-speaker-dim (handle-address m))])
      (and (>= n 0) n))))

(define (model-speaker-family m)
  (speaker-model-string 'model-speaker-family m capi-speaker-encoder-family))

(define (model-speaker-identity m)
  (speaker-model-string 'model-speaker-identity m capi-speaker-identity))

;; The warning is replaced by the model's next check, so it is read with the
;; model held as a computation holds it.
(define (model-speaker-warning m)
  (define who 'model-speaker-warning)
  (check-model who m)
  (with-model-call who m address
    (empty->false (borrowed-string (capi-speaker-last-warning address)))))

(define-record-type speaker-embedding
  (nongenerative) (sealed #t)
  (fields vector family weights)
  (protocol
    (lambda (new)
      (lambda (floats family weights)
        (define who 'make-speaker-embedding)
        (unless (and (bytevector? floats) (fx> (bytevector-length floats) 0)
                     (fx= 0 (fxand (bytevector-length floats) 3)))
          (assertion-violationf who "~s is not a bytevector of 32-bit floats" floats))
        (unless (or (not family) (string? family))
          (assertion-violationf who "~s is not an encoder family" family))
        (unless (or (not weights) (string? weights))
          (assertion-violationf who "~s is not an encoder weights hash" weights))
        (new floats family weights)))))

(define embed-speaker
  (case-lambda
    [(m samples) (embed-speaker m samples 16000)]
    [(m samples rate)
     (define who 'embed-speaker)
     (check-model who m)
     (check-samples who samples)
     (check-sample-rate who rate)
     (let ([out (make-out-pointers 1)] [dimension (make-out-pointers 1)])
       (with-model-call who m address
         (let ([status (with-locked (samples out dimension)
                         (capi-speaker-embed-pcm address samples (sample-count samples) rate
                           out dimension))])
           (unless (= status 0) (raise-parakeet-error who (context-error address)))
           (let ([p (out-pointer-ref out 0)])
             (dynamic-wind
               void
               (lambda ()
                 (let ([floats (make-bytevector (fx* 4 (out-int-ref dimension 0)))])
                   (c-memcpy-from floats p (bytevector-length floats))
                   (make-speaker-embedding floats
                     (empty->false (borrowed-string (capi-speaker-encoder-family address)))
                     (empty->false (borrowed-string (capi-speaker-identity address))))))
               (lambda () (capi-free-floats p)))))))]))

(define-record-type (speaker-registry new-speaker-registry speaker-registry?)
  (parent handle) (nongenerative) (sealed #t)
  (protocol handle-protocol))

(define (check-registry who r)
  (unless (speaker-registry? r) (assertion-violationf who "~s is not a speaker registry" r)))

(define (make-speaker-registry)
  (let ([address (capi-speaker-registry-new)])
    (when (eqv? address 0)
      (raise-parakeet-error 'make-speaker-registry "out of memory"))
    (register-handle! (new-speaker-registry address '() capi-speaker-registry-free))))

(define (load-speaker-registry path)
  (define who 'load-speaker-registry)
  (check-path who path)
  (let* ([c-path (string->c who path)]
         [address (with-locked (c-path) (capi-speaker-registry-load c-path))])
    (when (eqv? address 0)
      (raise-parakeet-error who (format "~a is missing or is not a speaker registry" path)))
    (register-handle! (new-speaker-registry address '() capi-speaker-registry-free))))

(define (close-speaker-registry r)
  (check-registry 'close-speaker-registry r)
  (close-handle! r))

(define (registry-error r) (c-string->string (capi-speaker-registry-last-error (handle-address r))))

;; A scene stream reads its registry while it runs.
(define (check-not-borrowed who r)
  (when (handle-borrowed? r)
    (assertion-violationf who "~s is in use by an open scene stream" r)))

(define (save-speaker-registry r path)
  (define who 'save-speaker-registry)
  (check-registry who r)
  (check-path who path)
  (let ([c-path (string->c who path)])
    (with-handles who ([r #t])
      (unless (= 0 (with-locked (c-path) (capi-speaker-registry-save (handle-address r) c-path)))
        (raise-parakeet-error who (registry-error r))))))

(define (speaker-registry-size r)
  (define who 'speaker-registry-size)
  (check-registry who r)
  (with-handles who ([r #f]) (capi-speaker-registry-size (handle-address r))))

(define (speaker-registry-family r)
  (define who 'speaker-registry-family)
  (check-registry who r)
  (with-handles who ([r #f])
    (empty->false (borrowed-string (capi-speaker-registry-encoder-family (handle-address r))))))

(define (speaker-registry-weights r)
  (define who 'speaker-registry-weights)
  (check-registry who r)
  (with-handles who ([r #f])
    (empty->false (borrowed-string (capi-speaker-registry-encoder-weights (handle-address r))))))

(define (speaker-registry-strict-set! r strict?)
  (define who 'speaker-registry-strict-set!)
  (check-registry who r)
  (unless (boolean? strict?) (assertion-violationf who "~s is not a boolean" strict?))
  (with-handles who ([r #t])
    (check-not-borrowed who r)
    (capi-speaker-registry-set-strict (handle-address r) (if strict? 1 0))))

(define (check-name who name)
  (unless (and (string? name) (fx> (string-length name) 0))
    (assertion-violationf who "~s is not a speaker name" name)))

(define enroll-speaker!
  (case-lambda
    [(r m name samples) (enroll-speaker! r m name samples 16000)]
    [(r m name samples rate)
     (define who 'enroll-speaker!)
     (check-registry who r)
     (check-model who m)
     (check-name who name)
     (check-samples who samples)
     (check-sample-rate who rate)
     (let ([c-name (string->c who name)])
       (with-handles who ([r #t] [m (model-exclusive? m)])
         (check-not-borrowed who r)
         (unless (= 0 (with-locked (c-name samples)
                        (capi-speaker-enroll (handle-address r) (handle-address m) c-name samples
                          (sample-count samples) rate)))
           (raise-parakeet-error who (context-error (handle-address m))))))]))

(define (speaker-registry-add! r name embedding)
  (define who 'speaker-registry-add!)
  (check-registry who r)
  (check-name who name)
  (unless (speaker-embedding? embedding)
    (assertion-violationf who "~s is not a speaker embedding" embedding))
  (let ([c-name (string->c who name)]
        [floats (speaker-embedding-vector embedding)]
        [c-family (string->c who (speaker-embedding-family embedding))]
        [c-weights (string->c who (speaker-embedding-weights embedding))])
    (with-handles who ([r #t])
      (check-not-borrowed who r)
      (unless (= 0 (with-locked (c-name floats c-family c-weights)
                     (capi-speaker-registry-add-embedding-fp (handle-address r) c-name floats
                       (fxsrl (bytevector-length floats) 2) c-family c-weights)))
        (raise-parakeet-error who (registry-error r))))))

(define identify-speaker-json
  (case-lambda
    [(r m samples) (identify-speaker-json r m samples 16000)]
    [(r m samples rate)
     (define who 'identify-speaker-json)
     (check-registry who r)
     (check-model who m)
     (check-samples who samples)
     (check-sample-rate who rate)
     (with-handles who ([r #f] [m (model-exclusive? m)])
       (string-result who
         (with-locked (samples)
           (capi-speaker-identify-pcm-json (handle-address r) (handle-address m) samples
             (sample-count samples) rate))
         (handle-address m)))]))
