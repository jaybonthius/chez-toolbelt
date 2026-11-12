(import (chezscheme) (parakeet) (check) (fixtures) (json))

(define speaker-model "wespeaker-resnet34-voxceleb.gguf")
(define two-voices (read-wav two-speakers-wav))
(define voice-a (sub-samples two-voices 9600 73600))
(define voice-b (sub-samples two-voices 110400 174400))
(define query-a (sub-samples two-voices 238400 296000))

(define (identified-name r m samples)
  (json-ref (string->json (identify-speaker-json r m samples)) "name"))

(define (with-registry proc)
  (let ([r (make-speaker-registry)])
    (let ([result (proc r)])
      (close-speaker-registry r)
      result)))

(define scratch "build/tests/scratch")

(test "an embedding matches parakeet.cpp's and names its encoder"
  (with-model speaker-model
    (lambda (m)
      (check-equal? 'speaker (model-kind m))
      (check-equal? 256 (model-speaker-dimension m))
      (let ([e (embed-speaker m speech-samples)])
        (check-equal? (reference-bytes "embedding.bin") (speaker-embedding-vector e))
        (check-equal? (model-speaker-family m) (speaker-embedding-family e))
        (check-equal? (model-speaker-identity m) (speaker-embedding-weights e))
        (check (string? (speaker-embedding-family e)))
        (check-equal? "sha256:" (substring (speaker-embedding-weights e) 0 7))
        (let ([floats (speaker-embedding-vector e)])
          (let loop ([i 0] [norm 0.0])
            (if (< i 256)
                (loop (+ i 1) (+ norm (expt (float-ref floats i) 2)))
                (check (< (abs (- norm 1.0)) 1e-5))))))
      (check-raises parakeet-error? (embed-speaker m (bytevector)))))
  (with-model "tdt_ctc-110m-q8_0.gguf"
    (lambda (m)
      (check-equal? #f (model-speaker-dimension m))
      (check-equal? #f (model-speaker-family m))
      (check-equal? #f (model-speaker-identity m)))))

(test "enrolled voices are recognised, refined, saved and loaded"
  (with-model speaker-model
    (lambda (m)
      (with-registry
        (lambda (r)
          (check-equal? 0 (speaker-registry-size r))
          (check-equal? #f (speaker-registry-family r))
          (enroll-speaker! r m "Ada" voice-a)
          (enroll-speaker! r m "Ben" voice-b)
          (check-equal? 2 (speaker-registry-size r))
          (check-equal? (model-speaker-family m) (speaker-registry-family r))
          (check-equal? (model-speaker-identity m) (speaker-registry-weights r))
          (check-equal? "Ada" (identified-name r m query-a))
          (check-equal? "Ben" (identified-name r m voice-b))
          (enroll-speaker! r m "Ada" query-a)
          (check-equal? 2 (speaker-registry-size r))
          (unless (file-exists? scratch) (mkdir scratch))
          (save-speaker-registry r (string-append scratch "/speakers.bin"))
          (let ([loaded (load-speaker-registry (string-append scratch "/speakers.bin"))])
            (check-equal? 2 (speaker-registry-size loaded))
            (check-equal? (speaker-registry-family r) (speaker-registry-family loaded))
            (check-equal? (identify-speaker-json r m query-a) (identify-speaker-json loaded m query-a))
            (close-speaker-registry loaded))
          (check-raises parakeet-error? (enroll-speaker! r m "Cy" (bytevector)))
          (check-raises parakeet-error?
            (save-speaker-registry r (string-append scratch "/missing/speakers.bin")))))
      (check-raises parakeet-error? (load-speaker-registry "/missing/registry.bin"))
      (check-raises parakeet-error? (load-speaker-registry speech-wav)))))

(test "embeddings kept elsewhere enrol with or without their encoder's fingerprint"
  (with-model speaker-model
    (lambda (m)
      (let* ([e (embed-speaker m voice-a)]
             [plain (make-speaker-embedding (speaker-embedding-vector e) #f #f)])
        (with-registry
          (lambda (r)
            (speaker-registry-add! r "Äda 🦜" plain)
            (check-equal? "Äda 🦜" (identified-name r m voice-a))
            (check (string? (model-speaker-warning m)))
            (speaker-registry-strict-set! r #t)
            (check-raises parakeet-error? (identify-speaker-json r m voice-a))
            (speaker-registry-strict-set! r #f)
            (check-raises parakeet-error? (speaker-registry-add! r "Ben" e))))
        (with-registry
          (lambda (r)
            (speaker-registry-add! r "Ada" e)
            (check-equal? (speaker-embedding-family e) (speaker-registry-family r))
            (check-raises parakeet-error? (speaker-registry-add! r "Ben" plain))
            (check-raises parakeet-error?
              (speaker-registry-add! r "Ben"
                (make-speaker-embedding (speaker-embedding-vector e) "another-family"
                  (speaker-embedding-weights e))))
            (check-raises parakeet-error?
              (speaker-registry-add! r "Ben" (make-speaker-embedding (make-bytevector 8 0) #f #f)))
            (check-equal? 1 (speaker-registry-size r))
            (speaker-registry-strict-set! r #t)
            (check-equal? "Ada" (identified-name r m voice-a))
            (check-equal? #f (model-speaker-warning m))))
        (with-registry
          (lambda (r)
            (speaker-registry-add! r "Ada"
              (make-speaker-embedding (speaker-embedding-vector e) (speaker-embedding-family e)
                "sha256:other-quantization"))
            (identified-name r m voice-a)
            (check (string? (model-speaker-warning m)))))))))

(test "a closed registry refuses every use"
  (let ([r (make-speaker-registry)])
    (close-speaker-registry r)
    (close-speaker-registry r)
    (check-raises assertion-violation? (speaker-registry-size r))
    (check-raises assertion-violation? (speaker-registry-family r))
    (check-raises assertion-violation? (speaker-registry-strict-set! r #t))
    (check-raises assertion-violation? (save-speaker-registry r "unused"))
    (check-raises assertion-violation?
      (speaker-registry-add! r "Ada" (make-speaker-embedding (make-bytevector 4 0) #f #f)))))

(test-exit)
