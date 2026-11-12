(import (chezscheme) (parakeet) (check) (fixtures) (json))

(define tdt-ctc "tdt_ctc-110m-q8_0.gguf")

(define (resample-to-8k samples)
  (let* ([n (fxquotient (float-count samples) 2)] [out (make-bytevector (* 4 n))])
    (do ([i 0 (fx+ i 1)]) ((fx= i n) out)
      (bytevector-ieee-single-native-set! out (fx* 4 i) (float-ref samples (fx* 2 i))))))

(test "the C API is the pinned version"
  (check-equal? 11 (abi-version)))

(test "file and PCM transcripts match parakeet.cpp called directly"
  (with-model tdt-ctc
    (lambda (m)
      (check-equal? 'asr (model-kind m))
      (check-equal? expected-transcript (reference-text "transcript-default.txt"))
      (check-equal? (reference-text "transcript-default.txt") (transcribe-file m speech-wav))
      (check-equal? (reference-text "transcript-ctc.txt")
        (transcribe-file m speech-wav (decode-options [decoder (decoder ctc)])))
      (check-equal? (reference-text "transcript-default.txt")
        (transcribe-file m speech-wav (decode-options [decoder (decoder transducer)] [language "en"])))
      (check-equal? (reference-text "pcm-default.txt") (transcribe-pcm m speech-samples))
      (check-equal? (reference-text "transcript-ctc.txt")
        (transcribe-pcm m speech-samples 16000 (decode-options [decoder 'ctc])))
      (check-equal? "" (transcribe-pcm m (silence 16000)))
      (check-equal? "" (transcribe-pcm m (bytevector))))))

(test "PCM at another rate is resampled"
  (with-model tdt-ctc
    (lambda (m)
      (let ([text (transcribe-pcm m (resample-to-8k speech-samples) 8000)])
        (check (string? text))
        (check (> (string-length text) 50))))))

(test "timestamped JSON matches parakeet.cpp and the word filter applies"
  (with-model tdt-ctc
    (lambda (m)
      (let ([document (transcribe-file-json m speech-wav)])
        (check-equal? (reference-text "json.txt") document)
        (let ([json (string->json document)])
          (check-equal? expected-transcript (json-ref json "text"))
          (check-equal? 23 (length (json-ref json "words")))
          (check-equal? 0.08 (json-ref json "frame_sec"))))
      (let ([json (string->json
                    (transcribe-file-json m speech-wav (decode-options)
                      (word-filter [min-local-confidence 0.01] [local-radius 5]
                                   [drop-punctuation-only #t])))])
        (check-equal? expected-transcript (json-ref json "text"))
        (check-equal? 0 (json-ref json "guard" "dropped_words"))))))

(test "n-best hypotheses match parakeet.cpp"
  (with-model tdt-ctc
    (lambda (m)
      (let ([document (transcribe-pcm-nbest-json m speech-samples 2)])
        (check-equal? (reference-text "nbest.txt") document)
        (let ([json (string->json document)])
          (check-equal? 2 (length (json-ref json "hypotheses")))
          (check-equal? expected-transcript (json-ref json "hypotheses" 0 "text"))))
      (check (string? (transcribe-file-nbest-json m speech-wav 3
                        (decode-options [beam-size 3] [normalized-ranking #f])))))))

(test "batches match parakeet.cpp"
  (with-model tdt-ctc
    (lambda (m)
      (let* ([n (float-count speech-samples)]
             [clips (vector (sub-samples speech-samples 0 (quotient n 2))
                            (sub-samples speech-samples (quotient n 2) n))])
        (let ([texts (transcribe-pcm-batch m clips 16000 (decode-options [decoder 'ctc]))])
          (check-equal? (reference-text "batch.txt")
            (string-append (vector-ref texts 0) "\n" (vector-ref texts 1))))
        (check-equal? (reference-text "batch-json.txt") (transcribe-pcm-batch-json m clips))
        (check-equal? (vector) (transcribe-pcm-batch m (vector)))
        (check-equal? "[]" (transcribe-pcm-batch-json m (vector)))
        (check-equal? (vector "" (reference-text "pcm-default.txt"))
          (transcribe-pcm-batch m (vector (bytevector) speech-samples)))))))

(test "CTC log probabilities match parakeet.cpp bit for bit"
  (with-model tdt-ctc
    (lambda (m)
      (let ([logits (transcribe-pcm-logits m speech-samples)]
            [shape (let ([p (open-string-input-port (reference-text "logits-shape.txt"))])
                     (let* ([frames (read p)] [vocabulary (read p)]) (list frames vocabulary)))])
        (check (ctc-logits? logits))
        (check-equal? (car shape) (ctc-logits-frames logits))
        (check-equal? (cadr shape) (ctc-logits-vocabulary-size logits))
        (check-equal? (reference-bytes "logits.bin") (ctc-logits-values logits))))))

(test "a speech model with a VAD head segments long audio"
  (with-model "redux-packed.gguf"
    (lambda (m)
      (let ([json (string->json (transcribe-file-vad-json m speech-wav))])
        (check (string? (json-ref json "text")))
        (check (positive? (length (json-ref json "words")))))
      (let ([json (string->json (vad-pcm-json m speech-samples 16000
                                  (vad-options [probabilities #t])))])
        (check-equal? 0.08 (json-ref json "frame_sec"))
        (check (positive? (length (json-ref json "probabilities"))))
        (check (positive? (length (json-ref json "segments"))))))))

(test "a Silero model segments audio for any speech model"
  (with-model tdt-ctc
    (lambda (m)
      (with-model "parakeet-bundle-small.gguf" "vad"
        (lambda (vad)
          (check-equal? 'vad (model-kind vad))
          (let ([json (string->json
                        (transcribe-file-vad-json m speech-wav vad
                          (vad-options [max-segment 4] [threshold 0.5])
                          (decode-options) (word-filter [min-local-confidence 0.01])))])
            (check (> (string-length (json-ref json "text")) 50)))))
      (check-raises parakeet-error? (transcribe-file-vad-json m speech-wav)))))

(test "bundles list their components and load each kind"
  (let* ([bundle (model-path "parakeet-bundle-small.gguf")]
         [components (string->json (bundle-components-json bundle))])
    (check-equal? '("asr" "diar" "ced" "voice" "vad")
      (map (lambda (c) (json-ref c "name")) components))
    (check-equal? "614feee3a990cf0e672b0314f4da0c80ae8da9094507f5ccb7c42e43b5fc5a12"
      (json-ref (car components) "source_sha256"))
    (for-each
      (lambda (name kind)
        (with-model "parakeet-bundle-small.gguf" name
          (lambda (m) (check-equal? kind (model-kind m)))))
      '("asr" "diar" "ced" "voice" "vad")
      '(asr diarization sound speaker vad))
    (with-model "parakeet-bundle-small.gguf"
      (lambda (m)
        (check-equal? 'asr (model-kind m))
        (check-equal? expected-transcript (transcribe-file m speech-wav))))
    (check-raises parakeet-error? (load-model bundle "absent"))
    (check-raises parakeet-error? (bundle-components-json (model-path tdt-ctc)))
    (check-raises parakeet-error? (load-model (model-path tdt-ctc) "asr"))))

(test "failures raise parakeet errors with parakeet.cpp's message"
  (check-raises parakeet-error? (load-model "/missing/model.gguf"))
  (with-model tdt-ctc
    (lambda (m)
      (guard (c [(parakeet-error? c)
                 (check (who-condition? c))
                 (check-equal? 'transcribe-file (condition-who c))
                 (check (positive? (string-length (condition-message c))))])
        (transcribe-file m "/missing/audio.wav")
        (check #f))
      (check-equal? expected-transcript (transcribe-file m speech-wav)))))

(test "concurrency spreads one model over several backends"
  (with-model tdt-ctc
    (lambda (m)
      (check-equal? 2 (model-set-concurrency! m 2 4))
      (let* ([results (make-vector 4 #f)]
             [mutex (make-mutex)]
             [done (make-condition)]
             [finished 0])
        (do ([i 0 (fx+ i 1)]) ((fx= i 4))
          (let ([i i])
            (fork-thread
              (lambda ()
                (let ([text (transcribe-pcm m speech-samples)])
                  (with-mutex mutex
                    (vector-set! results i text)
                    (set! finished (+ finished 1))
                    (condition-signal done)))))))
        (with-mutex mutex
          (let wait () (unless (= finished 4) (condition-wait done mutex) (wait))))
        (check-equal? (make-vector 4 expected-transcript) results))
      (check-equal? 1 (model-set-concurrency! m 1))))
  (with-model "parakeet-bundle-small.gguf" "vad"
    (lambda (vad) (check-raises parakeet-error? (model-set-concurrency! vad 2)))))

(test-exit)
