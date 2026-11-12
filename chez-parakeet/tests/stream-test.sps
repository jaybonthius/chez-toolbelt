(import (chezscheme) (parakeet) (check) (fixtures) (json))

(define eou-model "realtime_eou_120m-v1-q8_0.gguf")
(define prompt-model "nemotron-3.5-asr-streaming-0.6b-q8_0.gguf")
(define unified-model "nemotron-labs-unified-asr-0.6b-q4_k.gguf")

(define (for-each-piece proc samples size)
  (let ([n (float-count samples)])
    (let loop ([at 0])
      (when (< at n)
        (let ([end (min n (+ at size))])
          (proc (sub-samples samples at end))
          (loop end))))))

(define (stream-transcript s samples size)
  (let ([text (open-output-string)])
    (for-each-piece
      (lambda (piece)
        (let-values ([(delta fired) (asr-stream-feed s piece)]) (put-string text delta)))
      samples size)
    (put-string text (asr-stream-finish s))
    (get-output-string text)))

(define (reference-events)
  (let ([bytes (reference-bytes "stream-events.bin")])
    (let loop ([at (- (bytevector-length bytes) 16)] [events '()])
      (if (< at 0)
          events
          (loop (- at 16)
            (cons (list (if (= 0 (bytevector-s32-native-ref bytes (+ at 4))) 'eou 'eob)
                        (bytevector-s32-native-ref bytes at)
                        (bytevector-s32-native-ref bytes (+ at 8))
                        (bytevector-ieee-single-native-ref bytes (+ at 12)))
                  events))))))

(define (reference-lines name)
  (let ([p (open-string-input-port (reference-text name))])
    (let loop ([lines '()])
      (let ([line (get-line p)])
        (if (eof-object? line) (reverse lines) (loop (cons line lines)))))))

(define (event->list e)
  (list (stream-event-kind e) (stream-event-token e) (stream-event-frame e) (stream-event-time e)))

(test "a cache-aware stream's text and turn events match parakeet.cpp"
  (with-model eou-model
    (lambda (m)
      (check-equal? 'cache-aware (model-stream-kind m))
      (check (model-stream-eou-events? m))
      (check-equal? "[]" (model-stream-presets-json m))
      (let ([s (open-asr-stream m)] [text (open-output-string)] [events '()] [kinds '()])
        (check-equal? "cache_aware" (json-ref (string->json (asr-stream-info-json s)) "kind"))
        (let-values ([(piece fired) (asr-stream-feed s (bytevector))])
          (check-equal? "" piece)
          (check-equal? '() fired))
        (for-each-piece
          (lambda (piece)
            (let-values ([(delta fired) (asr-stream-feed s piece)])
              (put-string text delta)
              (let ([drained (asr-stream-drain-events s)])
                (check-equal? (map stream-event-kind drained) fired)
                (set! events (append events drained)))))
          streaming-samples 1600)
        (put-string text (asr-stream-finish s))
        (set! events (append events (asr-stream-drain-events s)))
        (check-equal? (reference-text "stream-text.txt") (get-output-string text))
        (check (pair? events))
        (check-equal? (reference-events) (map event->list events))
        (check-raises assertion-violation? (asr-stream-feed s (bytevector)))
        (check-raises assertion-violation? (asr-stream-finish s))
        (check-equal? '() (asr-stream-drain-events s))
        (asr-stream-reset! s)
        (check-equal? (reference-text "stream-text.txt") (stream-transcript s streaming-samples 1600))
        (close-asr-stream s)
        (close-asr-stream s)
        (check-raises assertion-violation? (asr-stream-drain-events s))))))

(test "a stream's JSON documents match parakeet.cpp call by call"
  (with-model eou-model
    (lambda (m)
      (let ([s (open-asr-stream m)] [documents '()])
        (for-each-piece
          (lambda (piece) (set! documents (cons (asr-stream-feed-json s piece) documents)))
          streaming-samples 1600)
        (set! documents (cons (asr-stream-finish-json s) documents))
        (let ([expected (reference-lines "stream-json.txt")] [documents (reverse documents)])
          (check-equal? (length expected) (length documents))
          (for-each (lambda (e d) (check-equal? e d)) expected documents))
        (check-equal? '() (asr-stream-drain-events s))
        (close-asr-stream s)))))

(test "pieces of any size give the same transcript"
  (with-model eou-model
    (lambda (m)
      (let ([s (open-asr-stream m)])
        (for-each
          (lambda (size)
            (check-equal? (reference-text "stream-text.txt")
              (stream-transcript s streaming-samples size))
            (asr-stream-reset! s))
          '(160 4000 1000000))
        (close-asr-stream s)))))

(test "a prompt-conditioned stream takes a language"
  (with-model prompt-model
    (lambda (m)
      (for-each
        (lambda (language)
          (let ([s (open-asr-stream m (asr-stream-options [language language]))])
            (let ([text (stream-transcript s streaming-samples 1600)])
              (check (> (string-length text) 50)))
            (close-asr-stream s)))
        '("en" "auto"))
      (check-raises parakeet-error? (open-asr-stream m (asr-stream-options [language "xx-none"])))
      (check-raises parakeet-error? (open-asr-stream m (asr-stream-options [preset 0])))
      (check-raises parakeet-error?
        (transcribe-pcm m speech-samples 16000 (decode-options [language "xx-none"]))))))

(test "a unified model streams in buffered windows"
  (with-model unified-model
    (lambda (m)
      (check-equal? 'buffered (model-stream-kind m))
      (check (not (model-stream-eou-events? m)))
      (let ([presets (string->json (model-stream-presets-json m))])
        (check (pair? presets))
        (for-each
          (lambda (options)
            (let ([s (open-asr-stream m options)])
              (check-equal? "buffered" (json-ref (string->json (asr-stream-info-json s)) "kind"))
              (let ([text (stream-transcript s streaming-samples 8000)])
                (check (> (string-length text) 50)))
              (check-equal? '() (asr-stream-drain-events s))
              (close-asr-stream s)))
          (list (asr-stream-options)
                (asr-stream-options [preset (json-ref (car (reverse presets)) "preset")])
                (asr-stream-options [left-frames 70] [chunk-frames 7] [right-frames 7]))))
      (check-raises parakeet-error?
        (open-asr-stream m (asr-stream-options [left-frames 4000] [chunk-frames 1] [right-frames 0])))
      (check-raises parakeet-error? (open-asr-stream m (asr-stream-options [preset 1000]))))))

(test "an offline model has no stream"
  (with-model "tdt_ctc-110m-q8_0.gguf"
    (lambda (m)
      (check-equal? 'none (model-stream-kind m))
      (check-raises parakeet-error? (open-asr-stream m)))))

(test "Silero VAD matches parakeet.cpp, and a VAD stream matches offline VAD"
  (with-model "parakeet-bundle-small.gguf" "vad"
    (lambda (m)
      (let ([document (vad-pcm-json m speech-samples 16000 (vad-options [probabilities #t]))])
        (check-equal? (reference-text "vad.txt") document)
        (let ([offline (string->json document)])
          (check-equal? 3 (length (json-ref offline "segments")))
          (check-equal? (json-ref offline "segments")
            (json-ref (string->json (vad-file-json m speech-wav)) "segments"))
          (let ([s (open-vad-stream m 16000 (vad-options [probabilities #t]))])
            (for-each
              (lambda (size)
                (let ([probabilities '()] [events '()])
                  (for-each-piece
                    (lambda (piece)
                      (let ([json (string->json (vad-stream-feed-json s piece))])
                        (set! probabilities (append probabilities (json-ref json "probabilities")))
                        (set! events (append events (json-ref json "events")))))
                    speech-samples size)
                  (let ([json (string->json (vad-stream-finish-json s))])
                    (set! probabilities (append probabilities (json-ref json "probabilities")))
                    (set! events (append events (json-ref json "events"))))
                  (check-equal? (json-ref offline "probabilities") probabilities)
                  (check-equal? (* 2 (length (json-ref offline "segments"))) (length events))
                  (check-raises parakeet-error? (vad-stream-feed-json s (silence 10)))
                  (vad-stream-reset! s)))
              '(512 1000 16000))
            (close-vad-stream s))))
      (check-raises parakeet-error? (open-vad-stream m 44100))
      (check-raises parakeet-error? (open-vad-stream m 16000 (vad-options [mode (vad-mode segments)])))
      (check-raises parakeet-error? (open-vad-stream m 16000 (vad-options [run-gate 0.5])))))
  (with-model eou-model
    (lambda (m) (check-raises parakeet-error? (open-vad-stream m)))))

(test-exit)
