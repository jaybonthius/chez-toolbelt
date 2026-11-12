(import (chezscheme) (parakeet) (check) (fixtures) (json))

(define hybrid-model "tdt_ctc-110m-q8_0.gguf")
(define rnnt-model "realtime_eou_120m-v1-q8_0.gguf")
(define prompt-model "nemotron-3.5-asr-streaming-0.6b-q8_0.gguf")
(define rnnt-text
  "well i don't wish to see it any more observed phoebe turning away her eyes it is certainly very like the old portrait")
(define commands '("split view right" "next line" "kill buffer" "save file" "open file" "undo"))

(define (quoted text)
  (call-with-string-output-port (lambda (p) (write text p))))

(define (choice phrases)
  (call-with-string-output-port
    (lambda (p)
      (put-string p "root ::= ")
      (let loop ([phrases phrases] [first? #t])
        (unless (null? phrases)
          (unless first? (put-string p " | "))
          (put-string p (quoted (car phrases)))
          (loop (cdr phrases) #f))))))

(define (score-per-frame document)
  (let ([json (string->json document)])
    (/ (- (json-ref json "score") (json-ref json "free_score")) (max 1 (json-ref json "frames")))))

(define (with-grammar m rules proc)
  (let ([g (compile-grammar m rules)])
    (let ([result (proc g)])
      (close-grammar g)
      result)))

(test "accepted text, empty text and no match are told apart"
  (with-model hybrid-model
    (lambda (m)
      (with-grammar m "root ::= \"\""
        (lambda (empty)
          (with-grammar m "root ::= \"a\""
            (lambda (word)
              (for-each
                (lambda (d)
                  (let ([options (decode-options [decoder d])])
                    (check-equal? "" (transcribe-pcm-grammar m empty (bytevector) 16000 options))
                    (check-equal? #f (transcribe-pcm-grammar m word (bytevector) 16000 options))
                    (check-equal? 0 (json-ref (string->json
                                                (transcribe-pcm-grammar-json m empty (bytevector)
                                                  16000 options))
                                      "frames"))
                    (check-equal? #f (transcribe-pcm-grammar-json m word (bytevector) 16000 options))))
                '(ctc transducer default))
              (check-raises parakeet-error? (compile-grammar m "root ::= root"))
              (check-raises parakeet-error? (compile-grammar m "start ::= \"a\""))
              (check-raises parakeet-error? (transcribe-file-grammar m word "/missing.wav"))
              (check-raises parakeet-error?
                (transcribe-pcm-grammar m word (bytevector) 16000 (decode-options [beam-size 65])))
              (check-raises parakeet-error? (open-grammar-stream m word)))))))))

(test "offline grammars match parakeet.cpp and constrain each decoder"
  (with-model hybrid-model
    (lambda (m)
      (with-grammar m "root ::= \"well\" | \"Well\""
        (lambda (g)
          (check-equal? (reference-text "grammar.txt")
            (transcribe-pcm-grammar-json m g speech-samples 16000 (decode-options [decoder 'ctc])))
          (check-equal? "well" (transcribe-pcm-grammar m g speech-samples 16000
                                 (decode-options [decoder 'ctc])))
          (check-equal? "Well" (transcribe-pcm-grammar m g speech-samples))
          (check-equal? "Well" (transcribe-file-grammar m g speech-wav
                                 (decode-options [decoder 'transducer])))))
      (with-grammar m (choice (list expected-transcript))
        (lambda (g)
          (check-equal? expected-transcript (transcribe-file-grammar m g speech-wav))
          (check-equal? expected-transcript
            (transcribe-pcm-grammar m g speech-samples 16000 (decode-options [language "en"])))))))
  (with-model rnnt-model
    (lambda (m)
      (with-grammar m (choice (list rnnt-text))
        (lambda (g)
          (check-equal? rnnt-text (transcribe-pcm-grammar m g speech-samples))
          (check-raises parakeet-error?
            (transcribe-pcm-grammar m g speech-samples 16000 (decode-options [decoder 'ctc])))))))
  (with-model prompt-model
    (lambda (m)
      (with-grammar m (choice '("Well, I don't wish to see it any more"))
        (lambda (g)
          (check-equal? "Well, I don't wish to see it any more"
            (transcribe-pcm-grammar m g (sub-samples speech-samples 0 40000) 16000
              (decode-options [language "en"])))
          (check-raises parakeet-error?
            (transcribe-pcm-grammar m g speech-samples 16000 (decode-options [language "zzz"]))))))))

(test "scores tell the spoken phrase from a forced one"
  (for-each
    (lambda (model spoken decoders)
      (with-model model
        (lambda (m)
          (with-grammar m (choice (cons spoken commands))
            (lambda (matching)
              (with-grammar m (choice commands)
                (lambda (unrelated)
                  (for-each
                    (lambda (d)
                      (let* ([options (decode-options [decoder d] [beam-size 8])]
                             [scored (transcribe-pcm-grammar-json m matching speech-samples 16000
                                       options)]
                             [forced (transcribe-pcm-grammar-json m unrelated speech-samples 16000
                                       options)])
                        (check-equal? spoken (json-ref (string->json scored) "text"))
                        (check-equal? scored
                          (transcribe-file-grammar-json m matching speech-wav options))
                        (check (> (score-per-frame scored) -0.05))
                        (check (or (not forced) (< (score-per-frame forced) -0.3)))))
                    decoders))))))))
    (list hybrid-model rnnt-model)
    (list expected-transcript rnnt-text)
    '((ctc transducer) (transducer))))

(test "a grammar belongs to the model it was compiled for"
  (with-model rnnt-model
    (lambda (a)
      (with-model rnnt-model
        (lambda (b)
          (with-grammar a "root ::= \"a\""
            (lambda (g)
              (check-raises assertion-violation? (transcribe-pcm-grammar b g (bytevector)))
              (check-raises assertion-violation? (transcribe-file-grammar-json b g speech-wav))
              (check-raises assertion-violation? (open-grammar-stream b g))))))))
  (with-model rnnt-model
    (lambda (m)
      (let ([g (compile-grammar m "root ::= \"a\"")])
        (close-grammar g)
        (close-grammar g)
        (check-raises assertion-violation? (transcribe-pcm-grammar m g (bytevector)))
        (check-raises assertion-violation? (open-grammar-stream m g))))))

(define (run-grammar-stream s samples size)
  (let ([units '()] [matched? #t])
    (let ([n (float-count samples)])
      (let loop ([at 0])
        (when (and matched? (< at n))
          (let ([end (min n (+ at size))])
            (let-values ([(document ok?) (grammar-stream-feed-json s (sub-samples samples at end))])
              (set! units (append units (json-ref (string->json document) "units")))
              (set! matched? ok?))
            (loop end)))))
    (if matched?
        (let-values ([(document ok?) (grammar-stream-finish-json s)])
          (values (append units (json-ref (string->json document) "units")) ok?))
        (values units #f))))

(test "a grammar stream commits units incrementally, then restarts after a reset"
  (with-model rnnt-model
    (lambda (m)
      (let* ([g (compile-grammar m (choice (list rnnt-text "hello world")))]
             [s (open-grammar-stream m g)])
        (close-grammar g)
        (for-each
          (lambda (size)
            (let-values ([(units ok?) (run-grammar-stream s streaming-samples size)])
              (check ok?)
              (check-equal? (list rnnt-text) (map (lambda (u) (json-ref u "text")) units))
              (let ([unit (car units)])
                (check (<= (json-ref unit "score") 0))
                (check (> (json-ref unit "frames") 0))
                (check (list? (json-ref unit "alternatives")))))
            (check-raises parakeet-error? (grammar-stream-feed-json s (silence 1600)))
            (grammar-stream-reset! s))
          '(1600 4000))
        (close-grammar-stream s))
      (with-grammar m (choice commands)
        (lambda (g)
          (let ([s (open-grammar-stream m g (decode-options [beam-size 4]))])
            (let-values ([(units ok?) (run-grammar-stream s streaming-samples 1600)])
              (check (not ok?)))
            (close-grammar-stream s)))))))

(test "a prompt-conditioned grammar stream takes a language"
  (with-model prompt-model
    (lambda (m)
      (with-grammar m (choice '("well i don't wish to see it any more"))
        (lambda (g)
          (let ([s (open-grammar-stream m g (decode-options [language "en"]))])
            (let-values ([(units ok?) (run-grammar-stream s (sub-samples speech-samples 0 40000) 1600)])
              (check (boolean? ok?)))
            (close-grammar-stream s))
          (check-raises parakeet-error?
            (open-grammar-stream m g (decode-options [language "zzz"]))))))))

(test-exit)
