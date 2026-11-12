(library (fixtures)
  (export model-path speech-wav two-speakers-wav thunder-wav expected-transcript
          read-wav speech-samples streaming-samples sub-samples silence
          reference-path reference-text reference-bytes
          float-ref float-count with-model)
  (import (chezscheme) (parakeet))

  (define (model-path name)
    (let ([directory (getenv "PARAKEET_MODELS")])
      (unless directory (error 'model-path "PARAKEET_MODELS is not set; run the tests with make"))
      (string-append directory "/" name)))

  (define speech-wav "vendor/parakeet.cpp/tests/fixtures/speech.wav")
  (define two-speakers-wav "vendor/parakeet.cpp/tests/fixtures/two_speakers.wav")
  (define thunder-wav "vendor/parakeet.cpp/third_party/ced.cpp/benchmarks/demo/clips/thunder.wav")

  (define expected-transcript
    "Well, I don't wish to see it any more, observed Phoebe, turning away her eyes. It is certainly very like the old portrait.")

  (define (read-bytes path)
    (call-with-port (open-file-input-port path) get-bytevector-all))

  ;; 16-bit mono WAV, the fixtures' format, as 32-bit float samples scaled
  ;; as parakeet.cpp's reader scales them.
  (define (read-wav path)
    (let* ([bytes (read-bytes path)] [size (bytevector-length bytes)])
      (let find ([at 12])
        (cond
          [(> (+ at 8) size) (error 'read-wav "no data chunk" path)]
          [(equal? (utf8->string (subbytevector bytes at (+ at 4))) "data")
           (let* ([n (quotient (bytevector-u32-ref bytes (+ at 4) 'little) 2)]
                  [samples (make-bytevector (* 4 n))])
             (do ([i 0 (fx+ i 1)]) ((fx= i n) samples)
               (bytevector-ieee-single-native-set! samples (fx* 4 i)
                 (/ (inexact (bytevector-s16-ref bytes (fx+ (+ at 8) (fx* 2 i)) 'little))
                    32768.0))))]
          [else (find (+ at 8 (bytevector-u32-ref bytes (+ at 4) 'little)))]))))

  (define (subbytevector bytes start end)
    (let ([out (make-bytevector (- end start))])
      (bytevector-copy! bytes start out 0 (- end start))
      out))

  (define speech-samples (read-wav speech-wav))

  (define (silence n) (make-bytevector (* 4 n) 0))

  (define (sub-samples samples start end)
    (subbytevector samples (* 4 start) (* 4 end)))

  (define (float-count samples) (fxsrl (bytevector-length samples) 2))
  (define (float-ref samples i) (bytevector-ieee-single-native-ref samples (fx* 4 i)))

  ;; The speech, then three seconds of silence for the stream to end on.
  (define streaming-samples
    (let ([samples (make-bytevector (+ (bytevector-length speech-samples) (* 4 48000)) 0)])
      (bytevector-copy! speech-samples 0 samples 0 (bytevector-length speech-samples))
      samples))

  (define (reference-path name) (string-append "build/reference/" name))
  (define (reference-bytes name) (read-bytes (reference-path name)))
  (define (reference-text name) (utf8->string (reference-bytes name)))

  (define with-model
    (case-lambda
      [(name proc) (with-model name #f proc)]
      [(name component proc)
       (let ([m (load-model (model-path name) component)])
         (dynamic-wind void (lambda () (proc m)) (lambda () (close-model m))))])))
