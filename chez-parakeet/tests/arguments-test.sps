(import (chezscheme) (parakeet) (check) (fixtures))

(define (expands? form)
  (guard (c [(syntax-violation? c) #f])
    (expand form (environment '(chezscheme) '(parakeet)))
    #t))

(test "an unknown or repeated option fails to compile"
  (check (expands? '(decode-options [decoder 'ctc] [language "en"])))
  (check (not (expands? '(decode-options [decodr 'ctc]))))
  (check (not (expands? '(decode-options [beam-size 2] [beam-size 3]))))
  (check (not (expands? '(vad-options threshold))))
  (check (not (expands? '(sound-options [window]))))
  (check (not (expands? '(scene-options [latency 'low]))))
  (check (not (expands? '(decoder fastest))))
  (check (not (expands? '(diarization-latency instant)))))

(test "option values are checked when the options are made"
  (check-raises assertion-violation? (decode-options [decoder 'fastest]))
  (check-raises assertion-violation? (decode-options [beam-size 0]))
  (check-raises assertion-violation? (decode-options [language 'en]))
  (check-raises assertion-violation? (decode-options [normalized-ranking 1]))
  (check-raises assertion-violation? (vad-options [threshold 0]))
  (check-raises assertion-violation? (vad-options [threshold +nan.0]))
  (check-raises assertion-violation? (vad-options [run-gate 1]))
  (check-raises assertion-violation? (vad-options [mode 'all]))
  (check-raises assertion-violation? (word-filter [min-local-confidence 2]))
  (check-raises assertion-violation? (asr-stream-options [left-frames 70]))
  (check-raises assertion-violation?
    (asr-stream-options [preset 1] [left-frames 1] [chunk-frames 1] [right-frames 1]))
  (check-raises assertion-violation? (asr-stream-options [chunk-frames 0] [left-frames 1]
                                                         [right-frames 1]))
  (check-raises assertion-violation? (sound-options [window -1]))
  (check-raises assertion-violation? (sound-options [top-k -1]))
  (check-raises assertion-violation? (speaker-options [accept-threshold 2]))
  (check-raises assertion-violation? (speaker-options [accept-threshold 0]))
  (check-raises assertion-violation? (speaker-options [margin 0]))
  (check-raises assertion-violation? (scene-options [sound (vad-options)]))
  (check-raises assertion-violation? (scene-options [diarization-latency 4]))
  (check (vad-options [threshold 1/2] [min-pause 1] [mode (vad-mode segments)]
           [probabilities #f])))

(test "arguments are checked before parakeet.cpp sees them"
  (with-model "tdt_ctc-110m-q8_0.gguf"
    (lambda (m)
      (check-raises assertion-violation? (transcribe-pcm 'model speech-samples))
      (check-raises assertion-violation? (transcribe-pcm m (make-vector 4 0.0)))
      (check-raises assertion-violation? (transcribe-pcm m (make-bytevector 6 0)))
      (check-raises assertion-violation? (transcribe-pcm m speech-samples 0))
      (check-raises assertion-violation? (transcribe-pcm m speech-samples 16000.0))
      (check-raises assertion-violation? (transcribe-pcm m speech-samples (expt 2 31)))
      (check-raises assertion-violation? (transcribe-pcm m speech-samples 16000 '((decoder . ctc))))
      (check-raises assertion-violation? (transcribe-file m 'speech))
      (check-raises assertion-violation? (transcribe-file m "speech\x0;.wav"))
      (check-raises assertion-violation?
        (transcribe-file m speech-wav (decode-options [language "e\x0;n"])))
      (check-raises assertion-violation? (transcribe-file-json m speech-wav (decode-options) 'filter))
      (check-raises assertion-violation? (transcribe-pcm-batch m (list speech-samples)))
      (check-raises assertion-violation? (transcribe-pcm-batch m (vector speech-samples 'x)))
      (check-raises assertion-violation? (transcribe-pcm-nbest-json m speech-samples 5))
      (check-raises assertion-violation? (transcribe-pcm-nbest-json m speech-samples 0))
      (check-raises assertion-violation? (model-set-concurrency! m 0))
      (check-raises assertion-violation? (compile-grammar m 'rules))
      (check-raises assertion-violation? (open-asr-stream m (decode-options)))
      (check-raises assertion-violation?
        (transcribe-file-json m speech-wav (decode-options [language "de"])))
      (check-raises assertion-violation?
        (transcribe-file-vad-json m speech-wav #f (vad-options) (decode-options [beam-size 2])))
      (check-raises assertion-violation?
        (transcribe-pcm m speech-samples 16000 (decode-options [normalized-ranking #f])))
      (check-raises assertion-violation?
        (transcribe-pcm-nbest-json m speech-samples 1 16000 (decode-options [decoder 'ctc])))
      (check-raises assertion-violation?
        (open-grammar-stream m (compile-grammar m "root ::= \"a\"") (decode-options [decoder 'ctc])))
      (check-raises assertion-violation? (open-diarization-stream m 'fast))
      (check-raises assertion-violation? (open-scene-stream #f #f #f))
      (check-raises assertion-violation? (open-scene-stream m #f #f m (make-speaker-registry)))
      (check-raises assertion-violation? (load-model 'path))
      (check-raises assertion-violation? (load-model (model-path "x.gguf") 'asr))))
  (check-raises assertion-violation? (make-speaker-embedding (bytevector) #f #f))
  (check-raises assertion-violation? (make-speaker-embedding (make-bytevector 6 0) #f #f))
  (check-raises assertion-violation? (make-speaker-embedding (make-bytevector 8 0) 'family #f))
  (let ([r (make-speaker-registry)])
    (check-raises assertion-violation?
      (speaker-registry-add! r "" (make-speaker-embedding (make-bytevector 8 0) #f #f)))
    (check-raises assertion-violation? (speaker-registry-add! r "Ada" (make-bytevector 8 0)))
    (check-raises assertion-violation? (speaker-registry-strict-set! r 'yes))
    (close-speaker-registry r))
  (let ([m (load-model (model-path "tdt_ctc-110m-q8_0.gguf"))])
    (close-model m)
    (check-raises assertion-violation? (transcribe-pcm-batch m (vector)))
    (check-raises assertion-violation? (transcribe-pcm-batch-json m (vector))))
  (check-raises assertion-violation? (close-model 'model))
  (check-raises assertion-violation? (close-asr-stream 'stream)))

(test-exit)
