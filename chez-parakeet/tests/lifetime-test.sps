(import (chezscheme) (parakeet) (only (parakeet native) live-handle-count free-unreachable-handles!)
        (check) (fixtures))

(define asr-model "tdt_ctc-110m-q8_0.gguf")
(define eou-model "realtime_eou_120m-v1-q8_0.gguf")
(define sound-model "ced-tiny-q8_0.gguf")

(define (repeat-samples samples times)
  (let* ([n (bytevector-length samples)] [out (make-bytevector (* n times))])
    (do ([i 0 (+ i 1)]) ((= i times) out)
      (bytevector-copy! samples 0 out (* i n) n))))

(define (spawn thunk)
  (let ([mutex (make-mutex)] [done (make-condition)] [outcome #f])
    (fork-thread
      (lambda ()
        (let ([result (guard (c [#t (cons 'raised c)]) (cons 'returned (thunk)))])
          (with-mutex mutex
            (set! outcome result)
            (condition-broadcast done)))))
    (lambda ()
      (with-mutex mutex
        (let wait () (unless outcome (condition-wait done mutex) (wait))))
      (if (eq? (car outcome) 'raised) (raise (cdr outcome)) (cdr outcome)))))

(define (full-collection)
  (collect (collect-maximum-generation))
  (free-unreachable-handles!))

(define (closed-error-names? h)
  (lambda (c)
    (and (assertion-violation? c) (message-condition? c) (irritants-condition? c)
         (equal? (format "~?" (condition-message c) (condition-irritants c))
                 (format "~s is closed" h)))))

(test "a closed handle refuses every use, and closing twice does nothing"
  (let ([m (load-model (model-path eou-model))])
    (let ([s (open-asr-stream m)] [g (compile-grammar m "root ::= \"a\"")])
      (close-asr-stream s)
      (close-grammar g)
      (close-model m)
      (close-model m)
      (check (not (model-open? m)))
      (check-raises assertion-violation? (asr-stream-feed s (bytevector)))
      (check-raises assertion-violation? (asr-stream-finish-json s))
      (check-raises assertion-violation? (asr-stream-reset! s))
      (check-raises assertion-violation? (transcribe-pcm-grammar m g (bytevector))))
    (for-each
      (lambda (use) (check-raises assertion-violation? (use m)))
      (list (lambda (m) (transcribe-pcm m speech-samples))
            (lambda (m) (transcribe-file m speech-wav))
            (lambda (m) (transcribe-file-json m speech-wav))
            (lambda (m) (transcribe-pcm-batch m (vector speech-samples)))
            (lambda (m) (transcribe-pcm-logits m speech-samples))
            (lambda (m) (vad-pcm-json m speech-samples))
            (lambda (m) (open-asr-stream m))
            (lambda (m) (open-vad-stream m))
            (lambda (m) (compile-grammar m "root ::= \"a\""))
            (lambda (m) (model-stream-kind m))
            (lambda (m) (model-set-concurrency! m 2))
            (lambda (m) (embed-speaker m speech-samples))
            (lambda (m) (model-speaker-dimension m))
            (lambda (m) (model-sound-labels m))
            (lambda (m) (open-sound-stream m))
            (lambda (m) (open-diarization-stream m))
            (lambda (m) (diarize-pcm-json m speech-samples))
            (lambda (m) (open-scene-stream m #f #f))))))

(test "closing a parent closes its open children first"
  (full-collection)
  (let* ([before (live-handle-count)] [m (load-model (model-path eou-model))])
    (let* ([s (open-asr-stream m)]
           [g (compile-grammar m "root ::= \"a\"")]
           [gs (open-grammar-stream m g)])
      (asr-stream-feed s (silence 1600))
      (close-model m)
      (check (not (model-open? m)))
      (check-equal? before (live-handle-count))
      (check-raises (closed-error-names? s) (asr-stream-feed s (silence 1600)))
      (check-raises (closed-error-names? gs) (grammar-stream-feed-json gs (silence 1600)))
      (check-raises (closed-error-names? g) (open-grammar-stream m g))
      (close-asr-stream s)
      (close-grammar g)))
  (let* ([m (load-model (model-path sound-model))] [s (open-scene-stream #f #f m)])
    (close-model m)
    (check-raises (closed-error-names? s) (scene-stream-feed-json s (silence 1600)))))

(test "a dropped child does not keep its parent open"
  (let ([m (load-model (model-path eou-model))])
    (transcribe-pcm-grammar m (compile-grammar m "root ::= \"a\"") (silence 1600))
    (open-asr-stream m)
    (close-model m)
    (check (not (model-open? m)))))

(test "a handle passed twice is held once"
  (with-model sound-model
    (lambda (m)
      (let ([r (make-speaker-registry)])
        (check-raises parakeet-error? (diarize-named-pcm-json m m r speech-samples))
        (check-raises parakeet-error? (transcribe-and-diarize m m speech-samples))
        (check-raises parakeet-error? (open-scene-stream m m m))
        (close-speaker-registry r))))
  (with-model asr-model
    (lambda (m)
      (check-raises parakeet-error? (transcribe-and-diarize-json m m speech-samples)))))

(test "unreachable models, streams and grammars are freed, children first"
  (full-collection)
  (let ([before (live-handle-count)])
    (do ([i 0 (+ i 1)]) ((= i 10))
      (let* ([m (load-model (model-path sound-model))]
             [s (open-sound-stream m)])
        (sound-stream-feed s (silence 16000))))
    (let ([m (load-model (model-path eou-model))])
      (do ([i 0 (+ i 1)]) ((= i 5))
        (asr-stream-feed (open-asr-stream m) (silence 1600))
        (compile-grammar m "root ::= \"a\"")))
    (check (> (live-handle-count) before))
    (full-collection)
    (full-collection)
    (check-equal? before (live-handle-count))))

(test "closing waits for calls in flight, and later calls are refused"
  (let* ([m (load-model (model-path asr-model))]
         [long (repeat-samples speech-samples 4)]
         [started (make-condition)]
         [mutex (make-mutex)]
         [running? #f]
         [result (spawn (lambda ()
                          (with-mutex mutex (set! running? #t) (condition-broadcast started))
                          (transcribe-pcm m long)))])
    (with-mutex mutex (let wait () (unless running? (condition-wait started mutex) (wait))))
    (sleep (make-time 'time-duration 50000000 0))
    (close-model m)
    (check (string? (result)))
    (check (> (string-length (result)) 200))
    (check-raises assertion-violation? (transcribe-pcm m speech-samples))))

(test "calls parakeet.cpp forbids at once on one object are serialised"
  (with-model eou-model
    (lambda (m)
      (let* ([s (open-asr-stream m)]
             [feeders (map (lambda (i)
                             (spawn (lambda ()
                                      (do ([j 0 (+ j 1)]) ((= j 40))
                                        (asr-stream-feed s (sub-samples streaming-samples
                                                             (* 1600 j) (* 1600 (+ j 1))))))))
                           '(0 1 2 3))])
        (for-each (lambda (wait) (wait)) feeders)
        (check (string? (asr-stream-finish s)))
        (close-asr-stream s))))
  (with-model "parakeet-bundle-small.gguf" "diar"
    (lambda (d)
      (let* ([expected (diarize-pcm-json d speech-samples)]
             [workers (map (lambda (i) (spawn (lambda () (diarize-pcm-json d speech-samples))))
                        '(0 1 2 3))])
        (for-each (lambda (wait) (check-equal? expected (wait))) workers))))
  (with-model asr-model
    (lambda (m)
      (model-set-concurrency! m 2 2)
      (let ([workers (map (lambda (i) (spawn (lambda () (transcribe-pcm m speech-samples))))
                       '(0 1 2 3 4 5))]
            [setter (spawn (lambda () (model-set-concurrency! m 3 2)))])
        (for-each (lambda (wait) (check-equal? expected-transcript (wait))) workers)
        (check-equal? 3 (setter))))))

;; A call that kept its thread active would hold every collection, and so
;; every other thread, until it returned.
(test "other threads collect and run while a model computes"
  (with-model asr-model
    (lambda (m)
      (let* ([long (repeat-samples speech-samples 3)]
             [mutex (make-mutex)]
             [done? #f]
             [worker (spawn (lambda ()
                              (let ([text (transcribe-pcm m long)])
                                (with-mutex mutex (set! done? #t))
                                text)))])
        (let loop ([last (current-time 'time-monotonic)] [longest 0] [collections 0])
          (if (with-mutex mutex done?)
              (begin
                (worker)
                (printf "longest pause beside a running model: ~a ms over ~a collections\n"
                  (exact->inexact longest) collections)
                (check (> collections 5))
                (check (< longest 50)))
              (begin
                (make-vector 100000 0)
                (collect-rendezvous)
                (let* ([now (current-time 'time-monotonic)]
                       [elapsed (time-difference now last)]
                       [gap (+ (* 1000 (time-second elapsed)) (/ (time-nanosecond elapsed) 1e6))])
                  (loop now (max longest gap) (+ collections 1))))))))))

(test-exit)
