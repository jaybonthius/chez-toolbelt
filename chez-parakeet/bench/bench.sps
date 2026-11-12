(import (chezscheme) (parakeet) (fixtures))

(define (now-ms)
  (let ([t (current-time 'time-monotonic)])
    (+ (* 1000.0 (time-second t)) (/ (time-nanosecond t) 1e6))))

(define (time-ms thunk)
  (let ([start (now-ms)])
    (thunk)
    (- (now-ms) start)))

(define (spawn thunk)
  (let ([mutex (make-mutex)] [done (make-condition)] [finished? #f])
    (fork-thread
      (lambda ()
        (thunk)
        (with-mutex mutex (set! finished? #t) (condition-broadcast done))))
    (lambda ()
      (with-mutex mutex (let wait () (unless finished? (condition-wait done mutex) (wait)))))))

(define seconds (/ (float-count speech-samples) 16000.0))

(with-model "tdt_ctc-110m-q8_0.gguf"
  (lambda (m)
    (transcribe-pcm m speech-samples)
    (let ([times (map (lambda (i) (time-ms (lambda () (transcribe-pcm m speech-samples))))
                   (iota 10))])
      (printf "Scheme  transcribe ~,2f s of speech: best ~,1f ms, mean ~,1f ms\n" seconds
        (apply min times) (/ (apply + times) 10)))

    (let ([sequential (time-ms (lambda ()
                                 (do ([i 0 (+ i 1)]) ((= i 8)) (transcribe-pcm m speech-samples))))])
      (model-set-concurrency! m 4 4)
      (let ([parallel (time-ms (lambda ()
                                 (for-each (lambda (wait) (wait))
                                   (map (lambda (i)
                                          (spawn (lambda ()
                                                   (do ([j 0 (+ j 1)]) ((= j 2))
                                                     (transcribe-pcm m speech-samples)))))
                                        (iota 4)))))])
        (printf "Scheme  8 transcriptions: ~d ms on one thread, ~d ms on 4 threads with 4 backends\n"
          (exact (round sequential)) (exact (round parallel))))
      (model-set-concurrency! m 1))

    (let ([mutex (make-mutex)] [done? #f])
      (let ([worker (spawn (lambda ()
                             (do ([i 0 (+ i 1)]) ((= i 5)) (transcribe-pcm m speech-samples))
                             (with-mutex mutex (set! done? #t))))])
        (let loop ([last (now-ms)] [longest 0.0] [ticks 0])
          (if (with-mutex mutex done?)
              (begin
                (worker)
                (printf "Scheme  10 ms timer beside 5 transcriptions: ~a ticks, longest gap ~,1f ms\n"
                  ticks longest))
              (begin
                (sleep (make-time 'time-duration 10000000 0))
                (make-vector 10000 0)
                (collect-rendezvous)
                (let ([now (now-ms)])
                  (loop now (max longest (- now last)) (+ ticks 1))))))))))

(with-model "realtime_eou_120m-v1-q8_0.gguf"
  (lambda (m)
    (let ([pieces (let loop ([at 0] [pieces '()])
                    (if (>= at (float-count speech-samples))
                        (reverse pieces)
                        (let ([end (min (float-count speech-samples) (+ at 1600))])
                          (loop end (cons (sub-samples speech-samples at end) pieces)))))])
      (let ([best (apply min
                    (map (lambda (round)
                           (let ([s (open-asr-stream m)])
                             (let ([elapsed (time-ms
                                              (lambda ()
                                                (for-each (lambda (piece) (asr-stream-feed s piece))
                                                  pieces)
                                                (asr-stream-finish s)))])
                               (close-asr-stream s)
                               (/ elapsed (+ 1 (length pieces))))))
                         (iota 3)))])
        (printf "Scheme  stream 100 ms pieces: best ~,2f ms per piece\n" best)))
    (let ([s (open-asr-stream m)] [empty (bytevector)])
      (let ([elapsed (time-ms (lambda ()
                                (do ([i 0 (+ i 1)]) ((= i 100000)) (asr-stream-feed s empty))))])
        (printf "Scheme  empty stream feed: ~,2f us per call\n" (/ elapsed 100)))
      (close-asr-stream s))))
