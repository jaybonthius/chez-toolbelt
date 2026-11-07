(import (chezscheme) (json))

(define (now-ns)
  (let ([t (current-time 'time-monotonic)])
    (+ (* (time-second t) 1000000000) (time-nanosecond t))))

(define (allocated) (+ (bytes-allocated) (bytes-deallocated)))

(define (run-once op inner)
  (collect (collect-maximum-generation))
  (let ([a0 (allocated)] [t0 (now-ns)])
    (do ([i 0 (+ i 1)]) ((= i inner)) (op))
    (let ([t1 (now-ns)] [a1 (allocated)])
      (values (/ (- t1 t0) 1e6 inner) (/ (- a1 a0) inner)))))

(define (median xs)
  (let ([v (list->vector (sort < xs))])
    (vector-ref v (quotient (vector-length v) 2))))

(define (measure op inner)
  (run-once op inner)
  (run-once op inner)
  (let loop ([i 0] [times '()] [bytes '()])
    (if (= i 7)
        (values (median times) (median bytes))
        (let-values ([(ms b) (run-once op inner)])
          (loop (+ i 1) (cons ms times) (cons b bytes))))))

(define (file-bytes path) (call-with-port (open-file-input-port path) get-bytevector-all))

(define (report file operation inner op)
  (let-values ([(ms bytes) (measure op inner)])
    (if (= inner 1)
        (printf "| ~a | ~a | ~,1f ms | ~,1f MB |\n" file operation ms (/ bytes 1e6))
        (printf "| ~a | ~a | ~,2f µs | ~,1f KB |\n" file operation (* ms 1000) (/ bytes 1e3)))
    (flush-output-port)))

(let* ([args (command-line-arguments)] [dir (car args)])
  (printf "Median of 7 runs after 2 warm-up runs, each after a full collection; a run of signals.json is 2,000 operations, reported per operation.\n\n")
  (printf "| File | Operation | Median | Allocated |\n|---|---|---|---|\n")
  (for-each
    (lambda (path inner)
      (let* ([bv (file-bytes path)]
             [text (utf8->string bv)]
             [value (string->json text)]
             [file (path-last path)])
        (report file "string->json" inner (lambda () (string->json text)))
        (report file "bytevector->json" inner (lambda () (bytevector->json bv)))
        (report file "json->string" inner (lambda () (json->string value)))
        (report file "json->bytevector" inner (lambda () (json->bytevector value)))))
    (list (string-append dir "/canada.json") (string-append dir "/citm_catalog.json")
          (string-append dir "/twitter.json") (cadr args))
    '(1 1 1 2000)))
