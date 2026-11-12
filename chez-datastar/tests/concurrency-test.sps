(import (chezscheme) (check) (support) (h2o) (datastar))

(define port (test-port 4))
(define threads 8)
(define events-per-thread 250)

(define streams (make-log))

(define server
  (start-server (list (http-listener "127.0.0.1" port))
    (list (make-host (list (format "127.0.0.1:~a" port))
            (list (route "/events"
                    (handler
                      (lambda (request)
                        (let ([stream (make-stream)])
                          (log! streams stream)
                          (sse-response stream))))))))))

(define (filler i) (make-string (* 300 (mod i 7)) #\x))

(define (send-all! stream t)
  (let loop ([i 0])
    (when (< i events-per-thread)
      (patch-elements stream (format "<div id=\"t~a-~a\">\n~a ~a ~a\n</div>" t i t i (filler i))
        (patch-elements-options [event-id (format "~a-~a" t i)]))
      (loop (+ i 1)))))

(define (expected-event t i)
  (format "event: datastar-patch-elements\nid: ~a-~a\ndata: elements <div id=\"t~a-~a\">\ndata: elements ~a ~a ~a\ndata: elements </div>"
    t i t i t i (filler i)))

;; Events end with a blank line, and no line inside these events is blank.
(define (split-events body)
  (let ([n (string-length body)])
    (let loop ([start 0] [i 0] [events '()])
      (cond
        [(>= (+ i 1) n) (check-equal? n start) (reverse events)]
        [(and (char=? (string-ref body i) #\newline) (char=? (string-ref body (+ i 1)) #\newline))
         (loop (+ i 2) (+ i 2) (cons (substring body start i) events))]
        [else (loop start (+ i 1) events)]))))

(define (event-thread event)
  (let* ([id-line (cadr (split-lines event))]
         [dash (string-index id-line #\-)])
    (string->number (substring id-line 4 dash))))

(test "events sent from many threads to one stream arrive whole and in each thread's order"
  (let* ([client (in-background (lambda () (curl "-N" (format "http://127.0.0.1:~a/events" port))))]
         [stream (and (wait-until 5000 (lambda () (pair? (log-items streams))))
                      (car (log-items streams)))]
         [senders (map (lambda (t) (in-background (lambda () (send-all! stream t)))) (iota threads))])
    (for-each (lambda (join) (join)) senders)
    (stream-close! stream)
    (let ([events (split-events (reply-body (client)))])
      (check-equal? (* threads events-per-thread) (length events))
      (for-each
        (lambda (t)
          (check-equal?
            (map (lambda (i) (expected-event t i)) (iota events-per-thread))
            (filter (lambda (event) (= t (event-thread event))) events)))
        (iota threads)))))

(server-stop! server)

(test-exit)
