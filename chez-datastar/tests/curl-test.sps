(import (chezscheme) (check) (support) (h2o) (haml) (datastar))

(define port (test-port 5))
(define base (format "http://127.0.0.1:~a" port))

(define streams (make-log))

(define (todo-item title) (haml (:li ([id (string-append "todo-" title)]) title)))

(define (send-rest! stream)
  (patch-signals stream '(("count" . 2) ("busy" . #f)))
  (patch-elements stream (haml (:li ([id "todo-c"]) "c"))
    (patch-elements-options [selector "#todos"] [mode (patch-mode append)]))
  (execute-script stream "console.log('done')")
  (patch-elements stream #f
    (patch-elements-options [selector "#spinner"] [mode (patch-mode remove)] [event-id "last"]))
  (stream-close! stream))

(define (todos request)
  (let ([stream (make-stream)])
    (patch-elements stream (haml (:ul ([id "todos"]) (map todo-item '("a" "b")))))
    (log! streams stream)
    (sse-response stream)))

(define server
  (start-server (list (http-listener "127.0.0.1" port))
    (list (make-host (list (format "127.0.0.1:~a" port))
            (list (route "/plain" (handler todos))
                  (route "/compressed" (handler todos) (compress)))))))

(define first-event
  "event: datastar-patch-elements\ndata: elements <ul id=\"todos\"><li id=\"todo-a\">a</li><li id=\"todo-b\">b</li></ul>\n\n")

(define expected
  (string-append first-event
    "event: datastar-patch-signals\ndata: signals {\"count\":2,\"busy\":false}\n\n"
    "event: datastar-patch-elements\ndata: selector #todos\ndata: mode append\ndata: elements <li id=\"todo-c\">c</li>\n\n"
    "event: datastar-patch-elements\ndata: selector body\ndata: mode append\ndata: elements <script data-effect=\"el.remove()\">console.log('done')</script>\n\n"
    "event: datastar-patch-elements\nid: last\ndata: selector #spinner\ndata: mode remove\n\n"))

;; The rest are sent only once the first has reached curl, which shows that
;; each event is flushed as it is sent.
(define (stream-through path . curl-arguments)
  (let* ([seen (length (log-items streams))]
         [out (scratch-file "partial")]
         [client (in-background
                   (lambda () (apply curl-into out "-N" (string-append base path) curl-arguments)))])
    (check (wait-until 5000 (lambda () (> (length (log-items streams)) seen))))
    (let ([stream (list-ref (log-items streams) seen)])
      (check (wait-until 5000 (lambda () (string=? first-event (read-text out)))))
      (send-rest! stream)
      (let ([reply (client)])
        (values (reply-body reply) (reply-header reply "content-encoding"))))))

(test "a handler streams several events to curl"
  (let-values ([(body encoding) (stream-through "/plain")])
    (check-equal? expected body)
    (check-equal? #f encoding)))

(for-each
  (lambda (encoding)
    (test (format "the same events arrive compressed with ~a" encoding)
      (let-values ([(body sent-encoding)
                    (stream-through "/compressed" "--compressed" "-H"
                      (string-append "accept-encoding: " encoding))])
        (check-equal? expected body)
        (check-equal? encoding sent-encoding))))
  '("gzip" "br" "zstd"))

(server-stop! server)

(test-exit)
