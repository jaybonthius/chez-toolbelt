(import (chezscheme) (check) (support) (h2o) (json) (datastar))

(define port (test-port 3))
(define base (format "http://127.0.0.1:~a" port))

(define (echo request)
  (guard (c [(json-error? c) (response 400 '() (condition-message c))])
    (response 200 '(("content-type" . "application/json")) (json->string (read-signals request)))))

(define (datastar request)
  (response 200 '() (if (datastar-request? request) "yes" "no")))

(define server
  (start-server (list (http-listener "127.0.0.1" port))
    (list (make-host (list (format "127.0.0.1:~a" port))
            (list (route "/echo" (handler echo)) (route "/datastar" (handler datastar)))))
    (server-options [max-request-body-size (* 4 1024 1024)])))

(define signals "{\"count\":3,\"user\":{\"name\":\"Ada\",\"tags\":[\"a\",\"é\"]},\"on\":true}")

(test "GET reads the datastar query parameter"
  (let ([r (curl "--get" "--data-urlencode" (string-append "datastar=" signals)
             "--data-urlencode" "other=1" (string-append base "/echo"))])
    (check-equal? 200 (reply-status r))
    (check-equal? signals (reply-body r)))
  (let ([r (curl (string-append base "/echo?datastar=%7B%22a+b%22%3A%22c+d%22%7D"))])
    (check-equal? "{\"a b\":\"c d\"}" (reply-body r))))

(test "DELETE reads the datastar query parameter, as the client sends no body with it"
  (let ([r (curl "-X" "DELETE" (string-append base "/echo?datastar=%7B%22x%22%3A1%7D"))])
    (check-equal? 200 (reply-status r))
    (check-equal? "{\"x\":1}" (reply-body r))))

(test "POST, PUT, PATCH and QUERY read the body"
  (for-each
    (lambda (method)
      (let ([r (curl "-X" method "-H" "content-type: application/json" "--data-binary" signals
                 (string-append base "/echo?datastar=%7B%7D"))])
        (check-equal? 200 (reply-status r))
        (check-equal? signals (reply-body r))))
    '("POST" "PUT" "PATCH" "QUERY")))

(test "a body is read even when the same signals are too large for a request line"
  (let* ([big (make-string (* 1024 1024) #\x)]
         [text (string-append "{\"big\":\"" big "\"}")]
         [file (scratch-file "big.json")]
         [config (scratch-file "curl-config")])
    (write-file! file (string->utf8 text))
    (write-file! config
      (string->utf8 (format "url = \"~a/echo?datastar=~a\"\n" base (percent-encode text))))
    (check-equal? 0 (reply-status (curl "-K" config)))
    (let ([r (curl "-H" "content-type: application/json" "--data-binary" (string-append "@" file)
               (string-append base "/echo"))])
      (check-equal? 200 (reply-status r))
      (check-equal? text (reply-body r)))))

(test "missing signals raise the json condition"
  (check-equal? 400 (reply-status (curl (string-append base "/echo"))))
  (check-equal? 400 (reply-status (curl (string-append base "/echo?other=1"))))
  (check-equal? 400 (reply-status (curl "-X" "POST" (string-append base "/echo"))))
  (check-equal? 400 (reply-status (curl "-X" "DELETE" (string-append base "/echo")))))

(test "invalid signals raise the json condition"
  (check-equal? 400 (reply-status (curl (string-append base "/echo?datastar=%7Bx"))))
  (check-equal? 400 (reply-status (curl "--data-binary" "{\"a\":1,}" (string-append base "/echo"))))
  (check-equal? 400 (reply-status (curl "--data-binary" "{\"a\":1}{" (string-append base "/echo")))))

(test "datastar-request? looks for Datastar-Request: true"
  (check-equal? "yes" (reply-body (curl "-H" "Datastar-Request: true" (string-append base "/datastar"))))
  (check-equal? "yes" (reply-body (curl "-H" "datastar-request: true" (string-append base "/datastar"))))
  (check-equal? "no" (reply-body (curl "-H" "Datastar-Request: false" (string-append base "/datastar"))))
  (check-equal? "no" (reply-body (curl (string-append base "/datastar")))))

(test "bad arguments raise"
  (check-raises assertion-violation? (read-signals 'request))
  (check-raises assertion-violation? (datastar-request? "request")))

(server-stop! server)

(test-exit)
