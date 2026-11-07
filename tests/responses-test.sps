(import (chezscheme) (check) (h2o) (haml) (json) (datastar))

(define (body r) (utf8->string (response-body r)))

(define (raises-naming? who option)
  (lambda (c)
    (and (assertion-violation? c) (who-condition? c) (eq? who (condition-who c))
         (irritants-condition? c) (equal? (list option) (condition-irritants c)))))

(test "elements-response with defaults"
  (let ([r (elements-response "<p id=\"p\">hi</p>")])
    (check-equal? 200 (response-status r))
    (check-equal? '(("content-type" . "text/html; charset=utf-8")) (response-headers r))
    (check-equal? "<p id=\"p\">hi</p>" (body r))))

(test "elements-response with every option it can carry"
  (let ([r (elements-response (haml (:circle ([id "c"] [r 2])))
             (patch-elements-options [selector "#vis"] [mode (patch-mode append)]
               [namespace (element-namespace svg)] [use-view-transition #t]))])
    (check-equal?
      '(("content-type" . "text/html; charset=utf-8") ("datastar-selector" . "#vis")
        ("datastar-mode" . "append") ("datastar-namespace" . "svg")
        ("datastar-use-view-transition" . "true"))
      (response-headers r))
    (check-equal? "<circle id=\"c\" r=\"2\"></circle>" (body r))))

(test "elements-response takes a bytevector, and #f for removal"
  (check-equal? "<p id=\"é\"></p>\n" (body (elements-response (string->utf8 "<p id=\"é\"></p>\n"))))
  (let ([r (elements-response #f
             (patch-elements-options [selector "#gone"] [mode (patch-mode remove)]))])
    (check-equal? "" (body r))
    (check-equal?
      '(("content-type" . "text/html; charset=utf-8") ("datastar-selector" . "#gone")
        ("datastar-mode" . "remove"))
      (response-headers r))))

(test "signals-response"
  (let ([r (signals-response '(("count" . 3) ("user" ("name" . "Ada"))))])
    (check-equal? '(("content-type" . "application/json")) (response-headers r))
    (check-equal? "{\"count\":3,\"user\":{\"name\":\"Ada\"}}" (body r)))
  (let ([r (signals-response "{\"a\":\n1}" (patch-signals-options [only-if-missing #t]))])
    (check-equal? '(("content-type" . "application/json") ("datastar-only-if-missing" . "true"))
      (response-headers r))
    (check-equal? "{\"a\":\n1}" (body r))))

(test "script-response"
  (let ([r (script-response "console.log('</script>')")])
    (check-equal? '(("content-type" . "text/javascript; charset=utf-8")) (response-headers r))
    (check-equal? "console.log('</script>')" (body r)))
  (let ([r (script-response "go()"
             (execute-script-options [attributes '(("type" . "module") ("data-x" . "a\"b"))]))])
    (check-equal?
      '(("content-type" . "text/javascript; charset=utf-8")
        ("datastar-script-attributes" . "{\"type\":\"module\",\"data-x\":\"a\\\"b\"}"))
      (response-headers r))))

(test "an option a one-shot response cannot carry raises, naming the response and the option"
  (check-raises (raises-naming? 'elements-response 'event-id)
    (elements-response "<p id=\"p\"></p>" (patch-elements-options [event-id "1"])))
  (check-raises (raises-naming? 'elements-response 'retry-duration)
    (elements-response "<p id=\"p\"></p>" (patch-elements-options [retry-duration 1000])))
  (check-raises (raises-naming? 'elements-response 'view-transition-selector)
    (elements-response "<p id=\"p\"></p>"
      (patch-elements-options [use-view-transition #t] [view-transition-selector "#m"])))
  (check-raises (raises-naming? 'signals-response 'event-id)
    (signals-response '() (patch-signals-options [event-id "1"])))
  (check-raises (raises-naming? 'signals-response 'retry-duration)
    (signals-response '() (patch-signals-options [retry-duration 5])))
  (check-raises (raises-naming? 'script-response 'auto-remove)
    (script-response "x()" (execute-script-options [auto-remove #f])))
  (check-raises (raises-naming? 'script-response 'event-id)
    (script-response "x()" (execute-script-options [event-id "1"])))
  (check-raises (raises-naming? 'script-response 'retry-duration)
    (script-response "x()" (execute-script-options [retry-duration 1]))))

(test "a header value outside printable ASCII raises, naming the response"
  (check-raises (lambda (c) (and (assertion-violation? c) (eq? 'elements-response (condition-who c))))
    (elements-response "<p></p>" (patch-elements-options [selector "a\x0;b"])))
  (check-raises (lambda (c) (and (assertion-violation? c) (eq? 'elements-response (condition-who c))))
    (elements-response "<p></p>" (patch-elements-options [selector "a\x1;b"])))
  (check-raises assertion-violation?
    (elements-response "<p></p>" (patch-elements-options [selector "#café"])))
  (check-raises assertion-violation?
    (script-response "x()" (execute-script-options [attributes '(("title" . "café"))]))))

(test "bad arguments raise"
  (check-raises assertion-violation? (elements-response 42))
  (check-raises assertion-violation? (elements-response "x" (patch-signals-options)))
  (check-raises assertion-violation? (signals-response 'x))
  (check-raises json-error? (signals-response "{\"a\":1}; alert(1)"))
  (check-raises json-error? (signals-response ""))
  (check-raises assertion-violation? (signals-response "\"text\""))
  (check-raises assertion-violation? (signals-response '() (patch-elements-options)))
  (check-raises assertion-violation? (script-response #f))
  (check-raises assertion-violation? (script-response "x" (fetch-options))))

(test-exit)
