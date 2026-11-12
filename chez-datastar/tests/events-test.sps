(import (chezscheme) (check) (support) (h2o) (haml) (json) (datastar))

(define port (test-port 1))

(define senders-mutex (make-mutex))
(define senders (make-eqv-hashtable))
(define sender-count 0)

;; Each check registers what to send and fetches it, so that the bytes
;; compared are those a client receives.
(define (fetch send)
  (let ([n (with-mutex senders-mutex
             (set! sender-count (+ sender-count 1))
             (hashtable-set! senders sender-count send)
             sender-count)])
    (curl (format "http://127.0.0.1:~a/send?~a" port n))))

(define (sent send) (reply-body (fetch send)))

(define (send request)
  (let ([send (with-mutex senders-mutex
                (hashtable-ref senders (string->number (request-query request)) #f))]
        [stream (make-stream)])
    (send stream)
    (stream-close! stream)
    (sse-response stream '(("x-test" . "yes")))))

(define server
  (start-server (list (http-listener "127.0.0.1" port))
    (list (make-host (list (format "127.0.0.1:~a" port))
            (list (route "/send" (handler send)))))))

(define (lines . strings) (apply string-append (map (lambda (s) (string-append s "\n")) strings)))

(test "sse-response sends the event-stream headers and the extra ones"
  (let ([r (fetch (lambda (s) (void)))])
    (check-equal? 200 (reply-status r))
    (check-equal? "text/event-stream" (reply-header r "content-type"))
    (check-equal? "no-cache" (reply-header r "cache-control"))
    (check-equal? "yes" (reply-header r "x-test"))
    (check-equal? "keep-alive" (reply-header r "connection"))
    (check-equal? "" (reply-body r))))

(test "patch-elements with defaults"
  (check-equal?
    (lines "event: datastar-patch-elements" "data: elements <div id=\"a\">x</div>" "")
    (sent (lambda (s) (patch-elements s "<div id=\"a\">x</div>")))))

(test "patch-elements with every option"
  (check-equal?
    (lines "event: datastar-patch-elements" "id: 123" "retry: 2000"
      "data: selector #feed" "data: mode inner" "data: useViewTransition true"
      "data: viewTransitionSelector #main" "data: namespace svg"
      "data: elements <circle id=\"c\"></circle>" "")
    (sent (lambda (s)
            (patch-elements s "<circle id=\"c\"></circle>"
              (patch-elements-options [selector "#feed"] [mode (patch-mode inner)]
                [use-view-transition #t] [view-transition-selector "#main"]
                [namespace (element-namespace svg)] [event-id "123"] [retry-duration 2000]))))))

(test "patch-elements writes only what differs from the defaults"
  (check-equal?
    (lines "event: datastar-patch-elements" "data: elements <p id=\"p\"></p>" "")
    (sent (lambda (s)
            (patch-elements s "<p id=\"p\"></p>"
              (patch-elements-options [selector #f] [mode (patch-mode outer)]
                [namespace (element-namespace html)] [use-view-transition #f] [event-id #f]
                [retry-duration 1000])))))
  (check-equal?
    (lines "event: datastar-patch-elements" "retry: 0" "data: mode append"
      "data: namespace mathml" "data: elements <mi id=\"m\">x</mi>" "")
    (sent (lambda (s)
            (patch-elements s "<mi id=\"m\">x</mi>"
              (patch-elements-options [mode (patch-mode append)]
                [namespace (element-namespace mathml)] [retry-duration 0]))))))

(test "an empty event id is written, which resets the client's last id"
  (check-equal?
    (lines "event: datastar-patch-elements" "id: " "data: elements <p id=\"p\"></p>" "")
    (sent (lambda (s) (patch-elements s "<p id=\"p\"></p>" (patch-elements-options [event-id ""]))))))

(test "view-transition-selector is written only with use-view-transition"
  (check-equal?
    (lines "event: datastar-patch-elements" "data: elements <p id=\"p\"></p>" "")
    (sent (lambda (s)
            (patch-elements s "<p id=\"p\"></p>"
              (patch-elements-options [use-view-transition #f] [view-transition-selector "#m"]))))))

(test "an event id may hold printable ASCII and tabs"
  (check-equal?
    (lines "event: datastar-patch-signals" "id: a\tb ~!" "data: signals {}" "")
    (sent (lambda (s) (patch-signals s '() (patch-signals-options [event-id "a\tb ~!"]))))))

(test "removing elements sends no elements line"
  (check-equal?
    (lines "event: datastar-patch-elements" "data: selector #feed, #other" "data: mode remove" "")
    (sent (lambda (s)
            (patch-elements s #f
              (patch-elements-options [selector "#feed, #other"] [mode (patch-mode remove)]))))))

(test "each line of the elements is a data line, whatever ends it"
  (let ([expected (lines "event: datastar-patch-elements" "data: elements <div id=\"a\">"
                    "data: elements   <span>1</span>" "data: elements </div>" "")])
    (for-each
      (lambda (end)
        (check-equal? expected
          (sent (lambda (s)
                  (patch-elements s
                    (string-append "<div id=\"a\">" end "  <span>1</span>" end "</div>"))))))
      '("\n" "\r" "\r\n")))
  (check-equal?
    (lines "event: datastar-patch-elements" "data: elements <p id=\"a\"></p>" "")
    (sent (lambda (s) (patch-elements s "<p id=\"a\"></p>\n"))))
  (check-equal?
    (lines "event: datastar-patch-elements" "data: elements <p id=\"a\">" "data: elements "
      "data: elements " "data: elements </p>" "")
    (sent (lambda (s) (patch-elements s "<p id=\"a\">\r\n\n\r</p>"))))
  (check-equal?
    (lines "event: datastar-patch-elements" "")
    (sent (lambda (s) (patch-elements s "")))))

(test "elements may be html, a bytevector or text outside ASCII"
  (check-equal?
    (lines "event: datastar-patch-elements"
      "data: elements <ul id=\"list\"><li>a &lt; b</li><li>café</li></ul>" "")
    (sent (lambda (s)
            (patch-elements s (haml (:ul ([id "list"]) (:li "a < b") (:li "café")))))))
  (check-equal?
    (lines "event: datastar-patch-elements" "data: elements <p id=\"p\">é</p>"
      "data: elements <p id=\"q\"></p>" "")
    (sent (lambda (s) (patch-elements s (string->utf8 "<p id=\"p\">é</p>\r\n<p id=\"q\"></p>")))))
  (check-equal?
    (lines "event: datastar-patch-elements" "data: elements <p id=\"x\">\x2028;日本</p>" "")
    (sent (lambda (s) (patch-elements s "<p id=\"x\">\x2028;日本</p>")))))

(test "patch-signals with defaults"
  (check-equal?
    (lines "event: datastar-patch-signals"
      "data: signals {\"output\":\"Patched\",\"show\":true,\"user\":{\"name\":\"\",\"n\":1.5}}" "")
    (sent (lambda (s)
            (patch-signals s '(("output" . "Patched") (show . #t) ("user" ("name" . "") ("n" . 1.5)))))))
  (check-equal?
    (lines "event: datastar-patch-signals" "data: signals {}" "")
    (sent (lambda (s) (patch-signals s '())))))

(test "patch-signals with every option"
  (check-equal?
    (lines "event: datastar-patch-signals" "id: 7" "retry: 3000" "data: onlyIfMissing true"
      "data: signals {\"one\":null,\"two\":{\"alpha\":null}}" "")
    (sent (lambda (s)
            (patch-signals s '(("one" . null) ("two" ("alpha" . null)))
              (patch-signals-options [only-if-missing #t] [event-id "7"] [retry-duration 3000])))))
  (check-equal?
    (lines "event: datastar-patch-signals" "data: signals {\"a\":1}" "")
    (sent (lambda (s)
            (patch-signals s '(("a" . 1))
              (patch-signals-options [only-if-missing #f] [retry-duration 1000]))))))

(test "a JSON value is written on one line, its newlines escaped"
  (check-equal?
    (lines "event: datastar-patch-signals" "data: signals {\"one\":\"first\\n signal\\r\"}" "")
    (sent (lambda (s) (patch-signals s '(("one" . "first\n signal\r")))))))

(test "a string of JSON is written one data line per line"
  (check-equal?
    (lines "event: datastar-patch-signals" "data: signals {" "data: signals \"one\": 1,"
      "data: signals \"two\":  " "data: signals 2}" "")
    (sent (lambda (s) (patch-signals s "{\n\"one\": 1,\r\n\"two\":  \r2}")))))

(test "execute-script with defaults"
  (check-equal?
    (lines "event: datastar-patch-elements" "data: selector body" "data: mode append"
      "data: elements <script data-effect=\"el.remove()\">console.log('hello');</script>" "")
    (sent (lambda (s) (execute-script s "console.log('hello');")))))

(test "execute-script with every option"
  (check-equal?
    (lines "event: datastar-patch-elements" "id: event1" "retry: 2000" "data: selector body"
      "data: mode append"
      "data: elements <script type=\"module\" data-x=\"a &quot;b&quot; &amp; &lt;c&gt;\">go();</script>"
      "")
    (sent (lambda (s)
            (execute-script s "go();"
              (execute-script-options [auto-remove #f]
                [attributes '(("type" . "module") ("data-x" . "a \"b\" & <c>"))]
                [event-id "event1"] [retry-duration 2000])))))
  (check-equal?
    (lines "event: datastar-patch-elements" "data: selector body" "data: mode append"
      "data: elements <script blocking=\"render\" data-effect=\"el.remove()\">x()</script>" "")
    (sent (lambda (s)
            (execute-script s "x()"
              (execute-script-options [attributes '(("blocking" . "render"))]))))))

(test "a multiline script is one data line per line"
  (check-equal?
    (lines "event: datastar-patch-elements" "data: selector body" "data: mode append"
      "data: elements <script data-effect=\"el.remove()\">if (true) {"
      "data: elements   console.log('hello');" "data: elements }</script>" "")
    (sent (lambda (s) (execute-script s "if (true) {\r\n  console.log('hello');\r}")))))

(define config
  (call-with-port (open-file-input-port "tests/datastar/sdk/datastar-sdk-config-v1.json") json-read))

(define (config-ref . path)
  (fold-left (lambda (value key) (cdr (assoc key value))) config path))

(define (enum-values name)
  (map (lambda (entry) (cdr (assoc "value" entry)))
       (vector->list (config-ref "enums" name "values"))))

(define (data-literals text)
  (map (lambda (line)
         (let ([rest (substring line 6 (string-length line))])
           (substring rest 0 (string-index rest #\space))))
       (filter (lambda (line) (and (> (string-length line) 6) (string=? "data: " (substring line 0 6))))
               (split-lines text))))

(test "defaults and enumerations match datastar-sdk-config-v1.json"
  (check-equal? "datastar" (config-ref "datastarKey"))
  (check-equal? '("datastar-patch-elements" "datastar-patch-signals") (enum-values "EventType"))
  (check-equal? "outer" (config-ref "enums" "ElementPatchMode" "default"))
  (check-equal? "html" (config-ref "enums" "Namespace" "default"))
  (check-equal? #f (config-ref "defaults" "booleans" "elementsUseViewTransitions"))
  (check-equal? #f (config-ref "defaults" "booleans" "patchSignalsOnlyIfMissing"))
  (check-equal? 1000 (config-ref "defaults" "durations" "sseRetryDuration"))
  (for-each
    (lambda (mode)
      (check-equal?
        (if (string=? mode "outer")
            (lines "event: datastar-patch-elements" "data: selector #t" "data: elements <b></b>" "")
            (lines "event: datastar-patch-elements" "data: selector #t"
              (string-append "data: mode " mode) "data: elements <b></b>" ""))
        (sent (lambda (s)
                (patch-elements s "<b></b>"
                  (patch-elements-options [selector "#t"] [mode (string->symbol mode)]))))))
    (enum-values "ElementPatchMode"))
  (check-equal? 8 (length (enum-values "ElementPatchMode")))
  (for-each
    (lambda (namespace)
      (check-equal?
        (if (string=? namespace "html")
            (lines "event: datastar-patch-elements" "data: elements <b id=\"b\"></b>" "")
            (lines "event: datastar-patch-elements" (string-append "data: namespace " namespace)
              "data: elements <b id=\"b\"></b>" ""))
        (sent (lambda (s)
                (patch-elements s "<b id=\"b\"></b>"
                  (patch-elements-options [namespace (string->symbol namespace)]))))))
    (enum-values "Namespace"))
  (check-equal? '("html" "svg" "mathml") (enum-values "Namespace"))
  ;; The specification writes viewTransitionSelector, which the list of
  ;; literals in the configuration lacks.
  (check-equal? '("selector" "mode" "elements" "useViewTransition" "namespace" "signals" "onlyIfMissing")
    (vector->list (config-ref "datalineLiterals")))
  (check-equal?
    '("selector" "mode" "useViewTransition" "viewTransitionSelector" "namespace" "elements"
      "onlyIfMissing" "signals")
    (data-literals
      (sent (lambda (s)
              (patch-elements s "<b></b>"
                (patch-elements-options [selector "#t"] [mode (patch-mode inner)]
                  [use-view-transition #t] [view-transition-selector "#v"]
                  [namespace (element-namespace svg)]))
              (patch-signals s '() (patch-signals-options [only-if-missing #t])))))))

(test "sending returns #f once the stream is closed"
  (let ([stream (make-stream)])
    (check-equal? #t (patch-elements stream "<p id=\"p\"></p>"))
    (stream-close! stream)
    (check-equal? #f (patch-elements stream "<p id=\"p\"></p>"))
    (check-equal? #f (patch-signals stream '()))
    (check-equal? #f (execute-script stream "x()"))))

(test "bad arguments raise"
  (let ([stream (make-stream)])
    (check-raises assertion-violation? (patch-elements 'stream "x"))
    (check-raises assertion-violation? (patch-elements stream 42))
    (check-raises assertion-violation? (patch-elements stream "x" (patch-signals-options)))
    (check-raises assertion-violation? (patch-signals stream '#(1 2)))
    (check-raises assertion-violation? (patch-signals stream '(1 2)))
    (check-raises json-error? (patch-signals stream '(("x" . +inf.0))))
    (check-raises assertion-violation? (patch-signals stream '() (patch-elements-options)))
    (check-raises assertion-violation? (execute-script stream 'x))
    (check-raises assertion-violation? (execute-script stream "a = '</SCRIPT>'"))
    (check-raises assertion-violation? (execute-script stream "var q = '<!--<script>';"))
    (check-raises json-error? (patch-signals stream "{\"name\":\"x\"}; alert(1)"))
    (check-raises json-error? (patch-signals stream ""))
    (check-raises assertion-violation? (patch-signals stream "[1]"))
    (check-raises assertion-violation? (patch-signals stream "null"))
    (check-raises assertion-violation? (sse-response 'stream))
    (check-raises assertion-violation? (sse-response stream 'headers))))

(test "bad option values raise when the options are made"
  (let ([mode 'sideways] [broken "#a\n#b"])
    (check-raises assertion-violation? (patch-elements-options [mode mode]))
    (check-raises assertion-violation? (patch-elements-options [namespace 'xml]))
    (check-raises assertion-violation? (patch-elements-options [selector broken]))
    (check-raises assertion-violation? (patch-elements-options [selector 'body]))
    (check-raises assertion-violation? (patch-elements-options [event-id "a\rb"]))
    (check-raises assertion-violation? (patch-elements-options [event-id "a\x0;b"]))
    (check-raises assertion-violation? (patch-elements-options [event-id "日本"]))
    (check-raises assertion-violation? (patch-elements-options [event-id "é"]))
    (check-raises assertion-violation? (patch-signals-options [event-id "a\x1;b"]))
    (check-raises assertion-violation? (execute-script-options [event-id "a\x7f;b"]))
    (check-raises assertion-violation? (patch-elements-options [retry-duration -1]))
    (check-raises assertion-violation? (patch-elements-options [retry-duration 1.5]))
    (check-raises assertion-violation? (patch-elements-options [use-view-transition 'yes]))
    (check-raises assertion-violation? (patch-signals-options [only-if-missing "true"]))
    (check-raises assertion-violation? (execute-script-options [auto-remove 1]))
    (check-raises assertion-violation? (execute-script-options [attributes '(("bad name" . "x"))]))
    (check-raises assertion-violation? (execute-script-options [attributes '(("a\"" . "x"))]))
    (check-raises assertion-violation? (execute-script-options [attributes '(("a" . 1))]))
    (check-raises assertion-violation? (execute-script-options [attributes '(("" . "x"))]))))

(test "option values are evaluated once each, in the order written"
  (let ([order '()])
    (define (note x) (set! order (cons x order)) x)
    (patch-elements-options [retry-duration (note 5)] [selector (note "#a")] [mode (note 'inner)])
    (check-equal? '(5 "#a" inner) (reverse order))))

(server-stop! server)

(test-exit)
