(import (chezscheme) (check) (support) (haml) (datastar))

(define (read-js-string s start)
  (assert (char=? (string-ref s start) #\'))
  (let-values ([(out get) (open-string-output-port)])
    (define (hex i n) (string->number (substring s i (+ i n)) 16))
    (let loop ([i (+ start 1)])
      (let ([c (string-ref s i)])
        (cond
          [(char=? c #\') (values (get) (+ i 1))]
          [(char=? c #\\)
           (let ([e (string-ref s (+ i 1))])
             (case e
               [(#\n) (write-char #\newline out) (loop (+ i 2))]
               [(#\r) (write-char #\return out) (loop (+ i 2))]
               [(#\t) (write-char #\tab out) (loop (+ i 2))]
               [(#\x) (write-char (integer->char (hex (+ i 2) 2)) out) (loop (+ i 4))]
               [(#\u) (write-char (integer->char (hex (+ i 2) 4)) out) (loop (+ i 6))]
               [else (error 'read-js-string "unexpected escape" e)]))]
          [else (write-char c out) (loop (+ i 1))])))))

(define (url-of action)
  (let-values ([(url end) (read-js-string action (+ (string-index action #\() 1))])
    (assert (char=? (string-ref action end) #\)))
    url))

(define (count-char s c)
  (fold-left (lambda (n x) (if (char=? x c) (+ n 1) n)) 0 (string->list s)))

(define awkward
  '("it's" "say \"hi\"" "back\\slash" "a\\'b" "ends with \\" "line\nbreak\r\nend"
    "\x2028;and\x2029;" "</script><script>alert(1)</script>" "&#39;);alert(1)//" "&quot;"
    "naïve 日本 🎉" "$signal and @post('/x') and ${x}" "tab\t nul\x0; delete\x7f;" ""))

(test "every backend action, without options"
  (check-equal? "@get('/items')" (get "/items"))
  (check-equal? "@post('/items')" (post "/items"))
  (check-equal? "@put('/items')" (put "/items"))
  (check-equal? "@patch('/items')" (patch "/items"))
  (check-equal? "@query('/items')" (query "/items"))
  (check-equal? "@delete('/items')" (delete "/items"))
  (check-equal? "@get('/items')" (get "/items" (fetch-options))))

(test "each option"
  (define (with options) (post "/x" options))
  (check-equal? "@post('/x', {contentType: 'form'})"
    (with (fetch-options [content-type (fetch-content-type form)])))
  (check-equal? "@post('/x', {contentType: 'json'})"
    (with (fetch-options [content-type (fetch-content-type json)])))
  (check-equal? "@post('/x', {filterSignals: {include: '^foo\\x5C.'}})"
    (with (fetch-options [filter-signals-include "^foo\\."])))
  (check-equal? "@post('/x', {filterSignals: {exclude: '(^|\\x5C.)_'}})"
    (with (fetch-options [filter-signals-exclude "(^|\\.)_"])))
  (check-equal? "@post('/x', {filterSignals: {include: 'a', exclude: 'b'}})"
    (with (fetch-options [filter-signals-exclude "b"] [filter-signals-include "a"])))
  (check-equal? "@post('/x', {selector: '#form'})" (with (fetch-options [selector "#form"])))
  (check-equal? "@post('/x', {headers: {['X-Csrf-Token']: 'abc', ['X-Other']: ''}})"
    (with (fetch-options [headers '(("X-Csrf-Token" . "abc") ("X-Other" . ""))])))
  (check-equal? "@post('/x', {openWhenHidden: true})" (with (fetch-options [open-when-hidden #t])))
  (check-equal? "@post('/x', {openWhenHidden: false})" (with (fetch-options [open-when-hidden #f])))
  (check-equal? "@post('/x', {payload: {['a']: [1, 2.5, 'x', true, false, null, {}, []], ['b']: -3}})"
    (with (fetch-options [payload '(("a" . #(1 2.5 "x" #t #f null () #())) (b . -3))])))
  (check-equal? "@post('/x', {payload: 'text'})" (with (fetch-options [payload "text"])))
  (for-each
    (lambda (retry)
      (check-equal? (format "@post('/x', {retry: '~a'})" retry) (with (fetch-options [retry retry]))))
    '(auto error always never))
  (check-equal? "@post('/x', {retryInterval: 500})" (with (fetch-options [retry-interval 500])))
  (check-equal? "@post('/x', {retryScaler: 1.5})" (with (fetch-options [retry-scaler 1.5])))
  (check-equal? "@post('/x', {retryScaler: 3})" (with (fetch-options [retry-scaler 3])))
  (check-equal? "@post('/x', {retryMaxWait: 0})" (with (fetch-options [retry-max-wait 0])))
  (check-equal? "@post('/x', {retryMaxCount: 10})" (with (fetch-options [retry-max-count 10])))
  (for-each
    (lambda (cancellation)
      (check-equal? (format "@post('/x', {requestCancellation: '~a'})" cancellation)
        (with (fetch-options [request-cancellation cancellation]))))
    '(auto cleanup disabled)))

(test "every option at once, in the reference's order"
  (check-equal?
    (string-append
      "@get('/feed', {contentType: 'json', filterSignals: {include: '^feed', exclude: '_'}, "
      "selector: '#f', headers: {['X-A']: 'b'}, openWhenHidden: true, payload: {['n']: 1}, "
      "retry: 'always', retryInterval: 100, retryScaler: 2, retryMaxWait: 5000, "
      "retryMaxCount: 4, requestCancellation: 'cleanup'})")
    (get "/feed"
      (fetch-options [request-cancellation (request-cancellation cleanup)] [retry-max-count 4]
        [retry-max-wait 5000] [retry-scaler 2] [retry-interval 100] [retry (fetch-retry always)]
        [payload '(("n" . 1))] [open-when-hidden #t] [headers '(("X-A" . "b"))] [selector "#f"]
        [filter-signals-exclude "_"] [filter-signals-include "^feed"]
        [content-type (fetch-content-type json)]))))

(test "a URL stays inside its string and its attribute"
  (for-each
    (lambda (url)
      (let ([action (post url)])
        (check-equal? url (url-of action))
        (check-equal? 2 (count-char action #\'))
        (check (not (exists (lambda (c) (memv c '(#\" #\& #\< #\> #\newline #\return #\x2028 #\x2029)))
                            (string->list action))))
        (check-equal? (string-append "<button data-on:click=\"" action "\"></button>")
          (html->string (haml (:button ([data-on:click action])))))))
    awkward))

(test "option values stay inside their strings and the attribute"
  (for-each
    (lambda (s)
      (let ([action (get "/x"
                      (fetch-options [selector s] [filter-signals-include s]
                        [payload (list (cons s (vector s)))]))])
        (check (not (exists (lambda (c) (memv c '(#\" #\& #\< #\> #\newline #\return #\x2028 #\x2029)))
                            (string->list action))))
        (check-equal? 10 (count-char action #\'))
        (let-values ([(selector end) (read-js-string action (+ (string-search action "selector: ") 10))])
          (check-equal? s selector))))
    awkward)
  (check-equal? "@get('/x', {selector: '\\x27);alert(1)//'})"
    (get "/x" (fetch-options [selector "');alert(1)//"])))
  (check-equal? "@get('/x', {headers: {['X-A']: 'a\\x27\\x22\\x5C\\x26\\x3C/script\\x3E\\t'}})"
    (get "/x" (fetch-options [headers '(("X-A" . "a'\"\\&</script>\t"))]))))

(test "a __proto__ key is a property, not the prototype"
  (check-equal? "@get('/x', {headers: {['__proto__']: 'v'}, payload: {['__proto__']: {['a']: 1}}})"
    (get "/x" (fetch-options [headers '(("__proto__" . "v"))] [payload '(("__proto__" ("a" . 1)))]))))

;; The string rule of statementRe in Datastar's engine.ts, '(?:\\'|[^'])*',
;; with a regular expression's backtracking order.
(define (statement-string-end s start)
  (let ([n (string-length s)])
    (let try ([i (+ start 1)])
      (or (and (< (+ i 1) n) (char=? (string-ref s i) #\\) (char=? (string-ref s (+ i 1)) #\')
               (try (+ i 2)))
          (and (< i n) (not (char=? (string-ref s i) #\')) (try (+ i 1)))
          (and (< i n) (char=? (string-ref s i) #\') (+ i 1))))))

(test "Datastar's statement splitter ends each string where it ends"
  (for-each
    (lambda (s)
      (let* ([action (post s (fetch-options [selector s]))]
             [start (+ (string-index action #\() 1)])
        (let-values ([(value end) (read-js-string action start)])
          (check-equal? s value)
          (check-equal? end (statement-string-end action start)))))
    awkward)
  (let ([escaped-with-backslashes "@post('ends with \\\\', {selector: 'x'})"])
    (check (> (statement-string-end escaped-with-backslashes 6) 20))))

(test "bad arguments raise"
  (check-raises assertion-violation? (get 'x))
  (check-raises assertion-violation? (get "/x" (patch-elements-options)))
  (check-raises assertion-violation? (delete "/x" "{retry: 'never'}")))

(test "bad option values raise when the options are made"
  (let ([content-type 'xml] [retry 'sometimes] [cancellation 'never] [cyclic (vector 1)])
    (vector-set! cyclic 0 cyclic)
    (check-raises assertion-violation? (fetch-options [content-type content-type]))
    (check-raises assertion-violation? (fetch-options [retry retry]))
    (check-raises assertion-violation? (fetch-options [request-cancellation cancellation]))
    (check-raises assertion-violation? (fetch-options [filter-signals-include 'x]))
    (check-raises assertion-violation? (fetch-options [filter-signals-exclude #f]))
    (check-raises assertion-violation? (fetch-options [selector 1]))
    (check-raises assertion-violation? (fetch-options [headers '(("a" . 1))]))
    (check-raises assertion-violation? (fetch-options [headers '("a")]))
    (check-raises assertion-violation? (fetch-options [headers '(("bad name" . "v"))]))
    (check-raises assertion-violation? (fetch-options [headers '(("" . "v"))]))
    (check-raises assertion-violation? (fetch-options [headers '(("X-A" . "a\nb"))]))
    (check-raises assertion-violation? (fetch-options [headers '(("X-A" . "a\x0;b"))]))
    (check-raises assertion-violation? (fetch-options [headers '(("X-A" . "日本"))]))
    (check-raises assertion-violation? (fetch-options [open-when-hidden "yes"]))
    (check-raises assertion-violation? (fetch-options [payload (list (cons "f" car))]))
    (check-raises assertion-violation? (fetch-options [payload 'undefined]))
    (check-raises assertion-violation? (fetch-options [payload +nan.0]))
    (check-raises assertion-violation? (fetch-options [payload 1/2]))
    (check-raises assertion-violation? (fetch-options [payload cyclic]))
    (check-equal? "payload nests deeper than 1000"
      (guard (c [(assertion-violation? c)
                 (apply format (condition-message c) (condition-irritants c))])
        (fetch-options [payload cyclic])))
    (check-raises assertion-violation? (fetch-options [retry-interval -1]))
    (check-raises assertion-violation? (fetch-options [retry-max-wait 1.0]))
    (check-raises assertion-violation? (fetch-options [retry-max-count "3"]))
    (check-raises assertion-violation? (fetch-options [retry-scaler +inf.0]))
    (check-raises assertion-violation? (fetch-options [retry-scaler 1/2]))))

(test-exit)
