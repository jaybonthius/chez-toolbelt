# chez-datastar

A Datastar SDK for threaded Chez Scheme 10.4.1, for Datastar v1.0.4. It reads the signals a browser sends, streams Datastar's server-sent events (patching elements, patching signals, running scripts) on an `(h2o)` event stream from any thread, builds the one-shot `text/html`, `application/json` and `text/javascript` responses, and writes backend-action strings such as `@post('/add')` for attribute values. Elements can be `(haml)` values; signals are `(json)` values.

```scheme
(import (chezscheme) (h2o) (haml) (datastar))

(define count 0)
(define count-mutex (make-mutex))

(define (counter n) (haml (:p ([id "count"]) n)))

(define (home request)
  (response 200 '(("content-type" . "text/html; charset=utf-8"))
    (html->bytevector
      (haml
        (:html ([lang "en"])
          (:head
            (:title "Counter")
            (:script ([type "module"]
                      [src "https://cdn.jsdelivr.net/gh/starfederation/datastar@v1.0.4/bundles/datastar.js"])))
          (:body ([data-signals "{step: 2}"])
            (:button ([data-on:click (post "/add")]) "Add")
            (counter count)))))))

(define (add request)
  (let* ([step (cdr (assoc "step" (read-signals request)))]
         [n (with-mutex count-mutex (set! count (+ count step)) count)]
         [stream (make-stream)])
    (patch-elements stream (counter n))
    (patch-signals stream '(("step" . 1)))
    (stream-close! stream)
    (sse-response stream)))

(define server
  (start-server (list (http-listener "127.0.0.1" 8080))
    (list (make-host '("127.0.0.1:8080" "localhost:8080")
            (list (route "/add" (handler add))
                  (route "/" (handler home)))))))
```

A click posts the page's signals to `/add`, which answers with two events: the new count, morphed into `<p id="count">`, and a signal patch. A handler can instead keep the stream and send events to it later, from any thread.

## Building

The build needs threaded Chez Scheme 10.4.1 and three libraries, each built with its own Makefile: chez-h2o for `(h2o)`, chez-json for `(json)` and chez-haml for `(haml)`. `H2O_DIR`, `JSON_DIR` and `HAML_DIR` name their checkouts, by default `../chez-h2o`, `../chez-json` and `../chez-haml`. Datastar v1.0.4 is a git submodule, for its SDK specification and conformance suites, so clone with `--recursive`.

```sh
git clone --recursive <this repository>
make SCHEME=/path/to/chez-10.4.1/scheme deps
make SCHEME=/path/to/chez-10.4.1/scheme build
make SCHEME=/path/to/chez-10.4.1/scheme test
make SCHEME=/path/to/chez-10.4.1/scheme conformance
```

`make deps` builds the three libraries. `CAP` prefixes every build and test process, for example `CAP="systemd-run --user --scope -q -p MemoryMax=8G"`. The tests need `curl`, `sh` and `awk`, and listen on ports `TEST_PORT`+1 to `TEST_PORT`+11; `TEST_PORT` is 18600 unless given, so two checkouts can test at once with different values. `make test` includes Datastar's shell conformance suite (`sdk/test`); `make conformance` runs it and Datastar's Go suite (`sdk/tests`), which needs Go and fetches the suite's Go modules the first time. Both suites run from copies under `build/`.

Programs import `(datastar)` with `--libdirs /path/to/chez-datastar/build/obj/src:/path/to/chez-h2o/build/obj/src:/path/to/chez-json/build/obj/src:/path/to/chez-haml/build/obj/src`.

## API

| Form | What it does |
|---|---|
| `(read-signals request)` | the signals a request carries, as a `(json)` value: the `datastar` query parameter for `GET` and `DELETE`, the body for every other method. Missing or invalid JSON raises `(json)`'s `&json` condition. |
| `(datastar-request? request)` | whether the request has `Datastar-Request: true` |
| `(sse-response stream [headers])` | an `(h2o)` stream response, status 200, with `Content-Type: text/event-stream`, `Cache-Control: no-cache` and the extra headers |
| `(patch-elements stream elements [options])` | sends a `datastar-patch-elements` event. `elements` is an `html` value, a string, a bytevector of UTF-8, or `#f` for none (`mode remove`); each line, ended by CR, LF or CRLF, is one `data: elements` line. Returns `#f` once the stream is closed, or when the event would take it over `(h2o)`'s `max-stream-buffer`, which fails the stream. `patch-signals` and `execute-script` return the same. |
| `(patch-signals stream signals [options])` | sends a `datastar-patch-signals` event. `signals` is a `(json)` object, written on one line, or a string holding a JSON object, written one `data: signals` line per line. A string that is not JSON raises `&json`, and one holding anything but an object raises. |
| `(execute-script stream script [options])` | sends a `<script>` element appended to `body`; a script containing `</script` or `<!--`, in any case, raises |
| `(elements-response elements [options])` | a `text/html` response, with `datastar-selector`, `datastar-mode`, `datastar-namespace` and `datastar-use-view-transition` headers; a selector must be printable ASCII or tab |
| `(signals-response signals [options])` | an `application/json` response, with `datastar-only-if-missing`; `signals` as for `patch-signals` |
| `(script-response script [options])` | a `text/javascript` response, with `datastar-script-attributes`; the attributes must be printable ASCII or tab |
| `(patch-elements-options [name value] ...)` | `selector`, `mode`, `namespace`, `use-view-transition`, `view-transition-selector`, `event-id`, `retry-duration` |
| `(patch-signals-options [name value] ...)` | `only-if-missing`, `event-id`, `retry-duration` |
| `(execute-script-options [name value] ...)` | `auto-remove` (default `#t`), `attributes` (an association list of strings), `event-id`, `retry-duration` |
| `(fetch-options [name value] ...)` | `content-type`, `filter-signals-include` and `filter-signals-exclude` (regular-expression sources; the client strips one leading and one trailing `/`), `selector`, `headers` (an association list of header names and values of printable ASCII or tab), `open-when-hidden`, `payload` (a `(json)` value), `retry`, `retry-interval`, `retry-scaler`, `retry-max-wait`, `retry-max-count`, `request-cancellation` |
| `(patch-mode name)`, `(element-namespace name)`, `(fetch-content-type name)`, `(fetch-retry name)`, `(request-cancellation name)` | the values of the closed options: `outer inner remove replace prepend append before after`; `html svg mathml`; `json form`; `auto error always never`; `auto cleanup disabled` |
| `(get url [options])`, `post`, `put`, `patch`, `query`, `delete` | the backend action as a string for an attribute value, such as `@get('/feed', {openWhenHidden: true})` |

An options form names its options at compile time: a misspelled name or a name given twice fails to compile, and so does a value outside a closed set when it is written with the enumeration form. Values are checked when the form is evaluated. Event options are written only when they differ from Datastar's defaults (`retry: 1000` is left out); action options are written whenever given. The events' `selector`, `view-transition-selector` and `event-id` take `#f` for none; selectors refuse a line break, and an event id must be printable ASCII or tab, because the client sends it back in a `Last-Event-ID` header. `view-transition-selector` is written only with `use-view-transition #t`. A one-shot response raises for an option it cannot carry in a header: `event-id`, `retry-duration`, `view-transition-selector` and `auto-remove`.

Each event is sent with one `stream-send!`, so events sent to one stream from several threads never interleave. No `Connection` header is added: HTTP/2 forbids it, and H2O keeps HTTP/1.1 connections alive itself.

An action string escapes its URL and option values as JavaScript string literals with `\x` escapes for quotes, backslashes, `&`, `<`, `>` and control characters, so nothing a caller passes can end the string, or the double-quoted attribute around it whether or not the attribute is HTML-escaped.
