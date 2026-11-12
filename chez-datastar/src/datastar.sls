(library (datastar)
  (export
    ;; Requests
    read-signals datastar-request?
    ;; Event streams
    sse-response patch-elements patch-signals execute-script
    ;; One-shot responses
    elements-response signals-response script-response
    ;; Options
    patch-elements-options patch-signals-options execute-script-options fetch-options
    patch-mode element-namespace fetch-content-type fetch-retry request-cancellation
    ;; Backend actions
    get post put patch query delete)
  (import (chezscheme) (h2o) (json) (haml))

  (include "datastar/options.ss")
  (include "datastar/requests.ss")
  (include "datastar/events.ss")
  (include "datastar/responses.ss")
  (include "datastar/actions.ss"))
