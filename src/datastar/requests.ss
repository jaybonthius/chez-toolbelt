(define (check-request who request)
  (unless (request? request) (assertion-violationf who "~s is not a request" request)))

;; A missing parameter reads as empty text, so that it raises the same
;; condition as invalid JSON.
(define (query-signals query)
  (cond
    [(and query (assoc "datastar" (query->alist query))) => cdr]
    [else ""]))

(define (read-signals request)
  (check-request 'read-signals request)
  ;; Datastar sends no body with GET or DELETE.
  (if (member (request-method request) '("GET" "DELETE"))
      (string->json (query-signals (request-query request)))
      (bytevector->json (request-body request))))

(define (datastar-request? request)
  (check-request 'datastar-request? request)
  (equal? (request-header request "datastar-request") "true"))
