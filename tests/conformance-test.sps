(import (chezscheme) (check) (support) (h2o) (conformance))

(define port (test-port 10))
(define base (format "http://127.0.0.1:~a" port))
(define server (start-conformance-server port))
(define suite (copy-suite "test"))

(define (cases directory)
  (sort string<? (directory-list (string-append suite "/" directory))))

(define (run-cases directory script)
  (for-each
    (lambda (name)
      (test (string-append directory "/" name)
        (let-values ([(status output) (run-in suite "sh" script (string-append "./" directory "/" name) base)])
          (printf "~a ~a/~a\n" (if (and (= status 0) (string=? output "")) "pass" "FAIL") directory name)
          (check-equal? "" output)
          (check-equal? 0 status))))
    (cases directory)))

(run-cases "get-cases" "test-get.sh")
(run-cases "post-cases" "test-post.sh")

;; test-all.sh exits 0 whatever happens; a difference shows only in what it
;; prints.
(test "test-all.sh prints no difference"
  (let-values ([(status output) (run-in suite "sh" "test-all.sh" base)])
    (check-equal?
      (format "Running tests with argument: ~a\nProcessing GET cases...\nFinished processing GET cases\nProcessing POST cases...\nFinished processing POST cases\n" base)
      output)
    (check-equal? 0 status)))

(server-stop! server)

(test-exit)
