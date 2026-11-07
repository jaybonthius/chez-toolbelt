(import (chezscheme) (check) (support) (h2o) (conformance))

(define port (test-port 11))
(define base (format "http://127.0.0.1:~a" port))
(define server (start-conformance-server port))
(define suite (copy-suite "tests"))

(define (result-lines output)
  (filter (lambda (line) (or (string-contains? line "--- PASS") (string-contains? line "--- FAIL")))
          (split-lines output)))

;; The runner reads its server from -server and ignores TEST_SERVER_URL,
;; which its README names, so both are given. -count=1 stops go test from
;; reporting a cached result without asking the server.
(test "the Go suite passes"
  (let-values ([(status output)
                (run-in suite "env" (string-append "TEST_SERVER_URL=" base)
                  "go" "test" "-count=1" "-v" "." "-args" "-server" base)])
    (for-each (lambda (line) (printf "~a\n" (string-trim line))) (result-lines output))
    (unless (= status 0) (display output))
    (check-equal? 0 status)
    (check (not (string-contains? output "--- FAIL")))
    (check-equal?
      (+ (length (directory-list (string-append suite "/golden/get")))
         (length (directory-list (string-append suite "/golden/post"))))
      (length (filter (lambda (line) (and (string-contains? line "--- PASS") (string-index line #\/)))
                      (result-lines output))))))

(server-stop! server)

(test-exit)
