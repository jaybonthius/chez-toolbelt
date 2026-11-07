(import (chezscheme) (json) (check))

(define (refused? thunk)
  (guard (c [(json-error? c) #t]) (thunk) #f))

(define (position-of thunk)
  (guard (c [(json-error? c) (json-error-position c)]) (thunk) 'accepted))

(define (string-position text) (position-of (lambda () (string->json text))))
(define (bytes-position text) (position-of (lambda () (bytevector->json (string->utf8 text)))))

(define (port-values port)
  (let loop ([acc '()])
    (let ([v (json-read port)])
      (if (eof-object? v) (reverse acc) (loop (cons v acc))))))

(define (nested n open close)
  (string-append (make-string n open) (make-string n close)))

(test "objects are association lists in document order, arrays vectors"
  (check-equal? '(("b" . 1) ("a" . #(1 "x" #t #f null)) ("c") ("d" . #()))
    (string->json "{\"b\":1,\"a\":[1,\"x\",true,false,null],\"c\":{},\"d\":[]}"))
  (check-equal? '() (string->json "{}"))
  (check-equal? '#() (string->json "[]"))
  (check-equal? '(("null" . null) ("" . "")) (string->json "{\"null\":null,\"\":\"\"}"))
  (check-equal? '#(#(#(1)) (("a" . (("b" . #(2)))))) (string->json "[[[1]],{\"a\":{\"b\":[2]}}]"))
  (check-equal? "" (string->json "\"\""))
  (check-equal? 'null (string->json " null "))
  (check (mutable-string? (string->json "\"a\"")))
  (check (mutable-string? (vector-ref (bytevector->json (string->utf8 "[\"a\"]")) 0))))

(define (all-keys v)
  (cond
    [(vector? v) (apply append (map all-keys (vector->list v)))]
    [(pair? v) (apply append (map (lambda (m) (cons (car m) (all-keys (cdr m)))) v))]
    [else '()]))

(test "keys are immutable strings, and equal keys in one parse share one string"
  (for-each
    (lambda (read)
      (let* ([v (read "[{\"id\":1,\"name\":\"x\"},{\"id\":2,\"name\":\"y\"},{\"n\\u0061me\":3}]")]
             [keys (all-keys v)])
        (check-equal? '("id" "name" "id" "name" "name") keys)
        (check (for-all immutable-string? keys))
        (check (eq? (list-ref keys 0) (list-ref keys 2)))
        (check (eq? (list-ref keys 1) (list-ref keys 3))))
      (let ([keys (all-keys (read "[{\"\xe9;t\xe9;\":1},{\"\xe9;t\xe9;\":2},{\"\x1F600;\":3},{\"\x1F600;\":4}]"))])
        (check-equal? '("\xe9;t\xe9;" "\xe9;t\xe9;" "\x1F600;" "\x1F600;") keys)
        (check (eq? (list-ref keys 0) (list-ref keys 1)))
        (check (eq? (list-ref keys 2) (list-ref keys 3))))
      (let ([keys (all-keys (read (string-append "[" (apply string-append
                                                       (map (lambda (i) (format "{\"k~a\":0}," i)) (iota 2000)))
                                    "{\"k7\":1},{\"k77\":1}]")))])
        (check-equal? 2002 (length keys))
        (check (equal? "k7" (list-ref keys 2000)))
        (check (equal? "k77" (list-ref keys 2001)))))
    (list string->json
          (lambda (text) (bytevector->json (string->utf8 text)))
          (lambda (text) (json-read (open-string-input-port text)))
          (lambda (text) (json-read (open-bytevector-input-port (string->utf8 text)))))))

(test "whitespace is space, tab, newline and return only"
  (check-equal? '#(1 2) (string->json " \t\r\n[ \t\r\n1 \t\r\n, \t\r\n2 \t\r\n] \t\r\n"))
  (for-each
    (lambda (ws) (check (refused? (lambda () (string->json (string-append "[1," ws "2]"))))))
    (list "\f" "\v" "\xA0;" "\x2060;" "\x3000;" "\xFEFF;")))

(test "grammar refusals"
  (for-each
    (lambda (text)
      (check (refused? (lambda () (string->json text))))
      (check (refused? (lambda () (bytevector->json (string->utf8 text))))))
    '("" " " "+1" ".5" "1." "01" "-01" "-" "1e" "1e+" "1E-" "0x10" "1.0e+-1" "NaN"
      "Infinity" "-Infinity" "1_000" "[1,]" "[,1]" "{\"a\":1,}" "{,}" "{\"a\"}" "{\"a\" 1}"
      "{a:1}" "{'a':1}" "['a']" "[1 2]" "[" "]" "{" "}" "[1}" "{\"a\":1]" "tru" "nul"
      "True" "falsey" "[true1]" "\"abc" "/* */1" "1 // x" "[1]x" "1 2" "{} {}" "[]]")))

(test "duplicate keys are refused unless allowed"
  (check (refused? (lambda () (string->json "{\"a\":1,\"a\":2}"))))
  (check (refused? (lambda () (string->json "{\"a\":1,\"b\":{\"a\":1,\"a\":2}}"))))
  (check-equal? '(("a" . 1) ("b" . (("a" . 1)))) (string->json "{\"a\":1,\"b\":{\"a\":1}}"))
  (check (refused? (lambda () (string->json "{\"a\":1,\"\\u0061\":2}"))))
  (let* ([keys (map (lambda (i) (format "\"k~a\":~a" i i)) (iota 40))]
         [object (lambda (members)
                   (string-append "{" (apply string-append
                                        (cdr (apply append (map (lambda (m) (list "," m)) members))))
                     "}"))])
    (check-equal? 40 (length (string->json (object keys))))
    (check (refused? (lambda () (string->json (object (append keys (list "\"k3\":0")))))))
    (check (refused? (lambda () (string->json (object (append keys (list "\"k39\":0")))))))
    (check (refused? (lambda () (string->json (object (append (list-head keys 32) (list "\"k31\":0")))))))
    (check (refused? (lambda () (string->json (object (append (list-head keys 33) (list "\"k32\":0")))))))
    (check (refused? (lambda () (string->json (object (append (list-head keys 33) (list "\"k0\":0")))))))
    (check (refused? (lambda () (string->json (object (append keys (list "\"k\\u0033\":0")))))))
    (let ([text (object (append keys (list "\"k39\":0" "\"k3\":0")))])
      (check-equal? (- (string-length text) (string-length "\"k39\":0,\"k3\":0}"))
        (string-position text)))
    (parameterize ([json-allow-duplicate-keys? #t])
      (check-equal? '(("a" . 1) ("b" . 2) ("a" . 3)) (string->json "{\"a\":1,\"b\":2,\"a\":3}"))
      (check-equal? 41 (length (string->json (object (append keys (list "\"k3\":0")))))))))

(test "nesting is limited by json-max-depth"
  (check-equal? 1000 (json-max-depth))
  (check (vector? (string->json (nested 1000 #\[ #\]))))
  (check (refused? (lambda () (string->json (nested 1001 #\[ #\])))))
  (check-equal? 1000 (string-position (nested 1001 #\[ #\])))
  (check (refused? (lambda () (string->json (nested 100000 #\[ #\])))))
  (let ([objects (lambda (n)
                   (string-append (apply string-append (make-list n "{\"a\":")) "1"
                     (make-string n #\})))])
    (check (pair? (string->json (objects 1000))))
    (check (refused? (lambda () (string->json (objects 1001))))))
  (parameterize ([json-max-depth 2])
    (check-equal? '#(#()) (string->json "[[]]"))
    (check (refused? (lambda () (string->json "[[[]]]"))))
    (check (refused? (lambda () (json-read (open-string-input-port "[[[]]]")))))
    (check (refused? (lambda () (json-read (open-bytevector-input-port (string->utf8 "[{\"a\":[]}]")))))))
  (parameterize ([json-max-depth 0])
    (check-equal? 1 (string->json "1"))
    (check (refused? (lambda () (string->json "[]")))))
  (check-raises assertion-violation? (json-max-depth -1))
  (check-raises assertion-violation? (json-max-depth 1.5)))

(test "numbers are limited by json-max-number-digits"
  (check-equal? 4300 (json-max-number-digits))
  (check-equal? (- (expt 10 4300) 1) (string->json (make-string 4300 #\9)))
  (check (refused? (lambda () (string->json (make-string 4301 #\9)))))
  (check (refused? (lambda () (string->json (string-append "0." (make-string 4300 #\0) "1")))))
  (check (refused? (lambda () (string->json (make-string 1000000 #\1)))))
  (check-equal? 1.0 (string->json (string-append "1." (make-string 4298 #\0))))
  (parameterize ([json-max-number-digits 5])
    (check-equal? 12345 (string->json "12345"))
    (check-equal? 1.2345e10 (string->json "1.2345e10"))
    (check (refused? (lambda () (string->json "123456"))))
    (check (refused? (lambda () (string->json "1234.56")))))
  (check-raises assertion-violation? (json-max-number-digits 0)))

(test "a number that overflows is refused; one that underflows reads as zero"
  (check (refused? (lambda () (string->json "1e309"))))
  (check (refused? (lambda () (string->json "-1e309"))))
  (check (refused? (lambda () (string->json "1e99999999999999999999"))))
  (check-equal? 0.0 (string->json "1e-99999999999999999999"))
  (check-equal? -0.0 (string->json "-1e-400")))

(test "errors carry the character or byte position"
  (check-equal? 3 (string-position "[1,]"))
  (check-equal? 2 (string-position "  x"))
  (check-equal? 0 (string-position ""))
  (check-equal? 4 (string-position "\"abc"))
  (check-equal? 6 (string-position "[\"\xe9;\", x]"))
  (check-equal? 7 (bytes-position "[\"\xe9;\", x]"))
  (check-equal? 7 (string-position "{\"a\":1,\"a\":2}"))
  (check-equal? 2 (string-position "[\"\\ud800\"]"))
  (check-equal? 1 (string-position "[01]"))
  (check-equal? 3 (string-position "[1.e5]"))
  (check-equal? 3 (string-position "[1]x"))
  (check-equal? 4 (string-position "[tru]"))
  (check-equal? 3 (position-of (lambda () (bytevector->json (bytevector #x5B #x22 #x61 #xFF #x22 #x5D)))))
  (check-equal? 4 (position-of (lambda () (bytevector->json (bytevector #xEF #xBB #xBF #x5B #x78 #x5D)))))
  (check-equal? 1 (parameterize ([json-max-depth 1]) (string-position "[[1]]")))
  (check-equal? 1 (parameterize ([json-max-number-digits 2]) (string-position "[-123]")))
  (check-equal? 1 (string-position "[1e400]"))
  (guard (c [#t (check (json-error? c))
                (check (who-condition? c))
                (check-equal? 'string->json (condition-who c))
                (check (message-condition? c))])
    (string->json "[1,]")))

(define (condition-text thunk)
  (guard (c [(json-error? c)
             (list (call-with-string-output-port (lambda (p) (display-condition c p)))
                   (condition-irritants c))])
    (thunk)))

(test "irritants are the values; the message gives the position in words"
  (check-equal? '("Exception in string->json: unexpected #\\] at position 3" (#\]))
    (condition-text (lambda () (string->json "[1,]"))))
  (check-equal? '("Exception in string->json: unexpected end of input at position 1" ())
    (condition-text (lambda () (string->json "["))))
  (check-equal? '("Exception in string->json: unterminated escape at position 1" ())
    (condition-text (lambda () (string->json "\"\\"))))
  (check-equal? '("Exception in string->json: duplicate key \"a\" at position 7" ("a"))
    (condition-text (lambda () (string->json "{\"a\":1,\"a\":2}"))))
  (check-equal? '("Exception in bytevector->json: invalid UTF-8 sequence starting with byte #xFF at position 2" (255))
    (condition-text (lambda () (bytevector->json (bytevector #x5B #x22 #xFF #x22 #x5D)))))
  (check-equal? '("Exception in bytevector->json: unexpected byte #xC3 at position 1" (#xC3))
    (condition-text (lambda () (bytevector->json (bytevector #x5B #xC3 #xA9 #x5D)))))
  (check-equal? '("Exception in string->json: nesting deeper than 1 at position 1" (1))
    (parameterize ([json-max-depth 1]) (condition-text (lambda () (string->json "[[]]"))))))

(test "json-read reads one value at a time and stops just after it"
  (let ([port (open-string-input-port "1 \"two\" [3] {\"four\":4}\ntrue null -5.5e1\t[]{}12")])
    (check-equal? '(1 "two" #(3) (("four" . 4)) #t null -55.0 #() () 12) (port-values port)))
  (let ([port (open-bytevector-input-port (string->utf8 "1 \"\xe9;\" [3]{\"a\":[]}"))])
    (check-equal? '(1 "\xe9;" #(3) (("a" . #()))) (port-values port)))
  (let ([port (open-string-input-port "[1] rest")])
    (check-equal? '#(1) (json-read port))
    (check-equal? #\space (lookahead-char port)))
  (let ([port (open-string-input-port "12,34")])
    (check-equal? 12 (json-read port))
    (check-equal? #\, (lookahead-char port))
    (check (refused? (lambda () (json-read port)))))
  (let ([port (open-string-input-port "\"a\\\"]b\"x")])
    (check-equal? "a\"]b" (json-read port))
    (check-equal? #\x (lookahead-char port)))
  (let ([port (open-bytevector-input-port (string->utf8 "{\"a\":\"}\"} 2"))])
    (check-equal? '(("a" . "}")) (json-read port))
    (check-equal? 32 (lookahead-u8 port))
    (check-equal? 2 (json-read port))
    (check (eof-object? (json-read port)))
    (check (eof-object? (json-read port))))
  (check (eof-object? (json-read (open-string-input-port ""))))
  (check (eof-object? (json-read (open-string-input-port " \n\t "))))
  (check (eof-object? (json-read (open-bytevector-input-port (bytevector)))))
  (check-equal? '#(1) (parameterize ([current-input-port (open-string-input-port " [1]")]) (json-read))))

(test "json-read refuses what string->json refuses"
  (for-each
    (lambda (text)
      (check (refused? (lambda () (json-read (open-string-input-port text)))))
      (check (refused? (lambda () (json-read (open-bytevector-input-port (string->utf8 text)))))))
    '("1x" "truex" "-" "[1," "[1,]" "{\"a\"" "\"abc" "\"\t\"" "x" "," "]" "01" "[1}"
      "{\"a\":1,\"a\":2}"))
  (let ([port (open-string-input-port "[1]x")])
    (check-equal? '#(1) (json-read port))
    (check (refused? (lambda () (json-read port))))))

(define (chunked-port text size binary?)
  (let ([bytes (if binary? (string->utf8 text) text)] [i 0])
    (define (read! target start count)
      (let ([n (min count size (- (if binary? (bytevector-length bytes) (string-length bytes)) i))])
        (if binary?
            (bytevector-copy! bytes i target start n)
            (string-copy! bytes i target start n))
        (set! i (+ i n))
        n))
    (if binary?
        (make-custom-binary-input-port "chunks" read! #f #f #f)
        (make-custom-textual-input-port "chunks" read! #f #f #f))))

(test "json-read reads values that cross the port's buffer at every point"
  (let* ([text "{\"a\\\"b\":[1,-2.5e3,\"x\\\\\",{\"\xe9;\":true}],\"c\":null}  \"s\\\"\" 12 [] {} \"\x1F600;\""]
         [expected (list (string->json "{\"a\\\"b\":[1,-2.5e3,\"x\\\\\",{\"\xe9;\":true}],\"c\":null}")
                         "s\"" 12 '#() '() "\x1F600;")])
    (for-each
      (lambda (size)
        (check-equal? expected (port-values (chunked-port text size #f)))
        (check-equal? expected (port-values (chunked-port text size #t))))
      '(1 2 3 5 7 64)))
  (let* ([long (make-string 100000 #\a)]
         [text (string-append "[\"" long "\"," (apply string-append (make-list 20000 "[1],")) "2] 3")])
    (for-each
      (lambda (port)
        (let ([v (json-read port)])
          (check-equal? 20002 (vector-length v))
          (check-equal? long (vector-ref v 0)))
        (check-equal? 3 (json-read port))
        (check (eof-object? (json-read port))))
      (list (chunked-port text 4096 #f) (chunked-port text 4096 #t)
            (open-string-input-port text) (open-bytevector-input-port (string->utf8 text)))))
  (let ([path "build/json-read-test.json"])
    (call-with-port (open-file-output-port path (file-options no-fail))
      (lambda (port) (put-bytevector port (string->utf8 "{\"k\": [\"\xe9;\", 1.5]}\n7"))))
    (call-with-port (open-input-file path)
      (lambda (port) (check-equal? '((("k" . #("\xe9;" 1.5))) 7) (port-values port))))
    (call-with-port (open-file-input-port path)
      (lambda (port) (check-equal? '((("k" . #("\xe9;" 1.5))) 7) (port-values port))))
    (delete-file path))
  (check (refused? (lambda () (json-read (chunked-port "[1, 2" 1 #f)))))
  (check (refused? (lambda () (json-read (chunked-port "\"abc\\" 1 #t)))))
  (check-equal? 5 (position-of (lambda () (json-read (chunked-port "  [1,]" 1 #f))))))

(test "json-read on a binary port skips a byte order mark only at position 0"
  (let ([port (open-bytevector-input-port (bytevector #xEF #xBB #xBF #x31 #x20 #xEF #xBB #xBF #x32))])
    (check-equal? 1 (json-read port))
    (check (refused? (lambda () (json-read port)))))
  (check-equal? 4 (position-of (lambda () (json-read (open-bytevector-input-port (bytevector #xEF #xBB #xBF #x5B #x78 #x5D))))))
  (check (refused? (lambda () (json-read (open-bytevector-input-port (bytevector #xEF #xBB #x31))))))
  (let ([port (chunked-port "\xFEFF;1" 1 #t)])
    (check (refused? (lambda () (json-read port)))))
  (check (eof-object? (json-read (open-bytevector-input-port (bytevector #xEF #xBB #xBF #x20)))))
  (check (refused? (lambda () (json-read (open-string-input-port "\xFEFF;1"))))))

(define (unbuffered-port text initial)
  (let ([i 0])
    (make-input-port
      (lambda (message . arguments)
        (record-case (cons message arguments)
          [read-char (port)
           (if (< i (string-length text))
               (let ([c (string-ref text i)]) (set! i (+ i 1)) c)
               (eof-object))]
          [peek-char (port)
           (if (< i (string-length text)) (string-ref text i) (eof-object))]
          [char-ready? (port) #t]
          [close-port (port) (void)]
          [port-name (port) "unbuffered"]
          [else (assertion-violationf 'unbuffered-port "unsupported message ~s" message)]))
      initial)))

(test "json-read reads ports without a buffer one unit at a time"
  (let ([port (unbuffered-port "[1, 2] 3" "")])
    (check-equal? '#(1 2) (json-read port))
    (check-equal? 3 (json-read port))
    (check (eof-object? (json-read port))))
  (let ([port (unbuffered-port " \"a\\\"b\" {\"k\": [true, null]}\ttrue,12" "")])
    (check-equal? '("a\"b" (("k" . #(#t null))) #t) (list (json-read port) (json-read port) (json-read port)))
    (check-equal? #\, (read-char port))
    (check-equal? 12 (json-read port)))
  (let ([port (unbuffered-port "[1, 2,]" "")])
    (check (refused? (lambda () (json-read port))))
    (check (eof-object? (read-char port))))
  (for-each
    (lambda (prefix)
      (let* ([text "{\"a\": [\"x\\\"y\", 12]} 7"]
             [port (unbuffered-port (substring text (string-length prefix) (string-length text)) prefix)])
        (check-equal? '(("a" . #("x\"y" 12))) (json-read port))
        (check-equal? 7 (json-read port))))
    '("" "{" "{\"" "{\"a\": [\"x" "{\"a\": [\"x\\" "{\"a\": [\"x\\\"y\", 1")))

(test "json-read reads two values from piped standard input"
  (let ([script "build/json-read-stdin.ss"] [output "build/json-read-stdin.out"])
    (call-with-port (open-file-output-port script (file-options no-fail) (buffer-mode block) (native-transcoder))
      (lambda (port)
        (put-string port "(import (chezscheme) (json)) (let* ([a (json-read)] [b (json-read)] [c (json-read)]) (write (list a b c)))")))
    (check-equal? 0
      (system (format "printf '[1, 2] {\"a\": 3}' | ~a -q --libdirs src::build/obj/src --script ~a > ~a"
                (or (getenv "SCHEME") "scheme") script output)))
    (let ([values-read (call-with-input-file output read)])
      (check-equal? '(#(1 2) (("a" . 3))) (list-head values-read 2))
      (check (eof-object? (list-ref values-read 2))))
    (delete-file script)
    (delete-file output)))

(test "after an error json-read leaves the port after the text it scanned"
  (let ([port (open-string-input-port "[1,] [2]")])
    (check (refused? (lambda () (json-read port))))
    (check-equal? '#(2) (json-read port)))
  (let ([port (open-string-input-port "{\"a\":1,\"a\":2} 3")])
    (check (refused? (lambda () (json-read port))))
    (check-equal? 3 (json-read port)))
  (let ([port (open-string-input-port "[[[1]]] 4")])
    (check (refused? (lambda () (parameterize ([json-max-depth 2]) (json-read port)))))
    (check-equal? #\1 (lookahead-char port)))
  (let ([port (open-string-input-port "[1, 2")])
    (check (refused? (lambda () (json-read port))))
    (check (eof-object? (lookahead-char port)))))

(test "json-read positions count from where the call began reading"
  (check-equal? 5 (position-of (lambda () (json-read (open-string-input-port "  [1,]")))))
  (check-equal? 6 (position-of (lambda () (json-read (open-bytevector-input-port (string->utf8 " [\"\xe9;\"x"))))))
  (let ([port (open-string-input-port "[1] [2,]")])
    (json-read port)
    (check-equal? 4 (position-of (lambda () (json-read port))))))

(test "arguments are checked"
  (check-raises assertion-violation? (string->json 5))
  (check-raises assertion-violation? (string->json (string->utf8 "1")))
  (check-raises assertion-violation? (bytevector->json "1"))
  (check-raises assertion-violation? (json-read "1"))
  (check-raises assertion-violation? (json-read (open-output-string))))

(test "settings are per thread"
  (let ([m (make-mutex)] [seen #f])
    (parameterize ([json-max-depth 3])
      (let ([t (fork-thread
                 (lambda ()
                   (json-max-depth 7)
                   (with-mutex m (set! seen (json-max-depth)))))])
        (thread-join t)
        (check-equal? 7 seen)
        (check-equal? 3 (json-max-depth))))
    (check-equal? 1000 (json-max-depth))))

(test-exit)
