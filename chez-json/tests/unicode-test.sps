(import (chezscheme) (json) (check))

(define (refused? thunk)
  (guard (c [(json-error? c) #t]) (thunk) #f))

(define (read-both text)
  (let ([v (string->json text)])
    (check-equal? v (bytevector->json (string->utf8 text)))
    v))

(define (refused-both? text)
  (and (refused? (lambda () (string->json text)))
       (refused? (lambda () (bytevector->json (string->utf8 text))))))

(test "escaped surrogate pairs read as one character"
  (check-equal? (vector (string (integer->char #x1F600))) (read-both "[\"\\ud83d\\ude00\"]"))
  (check-equal? (vector (string (integer->char #x1D11E))) (read-both "[\"\\uD834\\uDD1E\"]"))
  (check-equal? (vector (string #\a (integer->char #x10FFFF) #\b))
    (read-both "[\"a\\uDBFF\\uDFFFb\"]")))

(test "lone and misordered surrogate escapes are refused"
  (for-each
    (lambda (text) (check (refused-both? text)))
    '("[\"\\ud800\"]"
      "[\"\\udc00\"]"
      "[\"\\ud800\\u0041\"]"
      "[\"\\ud800\\ud800\"]"
      "[\"\\udc00\\ud800\"]"
      "[\"\\ud800x\"]"
      "[\"\\ud800\\\"]"
      "[\"\\ud800\\n\"]"
      "[\"\\uDFFF\"]")))

(test "escapes"
  (check-equal? (vector (string #\nul)) (read-both "[\"\\u0000\"]"))
  (check-equal? (vector "\"\\/\b\f\n\r\t") (read-both "[\"\\\"\\\\\\/\\b\\f\\n\\r\\t\"]"))
  (check-equal? (vector "\x00e9;\x00E9;") (read-both "[\"\\u00e9\\u00E9\"]"))
  (for-each
    (lambda (text) (check (refused-both? text)))
    '("[\"\\x41\"]" "[\"\\U0041\"]" "[\"\\u004\"]" "[\"\\u00G1\"]" "[\"\\'\"]" "[\"\\\"]"
      "[\"\\a\"]" "[\"\\")))

(test "raw characters: DEL and beyond are allowed, controls are not"
  (check-equal? (vector "\x7f;") (read-both "[\"\x7f;\"]"))
  (check-equal? (vector "\x2028;\x2029;") (read-both "[\"\x2028;\x2029;\"]"))
  (check-equal? (vector "\x1F600;") (read-both "[\"\x1F600;\"]"))
  (check-equal? (vector "\xFFFE;\xFFFF;\xFDD0;") (read-both "[\"\xFFFE;\xFFFF;\xFDD0;\"]"))
  (do ([i 0 (+ i 1)]) ((= i 32))
    (check (refused-both? (string #\[ #\" (integer->char i) #\" #\])))))

(define writer-cases
  (append
    (map (lambda (i)
           (list (string (integer->char i))
                 (case i
                   [(8) "\\b"] [(9) "\\t"] [(10) "\\n"] [(12) "\\f"] [(13) "\\r"]
                   [else (string-downcase (format "\\u00~2,'0x" i))])))
         (iota 32))
    (list (list "\"" "\\\"") (list "/" "/") (list "\\" "\\\\"))
    (map (lambda (i) (let ([s (string (integer->char i))]) (list s s)))
         '(#x7f #x80 #xe9 #x7ff #x800 #x2028 #x2029 #xd7ff #xe000
           #xfeff #xfffd #xfffe #xffff #x10000 #x1f600 #x10ffff))
    (list (list (string #\a #\" #\b #\\ #\c #\newline #\d (integer->char #x1F600) #\e #\x1)
                (string-append "a\\\"b\\\\c\\nd" (string (integer->char #x1F600)) "e\\u0001")))))

(test "52 writer cases give the expected text, which reads back"
  (check-equal? 52 (length writer-cases))
  (for-each
    (lambda (case)
      (let ([text (string-append "[\"" (cadr case) "\"]")] [v (vector (car case))])
        (check-equal? text (json->string v))
        (check-equal? (string->utf8 text) (json->bytevector v))
        (check-equal? v (read-both text))))
    writer-cases))

(test "keys are escaped as strings are"
  (check-equal? "{\"a\\\"b\\n\":1,\"\\u0000\":2}"
    (json->string (list (cons (string->symbol "a\"b\n") 1) (cons "\x0;" 2)))))

(define (bytes . items)
  (apply bytevector
    (apply append (map (lambda (x) (if (string? x) (bytevector->u8-list (string->utf8 x)) (list x)))
                       items))))

(test "invalid UTF-8 in byte input is refused"
  (for-each
    (lambda (sequence)
      (let ([input (apply bytes "[\"a" (append sequence (list "b\"]")))])
        (check (refused? (lambda () (bytevector->json input))))
        (check (refused? (lambda () (json-read (open-bytevector-input-port input)))))))
    '((#xC0 #x80) (#xC1 #xBF)
      (#xE0 #x80 #x80) (#xE0 #x9F #xBF)
      (#xF0 #x80 #x80 #x80) (#xF0 #x8F #xBF #xBF)
      (#xED #xA0 #x80) (#xED #xBF #xBF)
      (#xF4 #x90 #x80 #x80) (#xF5 #x80 #x80 #x80) (#xF8 #x88 #x80 #x80 #x80)
      (#xFC #x84 #x80 #x80 #x80 #x80) (#xFE) (#xFF)
      (#x80) (#xBF) (#xC3) (#xE2 #x82) (#xF0 #x9F #x98)
      (#xC3 #x41) (#xE2 #x41 #x82) (#xF0 #x9F #x41 #x80)))
  (check (refused? (lambda () (bytevector->json (bytes "[\"" #xE2 #x82)))))
  (check (refused? (lambda () (bytevector->json (bytes #xC3 #xA9)))))
  (check (refused? (lambda () (bytevector->json (bytes "[" #xC3 #xA9 "]"))))))

(test "the boundaries of valid UTF-8 are read"
  (for-each
    (lambda (case)
      (check-equal? (vector (string (integer->char (car case))))
        (bytevector->json (apply bytes "[\"" (append (cdr case) (list "\"]"))))))
    '((#x80 #xC2 #x80) (#x7FF #xDF #xBF)
      (#x800 #xE0 #xA0 #x80) (#xD7FF #xED #x9F #xBF) (#xE000 #xEE #x80 #x80)
      (#xFFFF #xEF #xBF #xBF) (#x10000 #xF0 #x90 #x80 #x80)
      (#x10FFFF #xF4 #x8F #xBF #xBF))))

(test "one leading byte order mark is skipped in byte input, and only there"
  (check-equal? '() (bytevector->json (bytes #xEF #xBB #xBF "{}")))
  (check-equal? 1 (bytevector->json (bytes #xEF #xBB #xBF " 1 ")))
  (check (refused? (lambda () (bytevector->json (bytes #xEF #xBB #xBF #xEF #xBB #xBF "{}")))))
  (check (refused? (lambda () (bytevector->json (bytes " " #xEF #xBB #xBF "{}")))))
  (check (refused? (lambda () (bytevector->json (bytes #xEF #xBB #xBF)))))
  (check (refused? (lambda () (bytevector->json (bytes #xEF #xBB "{}")))))
  (check (refused? (lambda () (string->json "\xFEFF;{}"))))
  (check-equal? (vector "\xFEFF;") (bytevector->json (bytes #xEF #xBB #xBF "[\"" #xEF #xBB #xBF "\"]"))))

(test "a string that starts with U+FEFF keeps it and what follows"
  (for-each
    (lambda (read)
      (check-equal? (vector "\xFEFF;a") (read (bytes "[\"" #xEF #xBB #xBF "a\"]")))
      (check-equal? (vector "\xFEFF;\xFEFF;ab\xe9;") (read (bytes "[\"" #xEF #xBB #xBF #xEF #xBB #xBF "ab" #xC3 #xA9 "\"]")))
      (check-equal? '(("\xFEFF;ab" . 1) ("\xFEFF;ab" . #("\xFEFF;x")))
        (parameterize ([json-allow-duplicate-keys? #t])
          (read (bytes "{\"" #xEF #xBB #xBF "ab\":1,\"" #xEF #xBB #xBF "ab\":[\"" #xEF #xBB #xBF "x\"]}"))))
      (check-equal? (vector "a\xFEFF;b") (read (bytes "[\"a" #xEF #xBB #xBF "b\"]"))))
    (list bytevector->json
          (lambda (bv) (json-read (open-bytevector-input-port bv)))))
  (check-equal? (vector "\xFEFF;a") (string->json "[\"\xFEFF;a\"]")))

(test "UTF-8 is written on binary ports"
  (let-values ([(port extract) (open-bytevector-output-port)])
    (json-write (vector "\xe9;\x1F600;") port)
    (check-equal? (bytes "[\"" #xC3 #xA9 #xF0 #x9F #x98 #x80 "\"]") (extract))))

(test-exit)
