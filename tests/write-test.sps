(import (chezscheme) (json) (check))

(define-record-type point (fields x y))

(define (string-keys v)
  (cond
    [(vector? v) (vector-map string-keys v)]
    [(pair? v)
     (map (lambda (m)
            (cons (if (symbol? (car m)) (symbol->string (car m)) (car m)) (string-keys (cdr m))))
          v)]
    [else v]))

(define (written-both v)
  (let ([text (json->string v)])
    (check-equal? (string->utf8 text) (json->bytevector v))
    text))

(test "values are written compactly, keys in the order given"
  (check-equal? "{\"b\":1,\"a\":[1,\"x\",true,false,null],\"c\":{},\"d\":[]}"
    (written-both '(("b" . 1) ("a" . #(1 "x" #t #f null)) ("c") ("d" . #()))))
  (check-equal? "{\"a\":1,\"a\":2}" (written-both '(("a" . 1) ("a" . 2))))
  (check-equal? "{\"k\":1,\"s\":2}" (written-both (list '(k . 1) (cons "s" 2))))
  (check-equal? "{}" (json->string '()))
  (check-equal? "[]" (json->string (vector)))
  (check-equal? "\"\"" (json->string ""))
  (check-equal? "null" (json->string 'null))
  (check-equal? "[-1,0,12345678901234567890123,-12345678901234567890123]"
    (json->string (vector -1 0 12345678901234567890123 -12345678901234567890123)))
  (check-equal? "[[[1]],{\"a\":{\"b\":[2]}}]" (written-both '#(#(#(1)) (("a" . (("b" . #(2)))))))))

(test "the writer refuses nesting past json-max-depth and circular values"
  (define (nested-vectors n) (if (= n 0) 1 (vector (nested-vectors (- n 1)))))
  (define (nested-objects n) (if (= n 0) 1 (list (cons "a" (nested-objects (- n 1))))))
  (define (refused-nesting? thunk)
    (guard (c [(json-error? c)
               (and (not (json-error-position c))
                    (equal? (list (json-max-depth)) (condition-irritants c)))])
      (thunk)
      #f))
  (check-equal? (string-append (make-string 1000 #\[) "1" (make-string 1000 #\]))
    (json->string (nested-vectors 1000)))
  (check (pair? (string->json (json->string (nested-objects 1000)))))
  (check (refused-nesting? (lambda () (json->string (nested-vectors 1001)))))
  (check (refused-nesting? (lambda () (json->bytevector (nested-objects 1001)))))
  (check (refused-nesting? (lambda () (json->string (nested-vectors 1000000)))))
  (parameterize ([json-max-depth 2])
    (check-equal? "[[1]]" (json->string (nested-vectors 2)))
    (check (refused-nesting? (lambda () (json->string (nested-vectors 3)))))
    (check (refused-nesting? (lambda () (json-write (nested-objects 3) (open-output-string))))))
  (parameterize ([json-max-depth 0])
    (check-equal? "1" (json->string 1))
    (check (refused-nesting? (lambda () (json->string (vector)))))
    (check (refused-nesting? (lambda () (json->string '())))))
  (parameterize ([json-max-depth 1])
    (check-equal? "[1]" (json->string (vector 1)))
    (check (refused-nesting? (lambda () (json->string (vector '())))))
    (check (refused-nesting? (lambda () (json->bytevector (vector (vector)))))))
  (let ([v (vector 1 2)])
    (vector-set! v 1 v)
    (check (refused-nesting? (lambda () (json->string v))))
    (check (refused-nesting? (lambda () (json->bytevector v)))))
  (let ([object (list (cons "a" 1))])
    (set-cdr! (car object) (vector object))
    (check (refused-nesting? (lambda () (json->string object)))))
  (for-each
    (lambda (length)
      (let ([object (map (lambda (i) (cons (format "k~a" i) i)) (iota length))])
        (set-cdr! (last-pair object) object)
        (for-each
          (lambda (write)
            (guard (c [(json-error? c)
                       (check (not (json-error-position c)))
                       (check-equal? (list object) (condition-irritants c))])
              (write object)
              (check (not "written"))))
          (list json->string json->bytevector
                (lambda (v) (json-write v (open-output-string)))))))
    '(1 2 3 10 101)))

(test "fixnums of every length are written as number->string writes them"
  (do ([i 0 (+ i 1)]) ((= i 20000))
    (let* ([n (random (expt 10 (+ 1 (mod i 19))))] [n (if (odd? i) (- n) n)])
      (when (fixnum? n)
        (check-equal? (number->string n) (json->string n)))))
  (for-each
    (lambda (n) (check-equal? (number->string n) (json->string n)))
    (list 0 9 10 99 100 999999999999999999 1000000000000000000
          (most-positive-fixnum) (most-negative-fixnum) (+ (most-negative-fixnum) 1))))

(test "the caller's print settings do not change the text"
  (parameterize ([print-precision 3] [print-subnormal-precision #t] [print-radix 16])
    (check-equal? "[3.141592653589793,5e-324,255]" (json->string (vector 3.141592653589793 5e-324 255)))))

(define (refused who thunk value)
  (guard (c [(json-error? c)
             (check-equal? #f (json-error-position c))
             (check-equal? who (condition-who c))
             (check-equal? (list value) (condition-irritants c))])
    (thunk)
    (check (not "written"))))

(test "values outside the mapping are refused"
  (for-each
    (lambda (v)
      (refused 'json->string (lambda () (json->string v)) v)
      (refused 'json->bytevector (lambda () (json->bytevector v)) v)
      (refused 'json-write (lambda () (json-write v (open-output-string))) v)
      (refused 'json->string (lambda () (json->string (vector 1 v))) v))
    (list +inf.0 -inf.0 +nan.0 1/2 -3/4 1+2i 1.5+2.5i 'foo (string->symbol "null ") #\a
      (void) (eof-object) (bytevector 1) (make-point 1 2) car (box 1) (make-eq-hashtable)
      '(1 2) '((a . 1) 2) '((a . 1) . 5) '((1 . 2)) '(("a" . 1) (#\b . 2)) '(a b)))
  (let ([bad '((a . 1) . 5)])
    (refused 'json->string (lambda () (json->string (list (cons 'k bad)))) bad)))

(test "a value that is refused leaves the port untouched"
  (let ([port (open-output-string)])
    (refused 'json-write (lambda () (json-write (vector 1 2 +nan.0) port)) +nan.0)
    (check-equal? "" (get-output-string port)))
  (let-values ([(port extract) (open-bytevector-output-port)])
    (refused 'json-write (lambda () (json-write (vector "a" 'b) port)) 'b)
    (check-equal? (bytevector) (extract))))

(test "json-write writes to textual and binary ports, and the current output port"
  (let ([port (open-output-string)])
    (json-write '((a . #(1 "\xe9;"))) port)
    (json-write 2 port)
    (check-equal? "{\"a\":[1,\"\xe9;\"]}2" (get-output-string port)))
  (let-values ([(port extract) (open-bytevector-output-port)])
    (json-write '((a . #(1 "\xe9;"))) port)
    (check-equal? (string->utf8 "{\"a\":[1,\"\xe9;\"]}") (extract)))
  (check-equal? "[true]"
    (with-output-to-string (lambda () (json-write (vector #t)))))
  (check-equal? (string->utf8 "[\"\x1F600;\"]") (json->bytevector (vector "\x1F600;")))
  (check-raises assertion-violation? (json-write 1 (open-string-input-port "")))
  (check-raises assertion-violation? (json-write 1 'port)))

(test "json-escape-html? escapes < > & U+2028 and U+2029"
  (let ([v (list (cons "</script>&" "<!--\x2028;\x2029;-->"))])
    (check-equal? "{\"</script>&\":\"<!--\x2028;\x2029;-->\"}" (json->string v))
    (parameterize ([json-escape-html? #t])
      (check-equal?
        "{\"\\u003c/script\\u003e\\u0026\":\"\\u003c!--\\u2028\\u2029--\\u003e\"}"
        (json->string v))
      (check-equal? v (string->json (written-both v)))
      (check-equal? "[\"\\n\\u0001\\\"\"]" (json->string (vector "\n\x1;\""))))
    (check-equal? #t (parameterize ([json-escape-html? 'yes]) (json-escape-html?)))
    (check-equal? #f (json-escape-html?))))

(test "keys repeated in a large value are written the same each time"
  (let* ([keys (list "a<b" (string->symbol "q\"\n") 'plain)]
         [v (make-vector 100 (map (lambda (k) (cons k 1)) keys))]
         [object "{\"a<b\":1,\"q\\\"\\n\":1,\"plain\":1}"]
         [html-object "{\"a\\u003cb\":1,\"q\\\"\\n\":1,\"plain\":1}"]
         [joined (lambda (o)
                   (string-append "[" o
                     (apply string-append (map (lambda (i) (string-append "," o)) (iota 99)))
                     "]"))])
    (check-equal? (joined object) (written-both v))
    (parameterize ([json-escape-html? #t])
      (check-equal? (joined html-object) (written-both v)))
    (check-equal? (string-keys v) (string->json (json->string v)))))

(test "a large value is written as its parts are"
  (let* ([long (apply string-append (make-list 3000 "ab\"c\n\x1F600;<"))]
         [parts (append
                  (map (lambda (i)
                         (list (cons (string->symbol (format "k~a" (mod i 70))) i)
                               (cons "x" (* i 1.5))
                               (cons "q\"\xe9;" (vector "s\x1F600;" #t 'null))))
                       (iota 3000))
                  (list long (vector long long) 123456789012345678901234567890))]
         [joined (lambda (texts)
                   (string-append "[" (car texts)
                     (apply string-append (map (lambda (t) (string-append "," t)) (cdr texts)))
                     "]"))])
    (check-equal? (joined (map json->string parts)) (written-both (list->vector parts)))
    (parameterize ([json-escape-html? #t])
      (check-equal? (joined (map json->string parts)) (written-both (list->vector parts))))
    (check-equal? (string-keys (list->vector parts))
      (string->json (json->string (list->vector parts))))
    (check-equal? (string-keys (list->vector parts))
      (bytevector->json (json->bytevector (list->vector parts))))))

(test "the writer does not limit the digits of an integer"
  (let ([text (json->string (expt 10 5000))])
    (check-equal? 5001 (string-length text))
    (check-raises json-error? (string->json text))
    (check-equal? (expt 10 5000)
      (parameterize ([json-max-number-digits 5001]) (string->json text)))))

(test "keys made one per object are written the same past the key table's size"
  (let* ([objects (map (lambda (i)
                         (list (cons (string-copy "name") i)
                               (cons (format "k~a" (mod i 300)) "\xe9;")
                               (cons 'shared (vector))))
                       (iota 2000))]
         [texts (map json->string objects)])
    (check-equal? (string-append "[" (car texts)
                    (apply string-append (map (lambda (t) (string-append "," t)) (cdr texts)))
                    "]")
      (written-both (list->vector objects)))))

(test "what is written reads back equal"
  (for-each
    (lambda (v)
      (check-equal? v (string->json (json->string v)))
      (check-equal? v (bytevector->json (json->bytevector v)))
      (let-values ([(port extract) (open-bytevector-output-port)])
        (json-write v port)
        (check-equal? v (json-read (open-bytevector-input-port (extract))))))
    (list '(("name" . "Ada") ("age" . 36) ("tags" . #("x" "y")) ("ratio" . 0.25) ("none" . null))
          '#(-0.0 0.0 1e300 -1e-300 5e-324)
          (list (cons "\"\\\n" "\x0;\x7f;\x2028;"))
          (list (cons "\xFEFF;a" "\xFEFF;b\x80;\x7FF;\x800;\xFFFF;\x10000;\x10FFFF;")))))

(test-exit)
