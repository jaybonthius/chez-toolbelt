(define (write-error who value)
  (raise
    (condition
      (make-json-error #f)
      (make-who-condition who)
      (make-format-condition)
      (make-message-condition "~s cannot be written as JSON")
      (make-irritants-condition (list value)))))

(define (write-circular-error who value)
  (raise
    (condition
      (make-json-error #f)
      (make-who-condition who)
      (make-format-condition)
      (make-message-condition "~s is a circular list")
      (make-irritants-condition (list value)))))

(define hex-digits "0123456789abcdef")

(define fixnum-powers-of-ten
  (let ([v (make-fxvector 19)])
    (do ([i 0 (fx+ i 1)] [p 1 (* p 10)]) ((fx= i 19) v)
      (fxvector-set! v i p))))

;; 1233/4096 is just above log10(2), so the estimate from the bit length
;; is the digit count or one less.
(define (decimal-length n)
  (if (fx< n 10)
      1
      (let ([d (fxsrl (fx* (fxlength n) 1233) 12)])
        (if (fx>= n (fxvector-ref fixnum-powers-of-ten d)) (fx+ d 1) d))))

(define digit-pairs
  (let ([s (make-string 200)])
    (do ([i 0 (fx+ i 1)]) ((fx= i 100) s)
      (string-set! s (fx* 2 i) (integer->char (fx+ 48 (fxquotient i 10))))
      (string-set! s (fx+ (fx* 2 i) 1) (integer->char (fx+ 48 (fxremainder i 10)))))))

;; The writer is compiled once for each kind of output, so that writing a
;; unit compiles inline for that kind; bytes are written as UTF-8 directly
;; rather than copied from a finished string.
(define (value->json-string who v html? max-depth)
  (define make-text make-string)
  (define text-length string-length)
  (define text-copy! string-copy!)
  (define text-truncate! string-truncate!)
  (define-syntax text-set!
    (syntax-rules ()
      [(_ b k c) (string-set! b k c)]))
  (define (put-ascii! s)
    (put-text! s))
  ;; Room for the whole string and its quotes is reserved once, and again
  ;; for the rest after each escape, so the runs between need no check.
  (define (put-json-string! s)
    (let ([n (string-length s)])
      (reserve! (fx+ n 2))
      (string-set! buffer fill #\")
      (set! fill (fx+ fill 1))
      (let segment ([run 0])
        (let scan ([i run])
          (cond
            [(fx= i n)
             (let ([m (fx- n run)] [b buffer] [k fill])
               (string-copy! s run b k m)
               (string-set! b (fx+ k m) #\")
               (set! fill (fx+ k m 1)))]
            [(escape? (char->integer (string-ref s i)))
             (let ([m (fx- i run)])
               (string-copy! s run buffer fill m)
               (set! fill (fx+ fill m))
               (put-escape! (char->integer (string-ref s i)))
               (reserve! (fx- n i))
               (segment (fx+ i 1)))]
            [else (scan (fx+ i 1))])))))
  (include "json/emit.ss"))

(define (value->json-bytevector who v html? max-depth)
  (define make-text make-bytevector)
  (define text-length bytevector-length)
  (define text-copy! bytevector-copy!)
  (define text-truncate! bytevector-truncate!)
  (define-syntax text-set!
    (syntax-rules ()
      [(_ b k c) (bytevector-u8-set! b k (char->integer c))]))
  (define (put-ascii! s)
    (let ([n (string-length s)])
      (reserve! n)
      (do ([i 0 (fx+ i 1)]) ((fx= i n))
        (bytevector-u8-set! buffer (fx+ fill i) (char->integer (string-ref s i))))
      (set! fill (fx+ fill n))))
  ;; Room is reserved for one byte per character and the quotes, and again
  ;; after each escape and each character that takes more than one byte,
  ;; so ASCII characters need no check.
  (define (put-json-string! s)
    (let ([n (string-length s)])
      (reserve! (fx+ n 2))
      (bytevector-u8-set! buffer fill 34)
      (let loop ([i 0] [k (fx+ fill 1)])
        (if (fx= i n)
            (begin
              (bytevector-u8-set! buffer k 34)
              (set! fill (fx+ k 1)))
            (let ([u (char->integer (string-ref s i))])
              (cond
                [(escape? u)
                 (set! fill k)
                 (put-escape! u)
                 (reserve! (fx- n i))
                 (loop (fx+ i 1) fill)]
                [(fx< u #x80)
                 (bytevector-u8-set! buffer k u)
                 (loop (fx+ i 1) (fx+ k 1))]
                [else
                 (set! fill k)
                 (reserve! (fx+ (fx- n i) 4))
                 (let ([b buffer] [k fill])
                   (cond
                     [(fx< u #x800)
                      (bytevector-u8-set! b k (fxior #xC0 (fxsrl u 6)))
                      (bytevector-u8-set! b (fx+ k 1) (fxior #x80 (fxand u #x3F)))
                      (loop (fx+ i 1) (fx+ k 2))]
                     [(fx< u #x10000)
                      (bytevector-u8-set! b k (fxior #xE0 (fxsrl u 12)))
                      (bytevector-u8-set! b (fx+ k 1) (fxior #x80 (fxand (fxsrl u 6) #x3F)))
                      (bytevector-u8-set! b (fx+ k 2) (fxior #x80 (fxand u #x3F)))
                      (loop (fx+ i 1) (fx+ k 3))]
                     [else
                      (bytevector-u8-set! b k (fxior #xF0 (fxsrl u 18)))
                      (bytevector-u8-set! b (fx+ k 1) (fxior #x80 (fxand (fxsrl u 12) #x3F)))
                      (bytevector-u8-set! b (fx+ k 2) (fxior #x80 (fxand (fxsrl u 6) #x3F)))
                      (bytevector-u8-set! b (fx+ k 3) (fxior #x80 (fxand u #x3F)))
                      (loop (fx+ i 1) (fx+ k 4))]))]))))))
  (include "json/emit.ss"))

(define (json->string v)
  (value->json-string 'json->string v (json-escape-html?) (json-max-depth)))

(define (json->bytevector v)
  (value->json-bytevector 'json->bytevector v (json-escape-html?) (json-max-depth)))

;; The text is built whole before any of it reaches the port, so a value
;; that cannot be written leaves the port untouched.
(define json-write
  (case-lambda
    [(v) (json-write v (current-output-port))]
    [(v port)
     (unless (output-port? port)
       (assertion-violationf 'json-write "~s is not an output port" port))
     (if (binary-port? port)
         (put-bytevector port
           (value->json-bytevector 'json-write v (json-escape-html?) (json-max-depth)))
         (put-string port
           (value->json-string 'json-write v (json-escape-html?) (json-max-depth))))]))
