(define (read-error who position message . irritants)
  (raise
    (condition
      (make-json-error position)
      (make-who-condition who)
      (make-format-condition)
      (make-message-condition
        (string-append message " at position " (number->string position)))
      (make-irritants-condition irritants))))

(define-record-type object-frame
  (nongenerative)
  (sealed #t)
  (fields base (mutable hashes) (mutable late-positions)))

;; Scanning the keys read so far is quadratic, so objects with more keys
;; are checked for duplicates by sorting once they are read.
(define duplicate-scan-limit 32)

(define small-key-cache-size 32)
(define large-key-cache-size 256)

(define (key-hash-step h u)
  (fxand (fx+ (fx* h 31) u) #x3FFFFFF))

(define flonum-powers-of-ten
  (let ([v (make-flvector 23)])
    (do ([i 0 (fx+ i 1)] [x 1.0 (fl* x 10.0)]) ((fx= i 23) v)
      (flvector-set! v i x))))

(define powers-of-two
  (let ([v (make-flvector 2098)])
    (do ([i 1074 (fx- i 1)] [x 1.0 (fl/ x 2.0)]) ((fx< i 0))
      (flvector-set! v i x))
    (do ([i 1074 (fx+ i 1)] [x 1.0 (fl* x 2.0)]) ((fx= i 2098))
      (flvector-set! v i x))
    v))

;; The quotient is rounded once, at the precision of the result: 53 bits,
;; or fewer for a subnormal. Converting a wider quotient to a flonum and
;; then scaling it would round twice.
(define (ratio->flonum a b)
  (let* ([s (fx- 55 (fx- (bitwise-length a) (bitwise-length b)))]
         [num (if (fx> s 0) (bitwise-arithmetic-shift-left a s) a)]
         [den (if (fx< s 0) (bitwise-arithmetic-shift-left b (fx- s)) b)])
    (let-values ([(q r) (div-and-mod num den)])
      (let* ([lq (fxlength q)]
             [shift (fxmax (fx- lq 53) (fx- s 1074))])
        (if (fx> shift lq)
            0.0
            (let* ([mantissa (fxsrl q shift)]
                   [rest (fxand q (fx- (fxsll 1 shift) 1))]
                   [half (fxsll 1 (fx- shift 1))]
                   [mantissa (if (or (fx> rest half)
                                     (and (fx= rest half)
                                          (or (not (eqv? r 0)) (fxodd? mantissa))))
                                 (fx+ mantissa 1)
                                 mantissa)]
                   [e (fx- shift s)])
              (if (fx> e 1023)
                  +inf.0
                  (fl* (fixnum->flonum mantissa) (flvector-ref powers-of-two (fx+ e 1074))))))))))

(define (decimal->flonum n e)
  (cond
    [(eqv? n 0) 0.0]
    ;; n and 10^e are both exact flonums, so one operation rounds correctly
    ;; (Clinger's fast path).
    [(and (fixnum? n) (fx<= n 9007199254740992) (fx<= -22 e 22))
     (if (fx< e 0)
         (fl/ (fixnum->flonum n) (flvector-ref flonum-powers-of-ten (fx- e)))
         (fl* (fixnum->flonum n) (flvector-ref flonum-powers-of-ten e)))]
    [(and (fixnum? n) (fx<= eisel-lemire-q-min e 308)) (eisel-lemire n e)]
    [else
     ;; Overflow and underflow to zero are decided from the bit length of n,
     ;; without computing 10^e, whose size only the saturation of e bounds.
     (let ([bits (bitwise-length n)] [log10-2 0.3010299956639812])
       (cond
         [(fl>= (fl+ (fl* (fixnum->flonum (fx- bits 1)) log10-2) (fixnum->flonum e)) 309.0) +inf.0]
         [(fl< (fl+ (fl* (fixnum->flonum bits) log10-2) (fixnum->flonum e)) -325.0) 0.0]
         [(fx>= e 0) (ratio->flonum (* n (power-of-ten e)) 1)]
         [else (ratio->flonum n (power-of-ten (fx- e)))]))]))

(define exact-powers-of-ten
  (let ([v (make-vector 400)])
    (do ([k 0 (fx+ k 1)] [p 1 (* p 10)]) ((fx= k 400) v)
      (vector-set! v k p))))

(define (power-of-ten k)
  (if (fx< k 400) (vector-ref exact-powers-of-ten k) (expt 10 k)))

;; Each kind of input includes the one grammar, so that reading a unit
;; compiles inline for that kind.
(define (parse-string who src start end offset max-depth max-digits duplicates? whole?)
  (define-syntax unit
    (syntax-rules ()
      [(_ e) (let ([i e]) (if (fx< i end) (char->integer (string-ref src i)) -1))]))
  (define (unit-value u) (integer->char u))
  (define (key-hash s)
    (let ([n (string-length s)])
      (let loop ([k 0] [h 0])
        (if (fx= k n)
            h
            (loop (fx+ k 1) (key-hash-step h (char->integer (string-ref s k))))))))
  (define (key-text=? key i j)
    (let ([n (string-length key)])
      (and (fx= n (fx- j i))
           (let loop ([k 0])
             (or (fx= k n)
                 (and (char=? (string-ref key k) (string-ref src (fx+ i k)))
                      (loop (fx+ k 1))))))))
  (define (read-string-body i)
    (let scan ([j i])
      (let ([u (unit j)])
        (cond
          [(fx= u 34) (values (substring src i j) (fx+ j 1))]
          [(or (fx= u 92) (fx< u 32))
           (let ([n (fx- j i)])
             (buffer-reserve! n)
             (string-copy! src i buffer 0 n)
             (read-string-rest j n))]
          [else (scan (fx+ j 1))]))))
  (define (read-non-ascii u i)
    (values (integer->char u) (fx+ i 1)))
  (define (read-plain-key i j)
    (values (substring src i j) (fx+ j 1)))
  (include "json/parse.ss"))

(define (parse-bytevector who src start end offset max-depth max-digits duplicates? whole?)
  (define-syntax unit
    (syntax-rules ()
      [(_ e) (let ([i e]) (if (fx< i end) (bytevector-u8-ref src i) -1))]))
  (define (unit-value u) (if (fx< u 128) (integer->char u) u))
  (define (key-hash s)
    (let ([bytes (string->utf8 s)])
      (let ([n (bytevector-length bytes)])
        (let loop ([k 0] [h 0])
          (if (fx= k n)
              h
              (loop (fx+ k 1) (key-hash-step h (bytevector-u8-ref bytes k))))))))
  (define (key-text=? key i j)
    (define (byte= p b) (and (fx< p j) (fx= (bytevector-u8-ref src p) b)))
    (let ([n (string-length key)])
      (let loop ([k 0] [p i])
        (if (fx= k n)
            (fx= p j)
            (let ([c (char->integer (string-ref key k))])
              (cond
                [(fx< c #x80) (and (byte= p c) (loop (fx+ k 1) (fx+ p 1)))]
                [(fx< c #x800)
                 (and (byte= p (fxior #xC0 (fxsrl c 6)))
                      (byte= (fx+ p 1) (fxior #x80 (fxand c #x3F)))
                      (loop (fx+ k 1) (fx+ p 2)))]
                [(fx< c #x10000)
                 (and (byte= p (fxior #xE0 (fxsrl c 12)))
                      (byte= (fx+ p 1) (fxior #x80 (fxand (fxsrl c 6) #x3F)))
                      (byte= (fx+ p 2) (fxior #x80 (fxand c #x3F)))
                      (loop (fx+ k 1) (fx+ p 3)))]
                [else
                 (and (byte= p (fxior #xF0 (fxsrl c 18)))
                      (byte= (fx+ p 1) (fxior #x80 (fxand (fxsrl c 12) #x3F)))
                      (byte= (fx+ p 2) (fxior #x80 (fxand (fxsrl c 6) #x3F)))
                      (byte= (fx+ p 3) (fxior #x80 (fxand c #x3F)))
                      (loop (fx+ k 1) (fx+ p 4)))]))))))
  ;; A string without escapes is validated as UTF-8 on the first pass and
  ;; then decoded by utf8->string, which is several times faster than a
  ;; loop here. utf8->string drops one leading byte order mark, which is
  ;; put back.
  (define (read-string-body i)
    (let scan ([j i] [n 0])
      (let ([u (unit j)])
        (cond
          [(fx= u 34)
           (let ([bytes (make-bytevector (fx- j i))])
             (bytevector-copy! src i bytes 0 (fx- j i))
             (values
               (if (and (fx= (unit i) #xEF) (fx= (unit (fx+ i 1)) #xBB) (fx= (unit (fx+ i 2)) #xBF))
                   (string-append "\xFEFF;" (utf8->string bytes))
                   (utf8->string bytes))
               (fx+ j 1)))]
          [(or (fx= u 92) (fx< u 32))
           (let loop ([j i] [k 0])
             (if (fx= k n)
                 (read-string-rest j n)
                 (let ([u (bytevector-u8-ref src j)])
                   (if (fx< u 128)
                       (begin
                         (buffer-put! k (integer->char u))
                         (loop (fx+ j 1) (fx+ k 1)))
                       (let-values ([(c j) (read-non-ascii u j)])
                         (buffer-put! k c)
                         (loop j (fx+ k 1)))))))]
          [(fx>= u 128)
           (let-values ([(c next) (read-non-ascii u j)])
             (scan next (fx+ n 1)))]
          [else (scan (fx+ j 1) (fx+ n 1))]))))
  (define (read-plain-key i j)
    (read-string-body i))
  ;; Bytes outside the well-formed UTF-8 sequences of the Unicode
  ;; Standard, table 3-7, are refused: overlong forms, encoded surrogates
  ;; and values above U+10FFFF.
  (define (read-non-ascii u i)
    (define (continuation k low high)
      (let ([b (unit (fx+ i k))])
        (if (fx<= low b high)
            (fxand b #x3F)
            (fail i "invalid UTF-8 sequence starting with byte #x~x" u))))
    (cond
      [(fx< u #xC2) (fail i "invalid UTF-8 sequence starting with byte #x~x" u)]
      [(fx< u #xE0)
       (values (integer->char (fxior (fxsll (fxand u #x1F) 6) (continuation 1 #x80 #xBF)))
         (fx+ i 2))]
      [(fx< u #xF0)
       (let* ([b1 (case u
                    [(#xE0) (continuation 1 #xA0 #xBF)]
                    [(#xED) (continuation 1 #x80 #x9F)]
                    [else (continuation 1 #x80 #xBF)])]
              [b2 (continuation 2 #x80 #xBF)])
         (values (integer->char (fxior (fxsll (fxand u #x0F) 12) (fxsll b1 6) b2))
           (fx+ i 3)))]
      [(fx< u #xF5)
       (let* ([b1 (case u
                    [(#xF0) (continuation 1 #x90 #xBF)]
                    [(#xF4) (continuation 1 #x80 #x8F)]
                    [else (continuation 1 #x80 #xBF)])]
              [b2 (continuation 2 #x80 #xBF)]
              [b3 (continuation 3 #x80 #xBF)])
         (values (integer->char (fxior (fxsll (fxand u #x07) 18) (fxsll b1 12) (fxsll b2 6) b3))
           (fx+ i 4)))]
      [else (fail i "invalid UTF-8 sequence starting with byte #x~x" u)]))
  (include "json/parse.ss"))

(define (json-whitespace? u)
  (or (fx= u 32) (fx= u 10) (fx= u 13) (fx= u 9)))

;; A top-level number or literal ends at one of these, so that "1x" or
;; "truex" is read whole and refused rather than read as two values.
(define (json-delimiter? u)
  (or (json-whitespace? u)
      (fx= u 91) (fx= u 93) (fx= u 123) (fx= u 125)
      (fx= u 44) (fx= u 58) (fx= u 34)))

;; What follows a value stays in the port, so the port's buffer is read in
;; place and its index set just past the value.
(define (read-textual-port port max-depth max-digits duplicates?)
  (define-syntax unit-at
    (syntax-rules ()
      [(_ buf j) (char->integer (string-ref buf j))]))
  (define port-buffer textual-port-input-buffer)
  (define port-index textual-port-input-index)
  (define port-size textual-port-input-size)
  (define set-port-index! set-textual-port-input-index!)
  (define lookahead lookahead-char)
  (define get get-char)
  (define unit-code char->integer)
  (define open-text-output open-string-output-port)
  (define put-unit put-char)
  (define make-text make-string)
  (define text-copy! string-copy!)
  (define text-length string-length)
  (define parse parse-string)
  (define (skip-byte-order-mark) 0)
  (include "json/port.ss"))

(define (read-binary-port port max-depth max-digits duplicates?)
  (define-syntax unit-at
    (syntax-rules ()
      [(_ buf j) (bytevector-u8-ref buf j)]))
  (define port-buffer binary-port-input-buffer)
  (define port-index binary-port-input-index)
  (define port-size binary-port-input-size)
  (define set-port-index! set-binary-port-input-index!)
  (define lookahead lookahead-u8)
  (define get get-u8)
  (define (unit-code u) u)
  (define open-text-output open-bytevector-output-port)
  (define put-unit put-u8)
  (define make-text make-bytevector)
  (define text-copy! bytevector-copy!)
  (define text-length bytevector-length)
  (define parse parse-bytevector)
  ;; A byte order mark is skipped only at the start of the input: a port
  ;; read from elsewhere may hold several values, and those bytes in the
  ;; middle are an error.
  (define (skip-byte-order-mark)
    (if (and (port-has-port-position? port)
             (eqv? (port-position port) 0)
             (eqv? (lookahead-u8 port) #xEF))
        (begin
          (get-u8 port)
          (unless (and (eqv? (get-u8 port) #xBB) (eqv? (get-u8 port) #xBF))
            (read-error 'json-read 0 "invalid UTF-8 sequence starting with byte #x~x" #xEF))
          3)
        0))
  (include "json/port.ss"))

(define json-read
  (case-lambda
    [() (json-read (current-input-port))]
    [(port)
     (unless (input-port? port)
       (assertion-violationf 'json-read "~s is not an input port" port))
     (if (binary-port? port)
         (read-binary-port port (json-max-depth) (json-max-number-digits)
           (json-allow-duplicate-keys?))
         (read-textual-port port (json-max-depth) (json-max-number-digits)
           (json-allow-duplicate-keys?)))]))

(define (string->json s)
  (unless (string? s)
    (assertion-violationf 'string->json "~s is not a string" s))
  (let-values ([(v end) (parse-string 'string->json s 0 (string-length s) 0
                         (json-max-depth) (json-max-number-digits) (json-allow-duplicate-keys?) #t)])
    v))

(define (bytevector->json bv)
  (unless (bytevector? bv)
    (assertion-violationf 'bytevector->json "~s is not a bytevector" bv))
  (let* ([end (bytevector-length bv)]
         [start (if (and (fx>= end 3)
                         (fx= (bytevector-u8-ref bv 0) #xEF)
                         (fx= (bytevector-u8-ref bv 1) #xBB)
                         (fx= (bytevector-u8-ref bv 2) #xBF))
                    3
                    0)])
    (let-values ([(v end) (parse-bytevector 'bytevector->json bv start end 0
                            (json-max-depth) (json-max-number-digits) (json-allow-duplicate-keys?)
                            #t)])
      v)))
