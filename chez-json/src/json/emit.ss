(define buffer (make-text 256))
(define fill 0)
(define full-chunks '())
(define scratch (make-bytevector 8))

;; The buffer doubles up to a chunk's size; past it, full chunks are set
;; aside and joined at the end. A large text then costs one allocation of
;; its own size, where a buffer doubled to hold all of it allocates and
;; touches about twice that, in large objects, and is three times slower
;; to fill with 2 MB of text.
(define chunk-size 16384)
(define (reserve! k)
  (let ([need (fx+ fill k)] [size (text-length buffer)])
    (when (fx> need size)
      (if (fx< size chunk-size)
          (let ([new (make-text (fxmax need (fx* 2 size)))])
            (text-copy! buffer 0 new 0 fill)
            (set! buffer new))
          (begin
            (set! full-chunks (cons (text-truncate! buffer fill) full-chunks))
            (set! buffer (make-text (fxmax k chunk-size)))
            (set! fill 0))))))

(define (text)
  (if (null? full-chunks)
      (text-truncate! buffer fill)
      (let* ([chunks (reverse (cons (text-truncate! buffer fill) full-chunks))]
             [result (make-text
                       (fold-left (lambda (n chunk) (fx+ n (text-length chunk))) 0 chunks))])
        (fold-left
          (lambda (k chunk)
            (text-copy! chunk 0 result k (text-length chunk))
            (fx+ k (text-length chunk)))
          0 chunks)
        result)))

(define (put-char! c)
  (reserve! 1)
  (text-set! buffer fill c)
  (set! fill (fx+ fill 1)))

(define (put-text! t)
  (let ([k (text-length t)])
    (reserve! k)
    (text-copy! t 0 buffer fill k)
    (set! fill (fx+ fill k))))

(define (put-digits! end f count)
  (let ([b buffer])
    (let loop ([f f] [k end] [count count])
      (cond
        [(fx>= count 2)
         (let* ([q (fxquotient f 100)] [r (fx* 2 (fx- f (fx* q 100)))])
           (text-set! b (fx- k 1) (string-ref digit-pairs (fx+ r 1)))
           (text-set! b (fx- k 2) (string-ref digit-pairs r))
           (loop q (fx- k 2) (fx- count 2)))]
        [(fx= count 1)
         (let ([q (fxquotient f 10)])
           (text-set! b (fx- k 1) (integer->char (fx+ 48 (fx- f (fx* q 10)))))
           q)]
        [else f]))))

(define (put-decimal! f n fraction-digits)
  (reserve! (fx+ n 1))
  (let ([end (fx+ fill n (if (fx> fraction-digits 0) 1 0))])
    (if (fx> fraction-digits 0)
        (let ([point (fx- end fraction-digits 1)])
          (text-set! buffer point #\.)
          (put-digits! point (put-digits! end f fraction-digits) (fx- n fraction-digits)))
        (put-digits! end f n))
    (set! fill end)))

(define (put-zeros! n)
  (do ([i 0 (fx+ i 1)]) ((fx= i n))
    (put-char! #\0)))

(define (put-fixnum! n)
  (if (fx< n 0)
      (if (fx= n (most-negative-fixnum))
          (put-ascii! (number->string n))
          (begin (put-char! #\-) (put-fixnum! (fx- n))))
      (put-decimal! n (decimal-length n) 0)))

;; The same text as number->string with print-subnormal-precision off,
;; except where two shortest decimals are equally near: the even one here.
(define (put-flonum! x)
  (cond
    [(fl= x 0.0) (put-ascii! (if (eqv? x -0.0) "-0.0" "0.0"))]
    [(fl< x 0.0) (put-char! #\-) (put-flonum! (fl- x))]
    [else
     (let-values ([(f e) (flonum->decimal x scratch)])
       (let strip ([f f] [e e])
         (if (fx= 0 (fxremainder f 10))
             (strip (fxquotient f 10) (fx+ e 1))
             (let* ([n (decimal-length f)] [exponent (fx+ e n -1)])
               (cond
                 [(fx<= 0 exponent 9)
                  (if (fx> n (fx+ exponent 1))
                      (put-decimal! f n (fx- n exponent 1))
                      (begin
                        (put-decimal! f n 0)
                        (put-zeros! (fx- exponent n -1))
                        (put-ascii! ".0")))]
                 [(fx<= -3 exponent -1)
                  (put-ascii! "0.")
                  (put-zeros! (fx- -1 exponent))
                  (put-decimal! f n 0)]
                 [else
                  (put-decimal! f n (fx- n 1))
                  (put-char! #\e)
                  (put-fixnum! exponent)])))))]))

(define (escape? u)
  (if html?
      (or (fx< u 32) (fx= u 34) (fx= u 92)
          (fx= u 60) (fx= u 62) (fx= u 38) (fx= u #x2028) (fx= u #x2029))
      (or (fx< u 32) (fx= u 34) (fx= u 92))))

(define (put-escape! u)
  (case u
    [(34) (put-ascii! "\\\"")]
    [(92) (put-ascii! "\\\\")]
    [(8) (put-ascii! "\\b")]
    [(12) (put-ascii! "\\f")]
    [(10) (put-ascii! "\\n")]
    [(13) (put-ascii! "\\r")]
    [(9) (put-ascii! "\\t")]
    [else
     (put-ascii! "\\u")
     (put-char! (string-ref hex-digits (fxsrl u 12)))
     (put-char! (string-ref hex-digits (fxand (fxsrl u 8) 15)))
     (put-char! (string-ref hex-digits (fxand (fxsrl u 4) 15)))
     (put-char! (string-ref hex-digits (fxand u 15)))]))

;; Keys repeat in large values, so the written text of each key is kept
;; and copied on later uses; the table is made only after 64 keys, since
;; in a small value it costs more than it saves. It is keyed by identity,
;; and keys made one per object are all distinct, so it stops growing at
;; 256 entries: past that, a table that misses only grows and slows the
;; writer.
(define key-texts #f)
(define keys-written 0)
(define (put-key! key)
  (if key-texts
      (let ([text (hashtable-ref key-texts key #f)])
        (if text
            (put-text! text)
            (let ([start fill] [chunks full-chunks])
              (put-json-string! (if (symbol? key) (symbol->string key) key))
              (when (and (eq? chunks full-chunks) (fx< (hashtable-size key-texts) 256))
                (let ([text (make-text (fx- fill start))])
                  (text-copy! buffer start text 0 (fx- fill start))
                  (hashtable-set! key-texts key text))))))
      (begin
        (set! keys-written (fx+ keys-written 1))
        (when (fx= keys-written 64) (set! key-texts (make-eq-hashtable)))
        (put-json-string! (if (symbol? key) (symbol->string key) key)))))

(define (refuse-nesting)
  (raise
    (condition
      (make-json-error #f)
      (make-who-condition who)
      (make-format-condition)
      (make-message-condition "nesting deeper than ~s")
      (make-irritants-condition (list max-depth)))))

(define (put-value! v depth)
  (cond
    [(string? v) (put-json-string! v)]
    [(fixnum? v) (put-fixnum! v)]
    [(flonum? v)
     (unless (and (fl< v +inf.0) (fl> v -inf.0)) (write-error who v))
     (put-flonum! v)]
    [(pair? v)
     (when (fx= depth max-depth) (refuse-nesting))
     (put-char! #\{)
     (let loop ([p v] [slow v] [first? #t] [move-slow? #f])
       (cond
         [(pair? p)
          (when (and move-slow? (eq? p slow))
            (write-circular-error who v))
          (let ([member (car p)])
            (unless (pair? member) (write-error who v))
            (let ([key (car member)])
              (unless first? (put-char! #\,))
              (if (or (string? key) (symbol? key))
                  (put-key! key)
                  (write-error who v))
              (put-char! #\:)
              (put-value! (cdr member) (fx+ depth 1))
              (loop (cdr p) (if move-slow? (cdr slow) slow) #f (not move-slow?))))]
         [(null? p) (put-char! #\})]
         [else (write-error who v)]))]
    [(vector? v)
     (when (fx= depth max-depth) (refuse-nesting))
     (put-char! #\[)
     (let ([n (vector-length v)])
       (do ([i 0 (fx+ i 1)]) ((fx= i n))
         (unless (fx= i 0) (put-char! #\,))
         (put-value! (vector-ref v i) (fx+ depth 1))))
     (put-char! #\])]
    [(null? v)
     (when (fx= depth max-depth) (refuse-nesting))
     (put-ascii! "{}")]
    [(eq? v #t) (put-ascii! "true")]
    [(eq? v #f) (put-ascii! "false")]
    [(eq? v 'null) (put-ascii! "null")]
    [(bignum? v) (put-ascii! (number->string v))]
    [else (write-error who v)]))

(put-value! v 0)
(text)
