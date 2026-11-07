(define stack (make-vector 32))
(define buffer "")
(define key-cache #f)
(define key-cache-misses 0)

(define stop #f)

(define (fail i message . irritants)
  (if stop
      (stop)
      (apply read-error who (fx+ offset i) message irritants)))

(define (fail-unit i before u after)
  (if (fx< u 0)
      (fail i (string-append before "end of input" after))
      (let ([x (unit-value u)])
        (if (char? x)
            (fail i (string-append before "~s" after) x)
            (fail i (string-append before "byte #x~x" after) x)))))

(define (unexpected i)
  (fail-unit i "unexpected " (unit i) ""))

(define (push! top v)
  (when (fx= top (vector-length stack))
    (let ([new (make-vector (fx* 2 top))])
      (vector-copy! stack 0 new 0 top)
      (set! stack new)))
  (vector-set! stack top v)
  (fx+ top 1))

(define (buffer-reserve! n)
  (when (fx>= n (string-length buffer))
    (let ([new (make-string (fxmax 64 (fx* 2 n)))])
      (string-copy! buffer 0 new 0 (string-length buffer))
      (set! buffer new))))

(define (buffer-put! n c)
  (buffer-reserve! n)
  (string-set! buffer n c))

(define (skip-whitespace i)
  (let ([u (unit i)])
    (cond
      [(fx= u 32) (skip-whitespace (fx+ i 1))]
      [(fx> u 32) i]
      [(or (fx= u 10) (fx= u 13) (fx= u 9)) (skip-whitespace (fx+ i 1))]
      [else i])))

(define (digit? u) (fx<= 48 u 57))

(define (hex-quad i)
  (let loop ([k 0] [n 0])
    (if (fx= k 4)
        n
        (let ([u (unit (fx+ i k))])
          (cond
            [(fx<= 48 u 57) (loop (fx+ k 1) (fx+ (fx* n 16) (fx- u 48)))]
            [(fx<= 65 u 70) (loop (fx+ k 1) (fx+ (fx* n 16) (fx- u 55)))]
            [(fx<= 97 u 102) (loop (fx+ k 1) (fx+ (fx* n 16) (fx- u 87)))]
            [else (fail-unit (fx+ i k) "invalid \\u escape: " u "")])))))

(define (read-unicode-escape i)
  (let ([n (hex-quad (fx+ i 2))])
    (cond
      [(or (fx< n #xD800) (fx> n #xDFFF)) (values (integer->char n) (fx+ i 6))]
      [(fx>= n #xDC00) (fail i "unpaired surrogate \\u~4,'0x" n)]
      [(and (fx= (unit (fx+ i 6)) 92) (fx= (unit (fx+ i 7)) 117))
       (let ([low (hex-quad (fx+ i 8))])
         (if (fx<= #xDC00 low #xDFFF)
             (values
               (integer->char (fx+ #x10000 (fxsll (fx- n #xD800) 10) (fx- low #xDC00)))
               (fx+ i 12))
             (fail i "unpaired surrogate \\u~4,'0x" n)))]
      [else (fail i "unpaired surrogate \\u~4,'0x" n)])))

(define (read-escape i)
  (let ([u (unit (fx+ i 1))])
    (case u
      [(34) (values #\" (fx+ i 2))]
      [(92) (values #\\ (fx+ i 2))]
      [(47) (values #\/ (fx+ i 2))]
      [(98) (values #\backspace (fx+ i 2))]
      [(102) (values #\page (fx+ i 2))]
      [(110) (values #\newline (fx+ i 2))]
      [(114) (values #\return (fx+ i 2))]
      [(116) (values #\tab (fx+ i 2))]
      [(117) (read-unicode-escape i)]
      [(-1) (fail i "unterminated escape")]
      [else (fail-unit i "invalid escape: \\ followed by " u "")])))

(define (read-string-rest i n)
  (let ([u (unit i)])
    (cond
      [(fx= u 34) (values (substring buffer 0 n) (fx+ i 1))]
      [(fx= u 92)
       (let-values ([(c i) (read-escape i)])
         (buffer-put! n c)
         (read-string-rest i (fx+ n 1)))]
      [(fx< u 0) (fail i "unterminated string")]
      [(fx< u 32) (fail-unit i "unescaped control character " u " in a string")]
      [(fx< u 128)
       (buffer-put! n (integer->char u))
       (read-string-rest (fx+ i 1) (fx+ n 1))]
      [else
       (let-values ([(c i) (read-non-ascii u i)])
         (buffer-put! n c)
         (read-string-rest i (fx+ n 1)))])))

(define (literal i text)
  (let ([n (string-length text)])
    (let loop ([k 1])
      (cond
        [(fx= k n) (fx+ i n)]
        [(fx= (unit (fx+ i k)) (char->integer (string-ref text k))) (loop (fx+ k 1))]
        [else (unexpected (fx+ i k))]))))

;; A number of more than 18 digits is read again, in 18-digit chunks, only
;; after the digit limit is checked, so that no input makes the reader do
;; quadratic bignum work.
(define (digits->integer from to)
  (let loop ([i from] [chunk 0] [size 0] [n 0])
    (cond
      [(fx= i to) (+ (* n (expt 10 size)) chunk)]
      [(digit? (unit i))
       (if (fx= size 18)
           (loop (fx+ i 1) (fx- (unit i) 48) 1 (+ (* n #e1e18) chunk))
           (loop (fx+ i 1) (fx+ (fx* chunk 10) (fx- (unit i) 48)) (fx+ size 1) n))]
      [else (loop (fx+ i 1) chunk size n)])))

(define (read-number start)
  (define negative? (fx= (unit start) 45))
  (define int-start (if negative? (fx+ start 1) start))
  (define (finish i mantissa-end digits m exponent fraction-digits integer?)
    (when (fx> digits max-digits)
      (fail start "number with more than ~s digits" max-digits))
    (let ([n (if (fx<= digits 18) m (digits->integer int-start mantissa-end))])
      (if integer?
          (values (if negative? (- n) n) i)
          (let ([x (decimal->flonum n (fx- exponent fraction-digits))])
            (when (fl= x +inf.0)
              (fail start "number out of range"))
            (values (if negative? (fl- x) x) i)))))
  (define (exponent-part i digits m fraction-digits)
    (let* ([mantissa-end (fx- i 1)]
           [u (unit i)]
           [exponent-negative? (fx= u 45)]
           [i (if (or exponent-negative? (fx= u 43)) (fx+ i 1) i)])
      (unless (digit? (unit i))
        (fail-unit i "expected a digit in the exponent, not " (unit i) ""))
      ;; Saturating keeps the exponent a fixnum; any exponent past the
      ;; bound already puts the value outside the flonum range.
      (let loop ([i i] [e 0])
        (let ([u (unit i)])
          (if (digit? u)
              (loop (fx+ i 1) (if (fx< e 100000000) (fx+ (fx* e 10) (fx- u 48)) e))
              (finish i mantissa-end digits m (if exponent-negative? (fx- e) e) fraction-digits #f))))))
  (define (fraction-part i digits m)
    (unless (digit? (unit i))
      (fail-unit i "expected a digit after the decimal point, not " (unit i) ""))
    (let loop ([i i] [digits digits] [m m] [fraction-digits 0])
      (let ([u (unit i)])
        (cond
          [(digit? u)
           (loop (fx+ i 1) (fx+ digits 1)
             (if (fx< digits 18) (fx+ (fx* m 10) (fx- u 48)) m)
             (fx+ fraction-digits 1))]
          [(or (fx= u 101) (fx= u 69)) (exponent-part (fx+ i 1) digits m fraction-digits)]
          [else (finish i i digits m 0 fraction-digits #f)]))))
  (let ([u (unit int-start)])
    (unless (digit? u)
      (fail-unit int-start "expected a digit, not " u ""))
    (when (and (fx= u 48) (digit? (unit (fx+ int-start 1))))
      (fail start "number with a leading zero")))
  (let loop ([i int-start] [digits 0] [m 0])
    (let ([u (unit i)])
      (cond
        [(digit? u)
         (loop (fx+ i 1) (fx+ digits 1) (if (fx< digits 18) (fx+ (fx* m 10) (fx- u 48)) m))]
        [(fx= u 46) (fraction-part (fx+ i 1) digits m)]
        [(or (fx= u 101) (fx= u 69)) (exponent-part (fx+ i 1) digits m 0)]
        [else (finish i i digits m 0 0 #t)]))))

(define (check-depth i depth)
  (when (fx= depth max-depth)
    (fail i "nesting deeper than ~s" max-depth)))

(define (parse-value i top frames depth)
  (let* ([i (skip-whitespace i)] [u (unit i)])
    (cond
      [(fx= u 34)
       (let-values ([(s i) (read-string-body (fx+ i 1))])
         (after-value s i top frames depth))]
      [(or (digit? u) (fx= u 45))
       (let-values ([(x i) (read-number i)])
         (after-value x i top frames depth))]
      [(fx= u 123)
       (check-depth i depth)
       (let ([j (skip-whitespace (fx+ i 1))])
         (if (fx= (unit j) 125)
             (after-value '() (fx+ j 1) top frames depth)
             (parse-key j top (cons (make-object-frame top 0 #f) frames) (fx+ depth 1))))]
      [(fx= u 91)
       (check-depth i depth)
       (let ([j (skip-whitespace (fx+ i 1))])
         (if (fx= (unit j) 93)
             (after-value (vector) (fx+ j 1) top frames depth)
             (parse-value j top (cons top frames) (fx+ depth 1))))]
      [(fx= u 116) (after-value #t (literal i "true") top frames depth)]
      [(fx= u 102) (after-value #f (literal i "false") top frames depth)]
      [(fx= u 110) (after-value 'null (literal i "null") top frames depth)]
      [else (unexpected i)])))

(define (parse-key i top frames depth)
  (unless (fx= (unit i) 34)
    (fail-unit i "expected a string key, not " (unit i) ""))
  (let-values ([(key h j) (read-key (fx+ i 1))])
    (unless duplicates? (check-duplicate (car frames) key h top i))
    (let ([j (skip-whitespace j)])
      (unless (fx= (unit j) 58)
        (fail-unit j "expected : after a key, not " (unit j) ""))
      (parse-value (fx+ j 1) (push! top (cons key #f)) frames depth))))

;; Equal keys in one parse share one string while it stays in a small
;; table indexed by a hash of the key's text; keys with escapes are not
;; cached. Misses only cost an allocation, so keys built to collide do no
;; more than unrelated keys. The table starts small, because making the
;; large one costs more than a small value's keys, and is replaced by the
;; large one after enough misses. The hash is of the key's encoded text,
;; so that an escaped key, hashed after decoding, gets the hash of the same
;; key unescaped.
(define (read-key i)
  (let scan ([j i] [h 0])
    (let ([u (unit j)])
      (cond
        [(fx= u 34)
         (unless key-cache (set! key-cache (make-vector small-key-cache-size #f)))
         (let* ([slot (fxand (fxxor h (fxsrl h 9)) (fx- (vector-length key-cache) 1))]
                [cached (vector-ref key-cache slot)])
           (if (and cached (key-text=? cached i j))
               (values cached h (fx+ j 1))
               (let-values ([(s next) (read-plain-key i j)])
                 (let ([key (string->immutable-string s)])
                   (set! key-cache-misses (fx+ key-cache-misses 1))
                   (if (fx= key-cache-misses small-key-cache-size)
                       (set! key-cache (make-vector large-key-cache-size #f))
                       (vector-set! key-cache slot key))
                   (values key h next)))))]
        [(or (fx= u 92) (fx< u 32))
         (let-values ([(s next) (read-string-body i)])
           (values (string->immutable-string s) (key-hash s) next))]
        [else (scan (fx+ j 1) (key-hash-step h u))]))))

;; Comparing each key with every earlier one is quadratic even within 32
;; keys, so a key is compared only when its hash bit has been seen.
(define (check-duplicate frame key h top i)
  (let ([base (object-frame-base frame)] [late (object-frame-late-positions frame)])
    (cond
      [late (object-frame-late-positions-set! frame (cons i late))]
      [(fx= (fx- top base) duplicate-scan-limit)
       (object-frame-late-positions-set! frame (list i))]
      [else
       (let ([bit (fxsll 1 (fxremainder (fxxor h (fxsrl h 7)) 60))]
             [hashes (object-frame-hashes frame)])
         (unless (fx= 0 (fxand hashes bit))
           (do ([k base (fx+ k 1)]) ((fx= k top))
             (let ([other (car (vector-ref stack k))])
               (when (or (eq? other key) (string=? other key))
                 (fail i "duplicate key ~s" key)))))
         (object-frame-hashes-set! frame (fxior hashes bit)))])))

;; A hashtable of strings would hash them with a fixed function, so keys
;; built to collide would make it quadratic; sorting stays n log n. Every
;; pair among the first keys has been compared, so a duplicate found here
;; is a later key, whose position was kept.
(define (check-duplicates-by-sorting base top late-positions)
  (define (key-at k) (car (vector-ref stack k)))
  (let ([order (let ([v (make-vector (fx- top base))])
                 (do ([k 0 (fx+ k 1)]) ((fx= k (vector-length v)))
                   (vector-set! v k (fx+ base k)))
                 (vector-sort!
                   (lambda (a b)
                     (let ([x (key-at a)] [y (key-at b)])
                       (or (string<? x y) (and (fx< a b) (string=? x y)))))
                   v)
                 v)])
    (let loop ([k 1] [earliest #f])
      (if (fx= k (vector-length order))
          (when earliest
            (fail (list-ref (reverse late-positions) (fx- earliest base duplicate-scan-limit))
              "duplicate key ~s" (key-at earliest)))
          (let ([a (vector-ref order (fx- k 1))] [b (vector-ref order k)])
            (loop (fx+ k 1)
              (if (and (string=? (key-at a) (key-at b)) (or (not earliest) (fx< b earliest)))
                  b
                  earliest)))))))

(define (after-value v i top frames depth)
  (if (null? frames)
      (values v i)
      (let ([frame (car frames)])
        (if (fixnum? frame)
            (let* ([top (push! top v)] [i (skip-whitespace i)] [u (unit i)])
              (cond
                [(fx= u 44) (parse-value (fx+ i 1) top frames depth)]
                [(fx= u 93)
                 (after-value (vector-copy stack frame (fx- top frame)) (fx+ i 1)
                   frame (cdr frames) (fx- depth 1))]
                [else (fail-unit i "expected , or ] in an array, not " u "")]))
            (begin
              (set-cdr! (vector-ref stack (fx- top 1)) v)
              (let* ([i (skip-whitespace i)] [u (unit i)])
                (cond
                  [(fx= u 44) (parse-key (skip-whitespace (fx+ i 1)) top frames depth)]
                  [(fx= u 125)
                   (let ([base (object-frame-base frame)])
                     (let ([late (object-frame-late-positions frame)])
                       (when late (check-duplicates-by-sorting base top late)))
                     (after-value
                       (let loop ([k (fx- top 1)] [members '()])
                         (if (fx< k base)
                             members
                             (loop (fx- k 1) (cons (vector-ref stack k) members))))
                       (fx+ i 1) base (cdr frames) (fx- depth 1)))]
                  [else (fail-unit i "expected , or } in an object, not " u "")])))))))

;; The caller of a text that may end inside its value retries any failure,
;; so failing returns #f as the end index instead of raising a condition.
(if whole?
    (let-values ([(v i) (parse-value start 0 '() 0)])
      (let ([i (skip-whitespace i)])
        (unless (fx= i end)
          (fail-unit i "unexpected " (unit i) " after the value")))
      (values v i))
    (call/cc
      (lambda (k)
        (set! stop (lambda () (k #f #f)))
        (parse-value start 0 '() 0))))
