(import (chezscheme) (json) (check))

(define (flonum->bits x)
  (let ([bv (make-bytevector 8)])
    (bytevector-ieee-double-set! bv 0 x (endianness big))
    (bytevector-u64-ref bv 0 (endianness big))))

(define (bits->flonum n)
  (let ([bv (make-bytevector 8)])
    (bytevector-u64-set! bv 0 n (endianness big))
    (bytevector-ieee-double-ref bv 0 (endianness big))))

(define largest-flonum 1.7976931348623157e308)

(define (random-flonum)
  (let ([x (bits->flonum (random #x7FF0000000000000))])
    (if (fl= x largest-flonum) (random-flonum) x)))

(define (nearest? d y)
  (and (flonum? y) (fl>= y 0.0) (fl< y +inf.0)
       (let* ([bits (flonum->bits y)]
              [error (abs (- d (exact y)))])
         (define (beats? neighbour-bits)
           (let ([other (abs (- d (exact (bits->flonum neighbour-bits))))])
             (or (< error other) (and (= error other) (even? bits)))))
         (and (or (= bits 0) (beats? (- bits 1)))
              (or (fl= y largest-flonum) (beats? (+ bits 1)))))))

(define (shortest x)
  (parameterize ([print-subnormal-precision #f]) (number->string x)))

(define (significant-digits text)
  (let* ([mantissa (let loop ([cs (string->list text)] [acc '()])
                     (if (or (null? cs) (char=? (car cs) #\e))
                         (reverse acc)
                         (loop (cdr cs) (cons (car cs) acc))))]
         [digits (filter char-numeric? mantissa)]
         [trim (lambda (ds) (let loop ([ds ds]) (if (and (pair? ds) (char=? (car ds) #\0)) (loop (cdr ds)) ds)))])
    (length (trim (reverse (trim digits))))))

(define (decimal-value text) (string->number (string-append "#e" text)))

;; Chez's number->string gives the shortest digits; where two are equally
;; short and equally near, it takes the higher and the writer the even one.
(define ties 0)
(define (index-of c text)
  (let loop ([i 0])
    (cond
      [(= i (string-length text)) #f]
      [(char=? (string-ref text i) c) i]
      [else (loop (+ i 1))])))
(define (written-shortest? x text)
  (let ([chez (shortest x)])
    (or (string=? text chez)
        (and (= (string-length text) (string-length chez))
             (eqv? (index-of #\. text) (index-of #\. chez))
             (eqv? (index-of #\e text) (index-of #\e chez))
             (= (significant-digits text) (significant-digits chez))
             (= (abs (- (decimal-value text) (exact x)))
                (abs (- (decimal-value chez) (exact x))))
             (begin (set! ties (+ ties 1)) #t)))))

(define (decimal q digits)
  (let* ([k (let loop ([k (- (exact (floor (/ (log (inexact q)) (log 10)))) (- digits 1))])
              (cond
                [(>= (/ q (expt 10 k)) (expt 10 digits)) (loop (+ k 1))]
                [(< (/ q (expt 10 k)) (expt 10 (- digits 1))) (loop (- k 1))]
                [else k]))]
         [m (round (/ q (expt 10 k)))])
    (if (= m (expt 10 digits))
        (decimal (* m (expt 10 k)) digits)
        (let ([s (number->string m)])
          (values
            (format "~a.~ae~a" (substring s 0 1) (substring s 1 digits) (+ k (- digits 1)))
            (* m (expt 10 k)))))))

(define (read-number text)
  (vector-ref (string->json (string-append "[" text "]")) 0))

(define (written x)
  (let ([text (json->string (vector x))])
    (substring text 1 (- (string-length text) 1))))

(define (refused? thunk)
  (guard (c [(json-error? c) #t]) (thunk) #f))

(random-seed 180)

(test "28 hard literals read as the correctly rounded flonum or exact integer"
  (for-each
    (lambda (case)
      (let ([text (car case)] [expected (cadr case)])
        (if (eq? expected 'refused)
            (check-raises json-error? (read-number text))
            (begin
              (check-equal? expected (read-number text))
              (when (flonum? expected)
                (check-equal? expected (read-number (written expected))))))))
    '(("0.1" 0.1)
      ("1e23" 1e23)
      ("5e-324" 5e-324)
      ("1.7976931348623157e308" 1.7976931348623157e308)
      ("9007199254740993" 9007199254740993)
      ("-0" 0)
      ("-0.0" -0.0)
      ("1e400" refused)
      ("-1e400" refused)
      ("1e-400" 0.0)
      ("123456789012345678901234567890" 123456789012345678901234567890)
      ("2.2250738585072011e-308" 2.225073858507201e-308)
      ("2.4703282292062327e-324" 0.0)
      ("2.4703282292062328e-324" 5e-324)
      ("1.7976931348623159e308" refused)
      ("7.038531e-26" 7.038531e-26)
      ("0.30000000000000004" 0.30000000000000004)
      ("0.1000000000000000055511151231257827021181583404541015625" 0.1)
      ("1E2" 100.0)
      ("1e+2" 100.0)
      ("100" 100)
      ("1.0" 1.0)
      ("3.141592653589793" 3.141592653589793)
      ("123.456e-789" 0.0)
      ("0.0001e310" 1e306)
      ("1e-7" 1e-7)
      ("1.5" 1.5)
      ("-12345678901234567890.5" -1.2345678901234567e19))))

(test "flonums are written without Chez's subnormal suffix"
  (check-equal? "[5e-324,2.225073858507201e-308,1e-310]"
    (json->string (vector 5e-324 2.225073858507201e-308 1e-310)))
  (check-equal? "[100.0,1e21,1e-7,-0.0,0.1,1.2345678901234567e19]"
    (json->string (vector 100.0 1e21 1e-7 -0.0 0.1 1.2345678901234567e19))))

(test "100,000 random flonums read exactly in shortest and 17-digit forms and write back"
  (do ([i 0 (+ i 1)]) ((= i 100000))
    (let* ([x (random-flonum)] [x (if (odd? i) (fl- x) x)])
      (check-equal? x (read-number (shortest x)))
      (let-values ([(text value) (decimal (abs (exact x)) 17)])
        (let ([y (read-number (if (fl< x 0.0) (string-append "-" text) text))])
          (check-equal? x y)))
      (let ([text (written x)])
        (check (written-shortest? x text))
        (check-equal? x (read-number text)))))
  (printf "random flonums written as Chez writes them but for ~a ties\n" ties))

(test "flonums are written as Chez writes them, but for ties, across every exponent"
  (do ([e 0 (+ e 1)]) ((= e 2047))
    (do ([d -2 (+ d 1)]) ((= d 3))
      (let ([bits (+ (* e (expt 2 52)) d)])
        (when (< 0 bits #x7FF0000000000000)
          (let* ([x (bits->flonum bits)] [text (written x)])
            (check (written-shortest? x text))
            (check-equal? x (read-number text)))))))
  (do ([t 1 (+ t 1)]) ((= t 5000))
    (let* ([x (bits->flonum t)] [text (written x)])
      (check (written-shortest? x text))
      (check-equal? x (read-number text))))
  (do ([i 1 (+ i 1)]) ((= i 20000))
    (for-each
      (lambda (x)
        (let ([text (written x)])
          (check (written-shortest? x text))
          (check-equal? x (read-number text))))
      (list (inexact i) (/ (inexact i) 1000.0) (* (inexact i) 1e20) (/ (inexact i) 3.0))))
  (check-equal? "2.7070304891547812e12" (written 2707030489154.78125))
  (check-equal? "5e-324" (written 5e-324))
  (check-equal? "1e-322" (written 1e-322))
  (check-equal? "1.7976931348623157e308" (written 1.7976931348623157e308))
  (check-equal? "1234567890.0" (written 1234567890.0))
  (check-equal? "1.2345678901e10" (written 12345678901.0))
  (check-equal? "0.001" (written 0.001))
  (check-equal? "1e-4" (written 0.0001))
  (check-equal? "-1.5e-7" (written -1.5e-7)))

(test "25,000 decimals of 17 to 25 digits read as the nearest flonum"
  (do ([i 0 (+ i 1)]) ((= i 25000))
    (let* ([digits (+ 17 (random 9))]
           [m (+ (expt 10 (- digits 1)) (random (- (expt 10 digits) (expt 10 (- digits 1)))))]
           [s (number->string m)]
           [e (- (random 641) 330)]
           [text (format "~a.~ae~a" (substring s 0 1) (substring s 1 digits) e)]
           [d (* m (expt 10 (- e (- digits 1))))])
      (if (>= d (+ (exact largest-flonum) (expt 2 970)))
          (check (refused? (lambda () (read-number text))))
          (let ([y (read-number text)])
            (check (nearest? d y))
            (check-equal? y (read-number (written y))))))))

(test "25,000 decimals within one part in 10^40 of a halfway point, a tenth of them subnormal, read as the nearest flonum"
  (do ([i 0 (+ i 1)]) ((= i 25000))
    (let* ([x (if (= (mod i 10) 0) (bits->flonum (random #x10000000000000)) (random-flonum))]
           [halfway (/ (+ (exact x) (exact (bits->flonum (+ (flonum->bits x) 1)))) 2)]
           [step (expt 10 (- (exact (floor (/ (log (inexact halfway)) (log 10)))) 39))]
           [q (+ halfway (* step (- (random 3) 1)))])
      (let-values ([(text d) (decimal q 46)])
        (let ([y (read-number text)])
          (check (nearest? d y))
          (check-equal? y (read-number (written y))))))))

(test "exact halfway points and the ends of the exponent range read as the nearest flonum"
  (define (check-decimal m q)
    (let* ([text (format "~ae~a" m q)] [d (* m (expt 10 q))])
      (if (>= d (+ (exact largest-flonum) (expt 2 970)))
          (check (refused? (lambda () (read-number text))))
          (check (nearest? d (read-number text))))))
  ;; m 10^q is halfway between two flonums when m 5^q is an odd number of
  ;; 54 bits.
  (do ([q -4 (+ q 1)]) ((> q 23))
    (do ([i 0 (+ i 1)]) ((= i 2000))
      (let ([m (if (>= q 0)
                   (let* ([low (ceiling (/ (expt 2 53) (expt 5 q)))]
                          [m (+ low (random (max 1 (- (quotient (expt 2 54) (expt 5 q)) low))))])
                     (if (even? m) (+ m 1) m))
                   (* (+ (expt 2 53) (* 2 (random (expt 2 52))) 1) (expt 5 (- q))))])
        (for-each (lambda (m) (check-decimal m q)) (list m (+ m 1) (- m 1))))))
  ;; With P = 512 u + 1 for an odd u = 1 (mod 4) of 54 bits, P/5^q 10^q is
  ;; just above a halfway point, close enough to look like one in the high
  ;; bits of the product.
  (do ([q 2 (+ q 1)]) ((> q 4))
    (do ([i 0 (+ i 1)]) ((= i 50))
      (let loop ([u (+ (expt 2 53) 1 (* 4 (random (expt 2 50))))])
        (let ([p (+ (* 512 u) 1)])
          (if (= 0 (mod p (expt 5 q)))
              (check-decimal (/ p (expt 5 q)) q)
              (loop (+ u 4)))))))
  (check-equal? 1.8446744073709728e19 (read-number "184467440737097257e2"))
  (check-equal? 3.6893488147419865e19 (read-number "36893488147419861e3"))
  (for-each
    (lambda (q)
      (do ([i 0 (+ i 1)]) ((= i 2000))
        (check-decimal (+ 1 (random (expt 10 (+ 1 (random 19))))) q)
        (check-decimal (- (most-positive-fixnum) (random 1000)) q)))
    '(-343 -342 -341 -325 -324 -323 -308 -307 -22 -21 0 21 22 23 288 289 290 307 308 309))
  (check-equal? 5e-324 (read-number "4.9406564584124654e-324"))
  (check-equal? 0.0 (read-number "1e-343"))
  (check-equal? 1e308 (read-number "1e308"))
  (check (refused? (lambda () (read-number "1e309")))))

(test "integers stay exact"
  (let ([big (- (expt 10 400) 1)])
    (check-equal? big (read-number (number->string big)))
    (check-equal? (- big) (read-number (string-append "-" (number->string big))))
    (check-equal? (number->string big) (written big)))
  (check-equal? (most-positive-fixnum) (read-number (number->string (most-positive-fixnum))))
  (check-equal? (+ (most-positive-fixnum) 1)
    (read-number (number->string (+ (most-positive-fixnum) 1))))
  (check-equal? 999999999999999999 (read-number "999999999999999999"))
  (check-equal? 1000000000000000000 (read-number "1000000000000000000"))
  (check-equal? 1.0e18 (read-number "1000000000000000000.0"))
  (check-equal? 0.0 (read-number "0e999999999999999999999"))
  (check-equal? -0.0 (read-number "-0e5"))
  (check-equal? 1.0 (read-number "0.000000000000000000000000000001e30")))

(test-exit)
