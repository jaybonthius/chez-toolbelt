(import (chezscheme) (json) (check))

;; Chez interns symbols in one table under a fixed hash. These keys all
;; share one hash value, so interning them is quadratic; reading them must
;; not be.
(define mask (- (expt 2 64) 1))

(define (mix h)
  (let ([h (bitwise-and (+ h (bitwise-arithmetic-shift-left h 10)) mask)])
    (bitwise-xor h (bitwise-arithmetic-shift-right h 6))))

(define letters
  (append (map (lambda (i) (+ i (char->integer #\A))) (iota 26))
          (map (lambda (i) (+ i (char->integer #\a))) (iota 26))))

(define (allowed? c)
  (and (<= #x20 c #x10FFFF) (not (<= #xD800 c #xDFFF)) (not (memv c '(#x22 #x5C)))))

(define (colliding-pair state)
  (let ([mixed (map (lambda (a) (cons a (mix (bitwise-and (+ state a) mask)))) letters)])
    (let search ([as letters])
      (let ([a (car as)])
        (let try ([bs (cdr as)])
          (if (null? bs)
              (search (cdr as))
              (let* ([a2 (car bs)]
                     [d (bitwise-and (- (cdr (assv a mixed)) (cdr (assv a2 mixed))) mask)]
                     [d (if (>= d (expt 2 63)) (- d (expt 2 64)) d)]
                     [found (find (lambda (b) (and (allowed? b) (allowed? (+ b d))))
                              '(#x4E00 #x20000 #x30000 #x100))])
                (if found
                    (values (string (integer->char a) (integer->char found))
                            (string (integer->char a2) (integer->char (+ found d)))
                            (mix (bitwise-and (+ (cdr (assv a mixed)) found) mask)))
                    (try (cdr bs))))))))))

(define (colliding-keys blocks)
  (let loop ([i 0] [state (bitwise-and (+ (* 2 blocks) 401887359) mask)] [keys '("")])
    (if (= i blocks)
        keys
        (let-values ([(x y next) (colliding-pair state)])
          (loop (+ i 1) next
            (append (map (lambda (k) (string-append k x)) keys)
                    (map (lambda (k) (string-append k y)) keys)))))))

(define (random-keys like)
  (let* ([seen (make-eqv-hashtable)]
         [chars (begin
                  (for-each (lambda (k) (string-for-each (lambda (c) (hashtable-set! seen c #t)) k))
                    like)
                  (vector-sort char<? (hashtable-keys seen)))]
         [n (string-length (car like))])
    (map (lambda (k)
           (let ([s (make-string n)])
             (do ([i 0 (+ i 1)]) ((= i n) s)
               (string-set! s i (vector-ref chars (random (vector-length chars)))))))
         like)))

(define (objects keys)
  (call-with-string-output-port
    (lambda (p)
      (put-char p #\[)
      (for-each (lambda (k i)
                  (unless (= i 0) (put-char p #\,))
                  (put-string p "{\"") (put-string p k) (put-string p "\":0}"))
        keys (iota (length keys)))
      (put-char p #\]))))

(define (one-object keys)
  (call-with-string-output-port
    (lambda (p)
      (put-char p #\{)
      (for-each (lambda (k i)
                  (unless (= i 0) (put-char p #\,))
                  (put-char p #\") (put-string p k) (put-string p "\":0"))
        keys (iota (length keys)))
      (put-char p #\}))))

(define (milliseconds thunk)
  (let loop ([i 0] [best #f])
    (if (= i 3)
        best
        (begin
          (collect)
          (let ([t0 (current-time 'time-monotonic)])
            (thunk)
            (let* ([d (time-difference (current-time 'time-monotonic) t0)]
                   [ms (+ (* 1000 (time-second d)) (/ (time-nanosecond d) 1e6))])
              (loop (+ i 1) (if best (min best ms) ms))))))))

(random-seed 1)
(define collide (colliding-keys 16))
(define unrelated (random-keys collide))

(test "the keys are distinct and collide in Chez's symbol hash"
  (check-equal? 65536 (length collide))
  (let ([sorted (sort string<? collide)])
    (check (for-all (lambda (a b) (not (string=? a b))) (cdr sorted) (reverse (cdr (reverse sorted))))))
  (check-equal? (symbol-hash (string->symbol (car collide)))
    (symbol-hash (string->symbol (list-ref collide 40000)))))

(test "65,536 colliding keys read as fast as unrelated keys of the same shape"
  (for-each
    (lambda (build read)
      (let* ([collide-text (build collide)]
             [unrelated-text (build unrelated)]
             [v (read collide-text)]
             [collide-ms (milliseconds (lambda () (read collide-text)))]
             [unrelated-ms (milliseconds (lambda () (read unrelated-text)))])
        (check-equal? (car collide)
          (if (vector? v) (caar (vector-ref v 0)) (caar v)))
        (printf "~a ms colliding, ~a ms unrelated\n" (round collide-ms) (round unrelated-ms))
        (check (< collide-ms (+ (* 2 unrelated-ms) 20)))))
    (list objects objects one-object one-object)
    (list string->json
          (lambda (text) (bytevector->json (string->utf8 text)))
          string->json
          (lambda (text) (json-read (open-string-input-port text))))))

(test-exit)
