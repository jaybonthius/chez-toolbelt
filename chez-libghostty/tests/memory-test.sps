(import (chezscheme) (ghostty) (check))

(define (bytes s) (string->utf8 s))

(define (resident-bytes)
  (call-with-input-file "/proc/self/statm"
    (lambda (p) (read p) (* 4096 (read p)))))

;; Resident memory moves by pages and with the collector's heap, too
;; coarsely to see one small object; libghostty-vt allocates with malloc,
;; whose own count sees each one.
(define-ftype mallinfo2
  (struct [arena size_t] [ordblks size_t] [smblks size_t] [hblks size_t] [hblkhd size_t]
          [usmblks size_t] [fsmblks size_t] [uordblks size_t] [fordblks size_t] [keepcost size_t]))

(define mallinfo2-into (foreign-procedure "mallinfo2" () (& mallinfo2)))

(define (malloc-bytes)
  (let ([p (make-ftype-pointer mallinfo2 (foreign-alloc (ftype-sizeof mallinfo2)))])
    (mallinfo2-into p)
    (let ([n (+ (ftype-ref mallinfo2 (uordblks) p) (ftype-ref mallinfo2 (hblkhd) p))])
      (foreign-free (ftype-pointer-address p))
      n)))

(define (collect-fully)
  (collect (collect-maximum-generation))
  (collect (collect-maximum-generation)))

(define filler (make-bytevector (* 256 1024) 120))

(define (make-busy-objects)
  (let ([t (make-terminal 80 24)])
    (terminal-write! t filler)
    (let ([rs (make-render-state)] [s (make-search t)] [g (make-selection-gesture t)]
          [tp (make-tracked-point t (make-grid-point 'active 0 0))]
          [k (make-key-encoder)] [m (make-mouse-encoder)]
          [d (make-snapshot-decoder (terminal->snapshot t))])
      (render-state-update! rs t)
      (search-set-needle! s "needle")
      (search-feed! s)
      (snapshot-decoder-ready! d)
      (list t rs s g tp k m d))))

(define (round-of-drops n)
  (do ([i 0 (+ i 1)]) ((= i n)) (make-busy-objects))
  (collect-fully)
  ;; Making an object frees what the collector found dropped.
  (terminal-close! (make-terminal 2 2)))

(test "dropped objects are freed"
  (round-of-drops 40)
  (let ([baseline (resident-bytes)])
    (do ([round 0 (+ round 1)]) ((= round 5)) (round-of-drops 40))
    (let ([growth (- (resident-bytes) baseline)])
      (check (< growth (* 40 1024 1024))))))

(test "dropped small objects are freed, each kind to within a few bytes"
  (let ([t (make-terminal 80 24)])
    (for-each
      (lambda (make)
        (let ([run (lambda ()
                     (do ([i 0 (+ i 1)]) ((= i 1000)) (make))
                     (collect-fully)
                     (terminal-close! (make-terminal 2 2)))])
          (run)
          (let ([baseline (malloc-bytes)])
            (run)
            (check (< (- (malloc-bytes) baseline) (* 16 1000))))))
      (list make-key-encoder make-mouse-encoder
            (lambda () (make-tracked-point t (make-grid-point 'active 0 0)))
            (lambda () (make-search t))
            (lambda () (make-selection-gesture t))))
    (terminal-close! t)))

(test "closed objects are freed"
  (let ([run (lambda ()
               (do ([i 0 (+ i 1)]) ((= i 40))
                 (let ([objects (make-busy-objects)])
                   (for-each
                     (lambda (o)
                       (cond
                         [(terminal? o) (terminal-close! o)]
                         [(render-state? o) (render-state-close! o)]
                         [(search? o) (search-close! o)]
                         [(selection-gesture? o) (selection-gesture-close! o)]
                         [(tracked-point? o) (tracked-point-close! o)]
                         [(key-encoder? o) (key-encoder-close! o)]
                         [(mouse-encoder? o) (mouse-encoder-close! o)]
                         [(snapshot-decoder? o) (snapshot-decoder-close! o)]))
                     (reverse objects))))
               (collect-fully))])
    (run)
    (let ([baseline (resident-bytes)])
      (do ([round 0 (+ round 1)]) ((= round 5)) (run))
      (check (< (- (resident-bytes) baseline) (* 40 1024 1024))))))

(test "dependents outlive or predecease their terminal in any order"
  (do ([i 0 (+ i 1)]) ((= i 50))
    (let ([objects (make-busy-objects)])
      (case (modulo i 3)
        [(0) (terminal-close! (car objects))]
        [(1) (for-each (lambda (o) (when (search? o) (search-close! o))) objects)]
        [else (void)])
      (when (= 0 (modulo i 10)) (collect-fully))))
  (collect-fully)
  (terminal-close! (make-terminal 2 2))
  (check #t))

(test "a collection inside a callback does not move what C is reading"
  (let ([t (make-terminal 40 5)] [calls 0])
    (terminal-set-bell-handler! t
      (lambda ()
        (set! calls (+ calls 1))
        (make-bytevector 100000)
        (collect-fully)))
    (terminal-set-enquiry-handler! t
      (lambda () (collect-fully) (make-string 10 #\r)))
    (let ([pty '()])
      (terminal-set-pty-write-handler! t
        (lambda (bv) (collect-fully) (set! pty (cons (utf8->string bv) pty))))
      (let ([input (bytes (string-append "abc\x7;def\x7;\x5;ghi\x1b;[6n" (make-string 50 #\z)))])
        (terminal-write! t input)
        (check-equal? 2 calls)
        (check-equal? '("rrrrrrrrrr" "\x1b;[1;10R") (reverse pty))
        (check-equal? (string-append "abcdefghi" (make-string 31 #\z) "\n" (make-string 19 #\z))
          (terminal->string t))))
    (let ([paste-seen '()])
      (terminal-set-pty-write-handler! t
        (lambda (bv) (collect-fully) (set! paste-seen (cons (utf8->string bv) paste-seen))))
      (check-equal? 'written (terminal-paste! t (make-string 5000 #\p)))
      (check-equal? (make-string 5000 #\p) (apply string-append (reverse paste-seen))))
    (terminal-close! t)))

(test "threads racing a close see whole results or a closed object"
  (do ([round 0 (+ round 1)]) ((= round 10))
    (let* ([t (make-terminal 80 24)]
           [rs (make-render-state)]
           [s (make-search t)]
           [outcomes '()]
           [m (make-mutex)]
           [record! (lambda (o) (with-mutex m (set! outcomes (cons o outcomes))))]
           [run (lambda (thunk)
                  (fork-thread
                    (lambda ()
                      (record!
                        (guard (c [(assertion-violation? c) 'closed])
                          (do ([i 0 (+ i 1)]) ((= i 50)) (thunk))
                          'complete)))))])
      (search-set-needle! s "x")
      (let ([threads
             (list
               (run (lambda () (terminal-write! t (make-bytevector 4096 120))))
               (run (lambda () (render-state-update! rs t) (render-state-rows rs)))
               (run (lambda () (search-feed! s) (search-total s)))
               (run (lambda () (terminal->string t))))])
        (terminal-close! t)
        (for-each thread-join threads)
        (check (for-all (lambda (o) (memq o '(complete closed))) outcomes))
        (check-equal? 4 (length outcomes))
        (search-close! s)
        (render-state-close! rs)))))

(test-exit)
