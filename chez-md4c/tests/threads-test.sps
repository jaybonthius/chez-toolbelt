(import (chezscheme) (md4c) (check) (support))

(define documents
  (list
    "# Title\n\nSome **bold** and `code` with [a link](https://x.org \"t\").\n\n- [x] done\n- é 日本 😀\n"
    (apply string-append (make-list 2000 "> quote with *emphasis* &amp; ~~more~~\n>\n"))
    (apply string-append (make-list 500 "| a | b |\n|:-|-:|\n| 1 | ![i](https://i.org) |\n\n```js\nx\n```\n\n"))
    (apply string-append (make-list 3000 "1. [a][r] <https://y.org> www.z.org\n   - nested\n\n"))
    "[r]: https://r.org\n"))

(define expected (map (lambda (d) (blocks->datum (markdown-parse d))) documents))

(define (in-threads count thunk)
  (let ([m (make-mutex)] [done (make-condition)] [results '()])
    (do ([i 0 (+ i 1)]) ((= i count))
      (fork-thread
        (lambda ()
          (let ([result (guard (c [#t c]) (thunk))])
            (with-mutex m
              (set! results (cons result results))
              (condition-signal done))))))
    (with-mutex m
      (let wait ()
        (unless (= (length results) count)
          (condition-wait done m)
          (wait))))
    results))

;; The documents are moved by collections other threads run while md4c
;; reads them, unless they are locked.
(test "parses on many threads, collecting all the while, agree with one"
  (let ([stop? (box #f)])
    (fork-thread
      (lambda ()
        (let loop ()
          (unless (unbox stop?)
            (collect-rendezvous)
            (loop)))))
    (let ([results (in-threads 8
                     (lambda ()
                       (let loop ([n 0] [same? #t])
                         (if (= n 20)
                             same?
                             (loop (+ n 1)
                                   (and same?
                                        (equal? expected
                                          (map (lambda (d) (blocks->datum (markdown-parse d)))
                                               documents))))))))])
      (set-box! stop? #t)
      (check-equal? (make-list 8 #t) results))))

;; md4c runs without holding up the collector: a collection another thread
;; asks for while md4c works waits only for the collection itself. md4c
;; takes hundreds of milliseconds over this document and returns one text.
(test "collections run while md4c parses"
  (let* ([document (make-string 6000000 #\[)]
         [parsing? (box #t)]
         [longest (box 0)])
    (fork-thread
      (lambda ()
        (markdown-parse document)
        (markdown-parse document)
        (set-box! parsing? #f)))
    (let loop ()
      (when (unbox parsing?)
        (let ([start (real-time)])
          (collect-rendezvous)
          (set-box! longest (max (unbox longest) (- (real-time) start))))
        (sleep (make-time 'time-duration 5000000 0))
        (loop)))
    (check-equal? '(#t) (list (< (unbox longest) 100)))))

(test-exit)
