;; Not inside the terminal's callbacks: an unfinished write is not a state
;; the snapshot can describe.
(define (terminal->snapshot t)
  (define who 'terminal->snapshot)
  (with-terminal-change who t (pointer)
    (let ([out-ptr (pointer-out)] [out-len (pointer-out)])
      (check-result who (ghostty-snapshot-encode-alloc pointer 0 out-ptr out-len))
      (take-allocated-bytes out-ptr out-len))))

(define-enumeration snapshot-decoder-option
  (retain-continuation compress-history)
  snapshot-decoder-options)

(define (check-decoder-settings who max-continuation-bytes options)
  (check who (or (not max-continuation-bytes) (size? max-continuation-bytes))
    max-continuation-bytes "an unsigned 64-bit integer or #f")
  (check who (enum-set-of? options (snapshot-decoder-options)) options
    "a snapshot-decoder-options set"))

(define (configure-decoder! who decoder max-continuation-bytes options)
  (when max-continuation-bytes
    (let ([bv (make-bytevector 8)])
      (bytevector-u64-native-set! bv 0 max-continuation-bytes)
      (check-result who
        (ghostty-snapshot-decoder-set decoder
          (GhosttySnapshotDecoderOption max-continuation-bytes) bv))))
  (for-each
    (lambda (option)
      (let ([key (symbol->c-enum GhosttySnapshotDecoderOption option who)])
        (check-result who (ghostty-snapshot-decoder-set decoder key (bytevector 1)))))
    (enum-set->list options)))

(define (decoder-offset who decoder)
  (let ([out (make-bytevector 8 0)])
    (check-result who
      (ghostty-snapshot-decoder-get decoder (GhosttySnapshotDecoderData source-offset) out))
    (u64-ref out)))

;; The decoder reads the bytes in place until it finishes.
(define snapshot->terminal
  (case-lambda
    [(bytes) (snapshot->terminal bytes #f (snapshot-decoder-options))]
    [(bytes max-continuation-bytes)
     (snapshot->terminal bytes max-continuation-bytes (snapshot-decoder-options))]
    [(bytes max-continuation-bytes options)
     (define who 'snapshot->terminal)
     (check who (bytevector? bytes) bytes "a bytevector")
     (check-decoder-settings who max-continuation-bytes options)
     (release-dropped-natives!)
     (let ([copy (immobile-copy bytes)] [out (pointer-out)])
       (check-result who
         (ghostty-snapshot-decoder-new-buf 0 out (immobile-address copy) (bytevector-length copy)))
       (let ([decoder (pointer-in out)])
         (let ([result
                (dynamic-wind
                  void
                  (lambda ()
                    (configure-decoder! who decoder max-continuation-bytes options)
                    (let ([terminal (pointer-out)])
                      (check-result who (ghostty-snapshot-decoder-decode decoder terminal))
                      (let ([t (adopt-terminal (pointer-in terminal))])
                        (unless (= (decoder-offset who decoder) (bytevector-length bytes))
                          (terminal-close! t)
                          (raise-result who (GhosttyResult invalid-value) "bytes after the snapshot"))
                        t)))
                  (lambda () (ghostty-snapshot-decoder-free decoder)))])
           (keep-live copy)
           result)))]))

(define-record-type (snapshot-decoder new-snapshot-decoder snapshot-decoder?)
  (nongenerative)
  (sealed #t)
  (opaque #t)
  (fields native (mutable terminal)))

(define-value-record snapshot-progress (fields screen rows remaining))

(define (check-snapshot-decoder who d) (check who (snapshot-decoder? d) d "a snapshot-decoder"))

(define make-snapshot-decoder
  (case-lambda
    [(bytes) (make-snapshot-decoder bytes #f (snapshot-decoder-options))]
    [(bytes max-continuation-bytes)
     (make-snapshot-decoder bytes max-continuation-bytes (snapshot-decoder-options))]
    [(bytes max-continuation-bytes options)
     (define who 'make-snapshot-decoder)
     (check who (bytevector? bytes) bytes "a bytevector")
     (check-decoder-settings who max-continuation-bytes options)
     (release-dropped-natives!)
     (let ([copy (immobile-copy bytes)] [out (pointer-out)])
       (check-result who
         (ghostty-snapshot-decoder-new-buf 0 out (immobile-address copy) (bytevector-length copy)))
       ;; The decoder may read the bytes until it is freed, whenever that is.
       (let ([native (make-native (pointer-in out)
                       (lambda (pointer) (ghostty-snapshot-decoder-free pointer) (keep-live copy))
                       (make-lock #f))])
         (let ([d (register-native! (new-snapshot-decoder native #f) native)])
           (configure-decoder! who (native-pointer native) max-continuation-bytes options)
           d)))]))

;; The decoder borrows the terminal until it finishes, so closing the
;; terminal must close the decoder first.
(define (snapshot-decoder-ready! d)
  (define who 'snapshot-decoder-ready!)
  (check-snapshot-decoder who d)
  (when (snapshot-decoder-terminal d) (assertion-violationf who "the decoder is already ready"))
  (with-native who (snapshot-decoder-native d) "snapshot decoder" (pointer)
    (let ([out (pointer-out)])
      (check-result who (ghostty-snapshot-decoder-ready pointer out))
      (let* ([t (adopt-terminal (pointer-in out))] [n (terminal-native t)])
        (native-borrowers-set! n (cons (snapshot-decoder-native d) (native-borrowers n)))
        (snapshot-decoder-terminal-set! d t)
        t))))

(define (snapshot-decoder-closed? d)
  (check-snapshot-decoder 'snapshot-decoder-closed? d)
  (native-closed? (snapshot-decoder-native d)))

(define (snapshot-decoder-next! d)
  (define who 'snapshot-decoder-next!)
  (check-snapshot-decoder who d)
  (let ([t (snapshot-decoder-terminal d)])
    (unless t (assertion-violationf who "the decoder is not ready"))
    (with-terminal-change who t (terminal-pointer)
      (with-native who (snapshot-decoder-native d) "snapshot decoder" (pointer)
        (let ([code (ghostty-snapshot-decoder-next pointer)])
          (cond
            [(fx= code (GhosttyResult success))
             (let ([get (lambda (key size ref)
                          (let ([out (make-bytevector size 0)])
                            (check-result who (ghostty-snapshot-decoder-get pointer key out))
                            (ref out)))])
               (make-snapshot-progress
                 (get (GhosttySnapshotDecoderData progress-screen) 4
                   (lambda (bv) (c-enum->symbol GhosttyTerminalScreen (int-ref bv))))
                 (get (GhosttySnapshotDecoderData progress-rows) 8 u64-ref)
                 (get (GhosttySnapshotDecoderData progress-remaining) 4 u32-ref)))]
            [(fx= code (GhosttyResult no-value)) #f]
            [else (raise-result who code)]))))))

(define (snapshot-decoder-source-offset d)
  (define who 'snapshot-decoder-source-offset)
  (check-snapshot-decoder who d)
  (with-native who (snapshot-decoder-native d) "snapshot decoder" (pointer)
    (let* ([out (make-bytevector 8 0)]
           [code (ghostty-snapshot-decoder-get pointer (GhosttySnapshotDecoderData source-offset) out)])
      (cond
        [(fx= code (GhosttyResult success)) (u64-ref out)]
        [(fx= code (GhosttyResult no-value)) #f]
        [else (raise-result who code)]))))

(define (snapshot-decoder-history d)
  (define who 'snapshot-decoder-history)
  (check-snapshot-decoder who d)
  (with-native who (snapshot-decoder-native d) "snapshot decoder" (pointer)
    (let ([get (lambda (key)
                 (let* ([out (make-bytevector 8 0)] [code (ghostty-snapshot-decoder-get pointer key out)])
                   (cond
                     [(fx= code (GhosttyResult success)) (u64-ref out)]
                     [(fx= code (GhosttyResult no-value)) #f]
                     [else (raise-result who code)])))])
      (values (get (GhosttySnapshotDecoderData history-rows-primary))
              (get (GhosttySnapshotDecoderData history-rows-alternate))))))

(define (snapshot-decoder-close! d)
  (define who 'snapshot-decoder-close!)
  (check-snapshot-decoder who d)
  (let ([t (snapshot-decoder-terminal d)] [n (snapshot-decoder-native d)])
    (if t
        (let ([tn (terminal-native t)])
          (with-lock who (native-lock tn) "terminal"
            (when (lock-busy? (native-lock tn))
              (assertion-violationf who "cannot close a decoder inside its terminal's callbacks"))
            (close-native! who n "snapshot decoder")
            (native-borrowers-set! tn (remq n (native-borrowers tn)))))
        (close-native! who n "snapshot decoder"))))
