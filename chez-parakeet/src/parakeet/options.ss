;; An option left out keeps parakeet.cpp's own default.
(define omitted (list 'omitted))
(define (given? value) (not (eq? value omitted)))

(define (option who name value valid? default)
  (cond
    [(eq? value omitted) default]
    [(valid? value) value]
    [else (assertion-violationf who "~s is not a valid value for ~s" value name)]))

;; The values are evaluated in the order written, not in the order of fields.
(meta define (expand-options who x fields maker clauses)
  (let ([given
         (map (lambda (clause)
                (syntax-case clause ()
                  [(name value) (identifier? #'name) (cons (syntax->datum #'name) #'value)]
                  [_ (syntax-violation who "expected [name value]" x clause)]))
              clauses)])
    (let loop ([names (map car given)] [seen '()])
      (unless (null? names)
        (unless (memq (car names) fields)
          (syntax-violation who (format "unknown option ~s" (car names)) x))
        (when (memq (car names) seen)
          (syntax-violation who (format "option ~s given twice" (car names)) x))
        (loop (cdr names) (cons (car names) seen))))
    (with-syntax ([maker maker]
                  [((temporary value) ...)
                   (map (lambda (g) (list (car (generate-temporaries '(t))) (cdr g))) given)])
      (with-syntax ([(argument ...)
                     (map (lambda (field)
                            (let loop ([g given] [ts #'(temporary ...)])
                              (cond
                                [(null? g) #'omitted]
                                [(eq? (caar g) field) (car ts)]
                                [else (loop (cdr g) (cdr ts))])))
                          fields)])
        #'(let* ([temporary value] ...) (maker argument ...))))))

(define (finite-number? x) (and (real? x) (finite? x)))
(define (non-negative? x) (and (finite-number? x) (>= x 0)))
(define (positive-real? x) (and (finite-number? x) (> x 0)))
(define (probability? x) (and (finite-number? x) (<= 0 x 1)))
(define (language? x) (or (not x) (string? x)))
(define (index? x) (and (fixnum? x) (fx<= 0 x int-max)))
(define (count-at-least low) (lambda (x) (and (fixnum? x) (fx<= low x int-max))))

(define-enumeration decoder (default ctc transducer) decoders)
(define-enumeration vad-mode (speech segments) vad-modes)
(define-enumeration diarization-latency (model low very-low ultra-low) diarization-latencies)

(define (enumeration-member? set)
  (let ([universe (enum-set-universe set)])
    (lambda (x) (and (symbol? x) (enum-set-member? x universe)))))

(define decoder? (enumeration-member? (decoders)))
(define vad-mode? (enumeration-member? (vad-modes)))
(define diarization-latency? (enumeration-member? (diarization-latencies)))

(define (decoder-code d) (case d [(ctc) 1] [(transducer) 2] [else 0]))

(define (diarization-latency-code latency)
  (case latency
    [(low) diar-latency-low]
    [(very-low) diar-latency-very-low]
    [(ultra-low) diar-latency-ultra-low]
    [else diar-latency-model]))

(define-record-type decode-options-record
  (nongenerative) (sealed #t)
  (fields decoder language beam-size normalized-ranking given))

(define (checked-decode-options decoder language beam-size normalized-ranking)
  (define who 'decode-options)
  (make-decode-options-record
    (option who 'decoder decoder decoder? 'default)
    (option who 'language language language? #f)
    (option who 'beam-size beam-size (count-at-least 1) 4)
    (option who 'normalized-ranking normalized-ranking boolean? #t)
    (filter values
      (map (lambda (name value) (and (given? value) name))
           '(decoder language beam-size normalized-ranking)
           (list decoder language beam-size normalized-ranking)))))

(define-syntax decode-options
  (lambda (x)
    (syntax-case x ()
      [(_ clause ...)
       (expand-options 'decode-options x '(decoder language beam-size normalized-ranking)
         #'checked-decode-options #'(clause ...))])))

(define default-decode-options (decode-options))

(define (check-decode-options who options uses)
  (unless (decode-options-record? options)
    (assertion-violationf who "~s is not a decode-options value" options))
  (for-each
    (lambda (name)
      (unless (memq name uses) (assertion-violationf who "~s takes no ~s option" who name)))
    (decode-options-record-given options)))

;; JSON objects built from options: names and values are known, so only
;; numbers, booleans and fixed words are written.

(define (json-number x)
  (let ([x (inexact x)])
    (if (integer? x)
        (number->string (exact x))
        (number->string x))))

(define (json-object entries)
  (if (null? entries)
      #f
      (call-with-string-output-port
        (lambda (p)
          (put-char p #\{)
          (let loop ([entries entries] [first? #t])
            (unless (null? entries)
              (unless first? (put-char p #\,))
              (fprintf p "\"~a\":~a" (caar entries) (cdar entries))
              (loop (cdr entries) #f)))
          (put-char p #\})))))

(define (json-entry key value convert)
  (if (given? value) (list (cons key (convert value))) '()))

(define (json-boolean b) (if b "true" "false"))
(define (json-word s) (format "\"~a\"" s))

(define-record-type word-filter-record
  (nongenerative) (sealed #t)
  (fields entries))

(define (checked-word-filter min-local-confidence local-radius drop-punctuation-only)
  (option 'word-filter 'min-local-confidence min-local-confidence probability? omitted)
  (option 'word-filter 'local-radius local-radius positive-real? omitted)
  (option 'word-filter 'drop-punctuation-only drop-punctuation-only boolean? omitted)
  (make-word-filter-record
    (append (json-entry "min_local_conf" min-local-confidence json-number)
            (json-entry "local_radius" local-radius json-number)
            (json-entry "drop_punct_only" drop-punctuation-only json-boolean))))

(define-syntax word-filter
  (lambda (x)
    (syntax-case x ()
      [(_ clause ...)
       (expand-options 'word-filter x '(min-local-confidence local-radius drop-punctuation-only)
         #'checked-word-filter #'(clause ...))])))

(define (check-word-filter who filter)
  (unless (word-filter-record? filter)
    (assertion-violationf who "~s is not a word-filter value" filter)))

(define-record-type vad-options-record
  (nongenerative) (sealed #t)
  (fields entries))

(define (checked-vad-options threshold min-pause min-speech speech-pad max-segment trim
          run-gate mode probabilities)
  (define who 'vad-options)
  (option who 'threshold threshold (lambda (x) (and (finite-number? x) (< 0 x) (<= x 1))) omitted)
  (option who 'min-pause min-pause non-negative? omitted)
  (option who 'min-speech min-speech non-negative? omitted)
  (option who 'speech-pad speech-pad non-negative? omitted)
  (option who 'max-segment max-segment positive-real? omitted)
  (option who 'trim trim non-negative? omitted)
  (option who 'run-gate run-gate (lambda (x) (and (finite-number? x) (<= 0 x) (< x 1))) omitted)
  (option who 'mode mode vad-mode? omitted)
  (option who 'probabilities probabilities boolean? omitted)
  (make-vad-options-record
    (append (json-entry "threshold" threshold json-number)
            (json-entry "min_pause" min-pause json-number)
            (json-entry "min_speech" min-speech json-number)
            (json-entry "speech_pad" speech-pad json-number)
            (json-entry "max_segment" max-segment json-number)
            (json-entry "trim" trim json-number)
            (json-entry "run_gate" run-gate json-number)
            (json-entry "mode" mode json-word)
            (json-entry "probabilities" probabilities json-boolean))))

(define-syntax vad-options
  (lambda (x)
    (syntax-case x ()
      [(_ clause ...)
       (expand-options 'vad-options x
         '(threshold min-pause min-speech speech-pad max-segment trim run-gate mode probabilities)
         #'checked-vad-options #'(clause ...))])))

(define default-vad-options (vad-options))

(define (check-vad-options who options)
  (unless (vad-options-record? options)
    (assertion-violationf who "~s is not a vad-options value" options)))

(define-record-type asr-stream-options-record
  (nongenerative) (sealed #t)
  (fields language preset left-frames chunk-frames right-frames))

(define (checked-asr-stream-options language preset left-frames chunk-frames right-frames)
  (define who 'asr-stream-options)
  (let ([language (option who 'language language language? #f)]
        [preset (option who 'preset preset index? omitted)]
        [left (option who 'left-frames left-frames index? omitted)]
        [chunk (option who 'chunk-frames chunk-frames (count-at-least 1) omitted)]
        [right (option who 'right-frames right-frames index? omitted)])
    (let ([window (map given? (list left chunk right))])
      (unless (or (for-all values window) (for-all not window))
        (assertion-violationf who "left-frames, chunk-frames and right-frames go together"))
      (when (and (car window) (given? preset))
        (assertion-violationf who "a preset and a window exclude each other")))
    (make-asr-stream-options-record language preset left chunk right)))

(define-syntax asr-stream-options
  (lambda (x)
    (syntax-case x ()
      [(_ clause ...)
       (expand-options 'asr-stream-options x
         '(language preset left-frames chunk-frames right-frames)
         #'checked-asr-stream-options #'(clause ...))])))

(define default-asr-stream-options (asr-stream-options))

;; parakeet.cpp's defaults for sound and scene options are read from it when
;; the options are used, so left-out fields stay omitted here.

(define-record-type sound-options-record
  (nongenerative) (sealed #t)
  (fields window hop on-threshold off-threshold min-duration top-k))

(define (checked-sound-options window hop on-threshold off-threshold min-duration top-k)
  (define who 'sound-options)
  (make-sound-options-record
    (option who 'window window positive-real? omitted)
    (option who 'hop hop positive-real? omitted)
    (option who 'on-threshold on-threshold probability? omitted)
    (option who 'off-threshold off-threshold probability? omitted)
    (option who 'min-duration min-duration non-negative? omitted)
    (option who 'top-k top-k index? omitted)))

(define-syntax sound-options
  (lambda (x)
    (syntax-case x ()
      [(_ clause ...)
       (expand-options 'sound-options x
         '(window hop on-threshold off-threshold min-duration top-k)
         #'checked-sound-options #'(clause ...))])))

(define default-sound-options (sound-options))

(define (check-sound-options who options)
  (unless (sound-options-record? options)
    (assertion-violationf who "~s is not a sound-options value" options)))

(define-record-type speaker-options-record
  (nongenerative) (sealed #t)
  (fields accept-threshold margin min-voice refresh max-voice))

;; parakeet.cpp reads 0 as its default for each of these, so 0 is refused
;; rather than silently replaced.
(define (checked-speaker-options accept-threshold margin min-voice refresh max-voice)
  (define who 'speaker-options)
  (make-speaker-options-record
    (option who 'accept-threshold accept-threshold
      (lambda (x) (and (finite-number? x) (<= -1 x 1) (not (zero? x)))) omitted)
    (option who 'margin margin positive-real? omitted)
    (option who 'min-voice min-voice positive-real? omitted)
    (option who 'refresh refresh positive-real? omitted)
    (option who 'max-voice max-voice positive-real? omitted)))

(define-syntax speaker-options
  (lambda (x)
    (syntax-case x ()
      [(_ clause ...)
       (expand-options 'speaker-options x '(accept-threshold margin min-voice refresh max-voice)
         #'checked-speaker-options #'(clause ...))])))

(define default-speaker-options (speaker-options))

(define (check-speaker-options who options)
  (unless (speaker-options-record? options)
    (assertion-violationf who "~s is not a speaker-options value" options)))

(define-record-type scene-options-record
  (nongenerative) (sealed #t)
  (fields diarization-latency sound speaker))

(define (checked-scene-options latency sound speaker)
  (define who 'scene-options)
  (make-scene-options-record
    (option who 'diarization-latency latency diarization-latency? 'model)
    (option who 'sound sound sound-options-record? default-sound-options)
    (option who 'speaker speaker speaker-options-record? default-speaker-options)))

(define-syntax scene-options
  (lambda (x)
    (syntax-case x ()
      [(_ clause ...)
       (expand-options 'scene-options x '(diarization-latency sound speaker)
         #'checked-scene-options #'(clause ...))])))

(define default-scene-options (scene-options))

(define (check-scene-options who options)
  (unless (scene-options-record? options)
    (assertion-violationf who "~s is not a scene-options value" options)))
