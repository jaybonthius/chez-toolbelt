(import (chezscheme) (parakeet) (check))

(define header "build/source/include/parakeet_capi.h")
(define bindings "src/parakeet/native.sls")

(define subsumed
  '(("parakeet_capi_transcribe_path" . "parakeet_capi_transcribe_path_lang")
    ("parakeet_capi_transcribe_pcm" . "parakeet_capi_transcribe_pcm_lang")
    ("parakeet_capi_transcribe_pcm_batch" . "parakeet_capi_transcribe_pcm_batch_lang")
    ("parakeet_capi_transcribe_pcm_batch_json" . "parakeet_capi_transcribe_pcm_batch_json_lang")
    ("parakeet_capi_transcribe_path_json_vad" . "parakeet_capi_transcribe_path_json_vad_with")
    ("parakeet_capi_stream_begin" . "parakeet_capi_stream_begin_opts")
    ("parakeet_capi_stream_begin_lang" . "parakeet_capi_stream_begin_opts")
    ("parakeet_capi_diarize_stream_begin" . "parakeet_capi_diarize_stream_begin_latency")
    ("parakeet_capi_sas_stream_begin" . "parakeet_capi_sas_stream_begin_latency")
    ("parakeet_capi_scene_stream_begin" . "parakeet_capi_scene_stream_begin_speaker")
    ("parakeet_capi_speaker_registry_add_embedding"
     . "parakeet_capi_speaker_registry_add_embedding_fp")
    ("parakeet_capi_last_error" . "chez_parakeet_last_error")))

(define (file-text path) (call-with-port (open-input-file path) get-string-all))

(define (without-comments text)
  (let ([out (open-output-string)] [n (string-length text)])
    (let loop ([i 0])
      (cond
        [(>= i n) (get-output-string out)]
        [(and (< (+ i 1) n) (char=? (string-ref text i) #\/) (char=? (string-ref text (+ i 1)) #\/))
         (let skip ([i i])
           (if (or (>= i n) (char=? (string-ref text i) #\newline)) (loop i) (skip (+ i 1))))]
        [(and (< (+ i 1) n) (char=? (string-ref text i) #\/) (char=? (string-ref text (+ i 1)) #\*))
         (let skip ([i (+ i 2)])
           (if (and (< (+ i 1) n) (char=? (string-ref text i) #\*)
                    (char=? (string-ref text (+ i 1)) #\/))
               (loop (+ i 2))
               (skip (+ i 1))))]
        [else (put-char out (string-ref text i)) (loop (+ i 1))]))))

(define (identifier-char? c) (or (char-alphabetic? c) (char-numeric? c) (char=? c #\_)))

(define (identifiers text prefix call?)
  (let ([n (string-length text)] [k (string-length prefix)])
    (let loop ([i 0] [found '()])
      (cond
        [(> (+ i k) n) (reverse found)]
        [(and (string=? (substring text i (+ i k)) prefix)
              (or (= i 0) (not (identifier-char? (string-ref text (- i 1))))))
         (let end ([j (+ i k)])
           (if (and (< j n) (identifier-char? (string-ref text j)))
               (end (+ j 1))
               (let ([name (substring text i j)]
                     [next (let skip ([j j])
                             (if (and (< j n) (char-whitespace? (string-ref text j)))
                                 (skip (+ j 1))
                                 (and (< j n) (string-ref text j))))])
                 (loop j (if (and (> j (+ i k))
                                  (or (not call?) (eqv? next #\())
                                  (not (member name found)))
                             (cons name found)
                             found)))))]
        [else (loop (+ i 1) found)]))))

(test "the binding is built against C API version 11"
  (check-equal? 11 (abi-version)))

(test "every function of parakeet_capi.h is bound, or reached through one that is"
  (let ([declared (identifiers (without-comments (file-text header)) "parakeet_capi_" #t)]
        [bound (identifiers (file-text bindings) "parakeet_capi_" #f)])
    (check (> (length declared) 100))
    (for-each
      (lambda (name)
        (check-equal? (list name #t)
          (list name (or (and (member name bound) #t)
                         (let ([entry (assoc name subsumed)])
                           (and entry (member (cdr entry) (cons "chez_parakeet_last_error" bound))
                                #t))))))
      declared)
    (for-each (lambda (name) (check-equal? (list name #t) (list name (and (member name declared) #t))))
      bound)
    (for-each (lambda (entry) (check (not (member (car entry) bound)))) subsumed)
    (printf "~a C functions: ~a bound, ~a reached through another\n"
      (length declared) (length bound) (length subsumed))))

(test-exit)
