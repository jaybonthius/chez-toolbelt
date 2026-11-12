(library (parakeet native)
  (export
    event-eou event-eob
    stream-kind-none stream-kind-cache-aware stream-kind-buffered stream-cap-eou-events
    stream-preset-default stream-preset-custom
    diar-latency-model diar-latency-low diar-latency-very-low diar-latency-ultra-low
    model-kind-none model-kind-asr model-kind-diarization model-kind-sound model-kind-speaker
    model-kind-vad
    grammar-ok grammar-no-match grammar-error
    stream-event-size stream-event-token-offset stream-event-is-eob-offset stream-event-encoder-frame-offset
    stream-event-time-sec-offset
    stream-opts-size stream-opts-size-field-offset stream-opts-target-lang-offset stream-opts-preset-offset
    stream-opts-left-frames-offset stream-opts-chunk-frames-offset stream-opts-right-frames-offset stream-opts-flags-offset
    sas-result-size sas-result-speaker-offset sas-result-text-offset sas-result-start-offset sas-result-end-offset
    sas-result-conf-offset
    diar-segment-size diar-segment-speaker-offset diar-segment-start-offset diar-segment-end-offset
    sound-opts-size sound-opts-size-field-offset sound-opts-window-sec-offset sound-opts-hop-sec-offset
    sound-opts-on-threshold-offset sound-opts-off-threshold-offset sound-opts-min-duration-sec-offset
    sound-opts-top-k-offset
    sound-segment-size sound-segment-class-index-offset sound-segment-label-offset sound-segment-start-offset
    sound-segment-end-offset sound-segment-peak-offset
    scene-opts-size scene-opts-size-field-offset scene-opts-diar-latency-offset scene-opts-sound-offset
    scene-opts-flags-offset scene-opts-speaker-accept-threshold-offset scene-opts-speaker-margin-offset
    scene-opts-speaker-min-voice-sec-offset scene-opts-speaker-refresh-sec-offset
    scene-opts-speaker-max-voice-sec-offset

    handle handle? handle-address handle-parents handle-borrowed?
    handle-protocol handle-open? register-handle! close-handle! with-handles call-with-handles
    free-unreachable-handles! live-handle-count

    &parakeet-error make-parakeet-error parakeet-error? raise-parakeet-error
    context-error load-error

    int-max check-samples sample-count check-sample-rate check-int check-path string->c
    with-locked make-out-pointers out-pointer-ref out-int-ref
    c-string->string take-string c-memcpy-from c-memcpy-to

    capi-abi-version capi-load capi-load-component capi-bundle-components-json capi-load-error
    capi-free capi-free-string capi-model-kind capi-set-concurrency
    capi-transcribe-path-lang capi-transcribe-pcm-lang
    capi-transcribe-pcm-batch-lang capi-transcribe-pcm-batch-json-lang
    capi-transcribe-path-json capi-transcribe-path-json-with capi-transcribe-path-json-vad-with
    capi-transcribe-path-nbest-json capi-transcribe-pcm-nbest-json
    capi-transcribe-pcm-logits capi-free-logits
    capi-vad-pcm-json capi-vad-path-json
    capi-vad-stream-begin capi-vad-stream-feed-json capi-vad-stream-reset capi-vad-stream-free
    capi-grammar-create capi-grammar-free
    capi-transcribe-pcm-grammar capi-transcribe-path-grammar
    capi-transcribe-pcm-grammar-json capi-transcribe-path-grammar-json
    capi-grammar-stream-begin capi-grammar-stream-feed-json capi-grammar-stream-finalize-json
    capi-grammar-stream-reset capi-grammar-stream-free
    capi-stream-kind capi-stream-caps capi-stream-opts-default capi-stream-begin-opts
    capi-stream-presets-json capi-stream-info-json capi-stream-reset
    capi-stream-feed capi-stream-finalize capi-stream-feed-json capi-stream-finalize-json
    capi-stream-drain-events capi-free-events capi-stream-free
    capi-diarize-path capi-diarize-pcm
    capi-transcribe-and-diarize capi-free-sas-results capi-transcribe-and-diarize-json
    capi-diarize-stream-begin-latency capi-diarize-stream-chunk-samples capi-diarize-stream-time
    capi-diarize-stream-active capi-diarize-stream-feed capi-free-diar-segments
    capi-diarize-stream-free
    capi-sas-stream-begin-latency capi-sas-stream-feed capi-sas-stream-free
    capi-sound-opts-default capi-sound-stream-begin capi-sound-stream-feed
    capi-sound-stream-active capi-sound-stream-drain-scores-json capi-free-sound-segments
    capi-sound-stream-free capi-num-classes capi-class-label
    capi-scene-opts-default capi-scene-stream-begin-speaker capi-scene-stream-feed-json
    capi-scene-stream-drain-scores-json capi-scene-stream-last-error capi-scene-stream-free
    capi-speaker-dim capi-speaker-registry-new capi-speaker-registry-free
    capi-speaker-registry-size capi-speaker-registry-last-error capi-speaker-enroll
    capi-speaker-embed-pcm capi-free-floats capi-speaker-registry-add-embedding-fp
    capi-speaker-registry-encoder-family capi-speaker-registry-encoder-weights
    capi-speaker-registry-set-strict capi-speaker-encoder-family capi-speaker-last-warning
    capi-speaker-registry-save capi-speaker-registry-load capi-speaker-identify-pcm-json
    capi-transcribe-and-diarize-named-json capi-diarize-named-pcm-json
    capi-diarize-profiles-pcm-json capi-speaker-identity)
  (import (chezscheme) (parakeet layout))

  (define libparakeet (load-shared-object libparakeet-path))
  (define shim (load-shared-object shim-path))

  ;; Every call that can run a model, read a file or wait is collect-safe, so
  ;; that other Scheme threads keep running and collecting meanwhile. Such a
  ;; call must not be passed movable memory: callers lock what they pass
  ;; (with-locked) and never pass strings, whose conversion is not allowed.
  (define-syntax define-blocking
    (syntax-rules ()
      [(_ name c-name (arg ...) result)
       (define name (foreign-procedure __collect_safe c-name (arg ...) result))]))

  (define-syntax define-quick
    (syntax-rules ()
      [(_ name c-name (arg ...) result)
       (define name (foreign-procedure c-name (arg ...) result))]))

  (define-quick c-strlen "strlen" (uptr) size_t)
  (define-quick c-memcpy-from "memcpy" (u8* uptr size_t) uptr)
  (define-quick c-memcpy-to "memcpy" (uptr u8* size_t) uptr)
  (define-quick shim-last-error "chez_parakeet_last_error" (uptr) uptr)

  (define-quick capi-abi-version "parakeet_capi_abi_version" () int)
  (define-blocking capi-load "parakeet_capi_load" (u8*) uptr)
  (define-blocking capi-load-component "parakeet_capi_load_component" (u8* u8*) uptr)
  (define-blocking capi-bundle-components-json "parakeet_capi_bundle_components_json" (u8*) uptr)
  (define-quick capi-load-error "parakeet_capi_load_error" () uptr)
  (define-blocking capi-free "parakeet_capi_free" (uptr) void)
  (define-quick capi-free-string "parakeet_capi_free_string" (uptr) void)
  (define-quick capi-model-kind "parakeet_capi_model_kind" (uptr) int)
  (define-blocking capi-set-concurrency "parakeet_capi_set_concurrency" (uptr int int) int)

  (define-blocking capi-transcribe-path-lang "parakeet_capi_transcribe_path_lang"
    (uptr u8* int u8*) uptr)
  (define-blocking capi-transcribe-pcm-lang "parakeet_capi_transcribe_pcm_lang"
    (uptr u8* int int int u8*) uptr)
  (define-blocking capi-transcribe-pcm-batch-lang "parakeet_capi_transcribe_pcm_batch_lang"
    (uptr u8* u8* int int int u8* uptr) int)
  (define-blocking capi-transcribe-pcm-batch-json-lang
    "parakeet_capi_transcribe_pcm_batch_json_lang" (uptr uptr u8* int int int u8*) uptr)
  (define-blocking capi-transcribe-path-json "parakeet_capi_transcribe_path_json"
    (uptr u8* int) uptr)
  (define-blocking capi-transcribe-path-json-with "parakeet_capi_transcribe_path_json_with"
    (uptr u8* int u8*) uptr)
  (define-blocking capi-transcribe-path-json-vad-with
    "parakeet_capi_transcribe_path_json_vad_with" (uptr uptr u8* int u8*) uptr)
  (define-blocking capi-transcribe-path-nbest-json "parakeet_capi_transcribe_path_nbest_json"
    (uptr u8* int int int u8*) uptr)
  (define-blocking capi-transcribe-pcm-nbest-json "parakeet_capi_transcribe_pcm_nbest_json"
    (uptr u8* int int int int int u8*) uptr)
  (define-blocking capi-transcribe-pcm-logits "parakeet_capi_transcribe_pcm_logits"
    (uptr u8* int int u8* u8* u8*) int)
  (define-quick capi-free-logits "parakeet_capi_free_logits" (uptr) void)

  (define-blocking capi-vad-pcm-json "parakeet_capi_vad_pcm_json" (uptr u8* int int u8*) uptr)
  (define-blocking capi-vad-path-json "parakeet_capi_vad_path_json" (uptr u8* u8*) uptr)
  (define-blocking capi-vad-stream-begin "parakeet_capi_vad_stream_begin" (uptr int u8*) uptr)
  (define-blocking capi-vad-stream-feed-json "parakeet_capi_vad_stream_feed_json"
    (uptr u8* int int) uptr)
  (define-blocking capi-vad-stream-reset "parakeet_capi_vad_stream_reset" (uptr) int)
  (define-blocking capi-vad-stream-free "parakeet_capi_vad_stream_free" (uptr) void)

  (define-blocking capi-grammar-create "parakeet_capi_grammar_create" (uptr u8*) uptr)
  (define-blocking capi-grammar-free "parakeet_capi_grammar_free" (uptr) void)
  (define-blocking capi-transcribe-pcm-grammar "parakeet_capi_transcribe_pcm_grammar"
    (uptr uptr u8* int int int int u8* u8*) int)
  (define-blocking capi-transcribe-path-grammar "parakeet_capi_transcribe_path_grammar"
    (uptr uptr u8* int int u8* u8*) int)
  (define-blocking capi-transcribe-pcm-grammar-json "parakeet_capi_transcribe_pcm_grammar_json"
    (uptr uptr u8* int int int int u8* u8*) int)
  (define-blocking capi-transcribe-path-grammar-json "parakeet_capi_transcribe_path_grammar_json"
    (uptr uptr u8* int int u8* u8*) int)
  (define-blocking capi-grammar-stream-begin "parakeet_capi_grammar_stream_begin"
    (uptr uptr int u8*) uptr)
  (define-blocking capi-grammar-stream-feed-json "parakeet_capi_grammar_stream_feed_json"
    (uptr u8* int u8*) int)
  (define-blocking capi-grammar-stream-finalize-json "parakeet_capi_grammar_stream_finalize_json"
    (uptr u8*) int)
  (define-blocking capi-grammar-stream-reset "parakeet_capi_grammar_stream_reset" (uptr) int)
  (define-blocking capi-grammar-stream-free "parakeet_capi_grammar_stream_free" (uptr) void)

  (define-quick capi-stream-kind "parakeet_capi_stream_kind" (uptr) int)
  (define-quick capi-stream-caps "parakeet_capi_stream_caps" (uptr) int)
  (define-quick capi-stream-opts-default "parakeet_capi_stream_opts_default" (uptr) void)
  (define-blocking capi-stream-begin-opts "parakeet_capi_stream_begin_opts" (uptr uptr) uptr)
  (define-blocking capi-stream-presets-json "parakeet_capi_stream_presets_json" (uptr) uptr)
  (define-blocking capi-stream-info-json "parakeet_capi_stream_info_json" (uptr) uptr)
  (define-blocking capi-stream-reset "parakeet_capi_stream_reset" (uptr) int)
  (define-blocking capi-stream-feed "parakeet_capi_stream_feed" (uptr u8* int u8*) uptr)
  (define-blocking capi-stream-finalize "parakeet_capi_stream_finalize" (uptr) uptr)
  (define-blocking capi-stream-feed-json "parakeet_capi_stream_feed_json" (uptr u8* int) uptr)
  (define-blocking capi-stream-finalize-json "parakeet_capi_stream_finalize_json" (uptr) uptr)
  (define-blocking capi-stream-drain-events "parakeet_capi_stream_drain_events" (uptr u8*) int)
  (define-quick capi-free-events "parakeet_capi_free_events" (uptr) void)
  (define-blocking capi-stream-free "parakeet_capi_stream_free" (uptr) void)

  (define-blocking capi-diarize-path "parakeet_capi_diarize_path" (uptr u8*) uptr)
  (define-blocking capi-diarize-pcm "parakeet_capi_diarize_pcm" (uptr u8* int int) uptr)
  (define-blocking capi-transcribe-and-diarize "parakeet_capi_transcribe_and_diarize"
    (uptr uptr u8* int int u8* u8*) int)
  (define-quick capi-free-sas-results "parakeet_capi_free_sas_results" (uptr int) void)
  (define-blocking capi-transcribe-and-diarize-json "parakeet_capi_transcribe_and_diarize_json"
    (uptr uptr u8* int int) uptr)
  (define-blocking capi-diarize-stream-begin-latency "parakeet_capi_diarize_stream_begin_latency"
    (uptr int) uptr)
  (define-quick capi-diarize-stream-chunk-samples "parakeet_capi_diarize_stream_chunk_samples"
    (uptr) int)
  (define-quick capi-diarize-stream-time "parakeet_capi_diarize_stream_time" (uptr) float)
  (define-blocking capi-diarize-stream-active "parakeet_capi_diarize_stream_active"
    (uptr u8* u8*) int)
  (define-blocking capi-diarize-stream-feed "parakeet_capi_diarize_stream_feed"
    (uptr u8* int int u8* u8*) int)
  (define-quick capi-free-diar-segments "parakeet_capi_free_diar_segments" (uptr) void)
  (define-blocking capi-diarize-stream-free "parakeet_capi_diarize_stream_free" (uptr) void)
  (define-blocking capi-sas-stream-begin-latency "parakeet_capi_sas_stream_begin_latency"
    (uptr uptr int) uptr)
  (define-blocking capi-sas-stream-feed "parakeet_capi_sas_stream_feed"
    (uptr u8* int int u8* u8*) int)
  (define-blocking capi-sas-stream-free "parakeet_capi_sas_stream_free" (uptr) void)

  (define-quick capi-sound-opts-default "parakeet_capi_sound_opts_default" (uptr) void)
  (define-blocking capi-sound-stream-begin "parakeet_capi_sound_stream_begin" (uptr uptr) uptr)
  (define-blocking capi-sound-stream-feed "parakeet_capi_sound_stream_feed"
    (uptr u8* int int u8* u8*) int)
  (define-blocking capi-sound-stream-active "parakeet_capi_sound_stream_active"
    (uptr u8* u8*) int)
  (define-blocking capi-sound-stream-drain-scores-json
    "parakeet_capi_sound_stream_drain_scores_json" (uptr) uptr)
  (define-quick capi-free-sound-segments "parakeet_capi_free_sound_segments" (uptr) void)
  (define-blocking capi-sound-stream-free "parakeet_capi_sound_stream_free" (uptr) void)
  (define-quick capi-num-classes "parakeet_capi_num_classes" (uptr) int)
  (define-quick capi-class-label "parakeet_capi_class_label" (uptr int) uptr)

  (define-quick capi-scene-opts-default "parakeet_capi_scene_opts_default" (uptr) void)
  (define-blocking capi-scene-stream-begin-speaker "parakeet_capi_scene_stream_begin_speaker"
    (uptr uptr uptr uptr uptr uptr) uptr)
  (define-blocking capi-scene-stream-feed-json "parakeet_capi_scene_stream_feed_json"
    (uptr u8* int int) uptr)
  (define-blocking capi-scene-stream-drain-scores-json
    "parakeet_capi_scene_stream_drain_scores_json" (uptr) uptr)
  (define-quick capi-scene-stream-last-error "parakeet_capi_scene_stream_last_error" (uptr) uptr)
  (define-blocking capi-scene-stream-free "parakeet_capi_scene_stream_free" (uptr) void)

  (define-quick capi-speaker-dim "parakeet_capi_speaker_dim" (uptr) int)
  (define-quick capi-speaker-registry-new "parakeet_capi_speaker_registry_new" () uptr)
  (define-quick capi-speaker-registry-free "parakeet_capi_speaker_registry_free" (uptr) void)
  (define-quick capi-speaker-registry-size "parakeet_capi_speaker_registry_size" (uptr) int)
  (define-quick capi-speaker-registry-last-error "parakeet_capi_speaker_registry_last_error"
    (uptr) uptr)
  (define-blocking capi-speaker-enroll "parakeet_capi_speaker_enroll"
    (uptr uptr u8* u8* int int) int)
  (define-blocking capi-speaker-embed-pcm "parakeet_capi_speaker_embed_pcm"
    (uptr u8* int int u8* u8*) int)
  (define-quick capi-free-floats "parakeet_capi_free_floats" (uptr) void)
  (define-blocking capi-speaker-registry-add-embedding-fp
    "parakeet_capi_speaker_registry_add_embedding_fp" (uptr u8* u8* int u8* u8*) int)
  (define-quick capi-speaker-registry-encoder-family
    "parakeet_capi_speaker_registry_encoder_family" (uptr) uptr)
  (define-quick capi-speaker-registry-encoder-weights
    "parakeet_capi_speaker_registry_encoder_weights" (uptr) uptr)
  (define-quick capi-speaker-registry-set-strict "parakeet_capi_speaker_registry_set_strict"
    (uptr int) void)
  (define-quick capi-speaker-encoder-family "parakeet_capi_speaker_encoder_family" (uptr) uptr)
  (define-quick capi-speaker-last-warning "parakeet_capi_speaker_last_warning" (uptr) uptr)
  (define-blocking capi-speaker-registry-save "parakeet_capi_speaker_registry_save"
    (uptr u8*) int)
  (define-blocking capi-speaker-registry-load "parakeet_capi_speaker_registry_load" (u8*) uptr)
  (define-blocking capi-speaker-identify-pcm-json "parakeet_capi_speaker_identify_pcm_json"
    (uptr uptr u8* int int) uptr)
  (define-blocking capi-transcribe-and-diarize-named-json
    "parakeet_capi_transcribe_and_diarize_named_json" (uptr uptr uptr uptr u8* int int) uptr)
  (define-blocking capi-diarize-named-pcm-json "parakeet_capi_diarize_named_pcm_json"
    (uptr uptr uptr u8* int int float float) uptr)
  (define-blocking capi-diarize-profiles-pcm-json "parakeet_capi_diarize_profiles_pcm_json"
    (uptr uptr uptr u8* int int float float) uptr)
  (define-quick capi-speaker-identity "parakeet_capi_speaker_identity" (uptr) uptr)

  (define-condition-type &parakeet-error &error make-parakeet-error parakeet-error?)

  (define (raise-parakeet-error who message)
    (raise (condition (make-parakeet-error) (make-who-condition who)
             (make-message-condition
               (if (string=? message "") "parakeet.cpp failed without a message" message)))))

  ;; The copy is freed by parakeet.cpp's free: a free looked up by name can
  ;; belong to another allocator than the shim's strdup, when one is preloaded.
  (define (context-error address)
    (or (take-string (shim-last-error address))
        "parakeet.cpp's error message could not be copied"))

  ;; The message is per thread and kept until the thread's next load, so it is
  ;; read on the thread that loaded, right after the load.
  (define (load-error) (c-string->string (capi-load-error)))

  (define int-max #x7fffffff)

  (define (check-int who what n low)
    (unless (and (fixnum? n) (fx<= low n int-max))
      (assertion-violationf who "~s is not a valid ~a" n what)))

  (define (check-samples who samples)
    (unless (and (bytevector? samples) (fx= (fxand (bytevector-length samples) 3) 0)
                 (fx<= (fxsrl (bytevector-length samples) 2) int-max))
      (assertion-violationf who "~s is not a bytevector of 32-bit float samples" samples)))

  (define (sample-count samples) (fxsrl (bytevector-length samples) 2))

  (define (check-sample-rate who rate) (check-int who "sample rate" rate 1))

  (define (check-path who path)
    (unless (string? path) (assertion-violationf who "~s is not a path string" path)))

  (define (string->c who s)
    (cond
      [(not s) #f]
      [(not (string? s)) (assertion-violationf who "~s is not a string" s)]
      [else
       (let loop ([i 0])
         (when (fx< i (string-length s))
           (when (char=? (string-ref s i) #\nul)
             (assertion-violationf who "~s contains a NUL character" s))
           (loop (fx+ i 1))))
       (let* ([utf8 (string->utf8 s)]
              [n (bytevector-length utf8)]
              [c (make-bytevector (fx+ n 1) 0)])
         (bytevector-copy! utf8 0 c 0 n)
         c)]))

  ;; lock-object counts, so one object may be named twice.
  (define-syntax with-locked
    (syntax-rules ()
      [(_ (x ...) e1 e2 ...)
       (dynamic-wind
         (lambda () (lock-object x) ...)
         (lambda () e1 e2 ...)
         (lambda () (unlock-object x) ...))]))

  (define (make-out-pointers n) (make-bytevector (fx* n 8) 0))
  (define (out-pointer-ref out i) (bytevector-u64-native-ref out (fx* i 8)))
  (define (out-int-ref out i) (bytevector-s32-native-ref out (fx* i 8)))

  (define (c-string->string address)
    (let* ([n (c-strlen address)] [bytes (make-bytevector n)])
      (c-memcpy-from bytes address n)
      (utf8->string bytes)))

  (define (take-string address)
    (if (eqv? address 0)
        #f
        (dynamic-wind void
          (lambda () (c-string->string address))
          (lambda () (capi-free-string address)))))

  ;; A handle owns one native object. Calls hold it shared or exclusive, as
  ;; parakeet.cpp allows for that object, and close waits for running calls,
  ;; so no call sees a freed object and no object runs two calls it forbids.
  ;; A handle holds the handles it borrows (its parents), which therefore
  ;; outlive it, and closing a parent closes its children first. Handles
  ;; are always taken in the order of their ids, and a child's id is larger
  ;; than its parents', so two calls never wait for each other.

  ;; children is a list of weak pairs, so that a dropped child can still be
  ;; collected. The guardian leaves a child in the car of a weak pair until
  ;; it has been returned and dropped, which happens only after it is freed,
  ;; so closing a parent finds every child not yet freed.
  (define-record-type handle
    (nongenerative)
    (fields (mutable address) id mutex condition (mutable users) (mutable exclusive?)
            (mutable children) parents free)
    (protocol
      (lambda (new)
        (lambda (address parents free)
          (new address (next-id!) (make-mutex) (make-condition) 0 #f '() parents free)))))

  (define handle-protocol
    (lambda (pargs->new)
      (lambda (address parents free . fields)
        (apply (pargs->new address parents free) fields))))

  (define id-mutex (make-mutex))
  (define last-id 0)
  (define (next-id!)
    (with-mutex id-mutex
      (set! last-id (+ last-id 1))
      last-id))

  (define (handle-open? h) (not (eqv? (handle-address h) 0)))

  (define (acquire! h exclusive?)
    (with-mutex (handle-mutex h)
      (let wait ()
        (cond
          [(eqv? (handle-address h) 0) #f]
          [(or (handle-exclusive? h) (and exclusive? (fx> (handle-users h) 0)))
           (condition-wait (handle-condition h) (handle-mutex h))
           (wait)]
          [exclusive? (handle-exclusive?-set! h #t) #t]
          [else (handle-users-set! h (fx+ (handle-users h) 1)) #t]))))

  (define (release! h exclusive?)
    (with-mutex (handle-mutex h)
      (if exclusive?
          (handle-exclusive?-set! h #f)
          (handle-users-set! h (fx- (handle-users h) 1)))
      (condition-broadcast (handle-condition h))))

  ;; A handle named twice is taken once, exclusive if either use is: a thread
  ;; waiting for its own exclusive hold would wait forever.
  (define (merge-uses sorted)
    (fold-right
      (lambda (use merged)
        (if (and (pair? merged) (eq? (car use) (caar merged)))
            (cons (cons (car use) (or (cdr use) (cdar merged))) (cdr merged))
            (cons use merged)))
      '()
      sorted))

  (define (release-uses! uses)
    (for-each (lambda (use) (release! (car use) (cdr use))) uses))

  ;; Closing a parent closes its children, whose ids are larger, so the last
  ;; closed handle is the child the caller passed, not the parent that closed it.
  (define (last-closed uses)
    (fold-left (lambda (closed use) (if (handle-open? (car use)) closed (car use))) #f uses))

  (define (call-with-handles who uses thunk)
    (let ([uses (merge-uses
                  (sort (lambda (a b) (< (handle-id (car a)) (handle-id (car b)))) uses))])
      (dynamic-wind
        (lambda ()
          (let loop ([pending uses] [held '()])
            (unless (null? pending)
              (let ([use (car pending)])
                (unless (guard (c [#t (release-uses! held) (raise c)])
                          (acquire! (car use) (cdr use)))
                  (release-uses! held)
                  (assertion-violationf who "~s is closed" (last-closed uses)))
                (loop (cdr pending) (cons use held))))))
        thunk
        (lambda () (release-uses! uses)))))

  (define-syntax with-handles
    (syntax-rules ()
      [(_ who ([h exclusive?] ...) e1 e2 ...)
       (call-with-handles who (list (cons h exclusive?) ...) (lambda () e1 e2 ...))]))

  (define guardian (make-guardian))
  (define guardian-mutex (make-mutex))
  (define live-count 0)

  (define (adjust-live! n)
    (with-mutex guardian-mutex (set! live-count (+ live-count n))))

  (define (live-handle-count) (with-mutex guardian-mutex live-count))

  (define (add-child! p h)
    (with-mutex (handle-mutex p)
      (handle-children-set! p (weak-cons h (handle-children p)))))

  (define (remove-child! p h)
    (with-mutex (handle-mutex p)
      (handle-children-set! p
        (let loop ([children (handle-children p)])
          (cond
            [(null? children) '()]
            [(eq? (car children) h) (cdr children)]
            [else (weak-cons (car children) (loop (cdr children)))])))))

  (define (handle-borrowed? h)
    (with-mutex (handle-mutex h) (pair? (handle-children h))))

  ;; Called with every parent held, so none can close before it lists the
  ;; child.
  (define (register-handle! h)
    (free-unreachable-handles!)
    (for-each (lambda (p) (add-child! p h)) (handle-parents h))
    (with-mutex guardian-mutex
      (set! live-count (+ live-count 1))
      (guardian h))
    h)

  (define (release-native! h)
    ((handle-free h) (handle-address h))
    (handle-address-set! h 0)
    (adjust-live! -1)
    (for-each (lambda (p) (remove-child! p h)) (handle-parents h)))

  (define (close-handle! h)
    (when (with-mutex (handle-mutex h)
            (let wait ()
              (cond
                [(eqv? (handle-address h) 0) #f]
                [(or (handle-exclusive? h) (fx> (handle-users h) 0))
                 (condition-wait (handle-condition h) (handle-mutex h))
                 (wait)]
                [else (handle-exclusive?-set! h #t) #t])))
      (for-each close-handle! (with-mutex (handle-mutex h) (map values (handle-children h))))
      (release-native! h)
      (release! h #t)))

  ;; A collection can find a child and its parents unreachable at once; freeing
  ;; in decreasing id order frees each child before what it borrows. Closing a
  ;; parent can reach a handle found here through its weak pair, so a handle
  ;; that is held or still has children is left to that close and offered
  ;; again after a later collection, rather than waited for: the thread may
  ;; hold handles that the close needs.
  (define (free-unreachable-handles!)
    (let ([found (with-mutex guardian-mutex
                   (let loop ([found '()])
                     (let ([h (guardian)])
                       (if h (loop (cons h found)) found))))])
      (for-each
        (lambda (h)
          (when (with-mutex (handle-mutex h)
                  (cond
                    [(eqv? (handle-address h) 0) #f]
                    [(or (handle-exclusive? h) (fx> (handle-users h) 0)
                         (pair? (handle-children h)))
                     (with-mutex guardian-mutex (guardian h))
                     #f]
                    [else (handle-exclusive?-set! h #t) #t]))
            (release-native! h)
            (release! h #t)))
        (sort (lambda (a b) (> (handle-id a) (handle-id b))) found))))

  (record-writer (record-type-descriptor handle)
    (lambda (h port write)
      (fprintf port "#<~a~a>" (record-type-name (record-rtd h))
        (if (handle-open? h) "" " closed")))))
