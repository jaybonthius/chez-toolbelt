(library (parakeet)
  (export
    load-model model? model-kind model-open? close-model model-set-concurrency!
    bundle-components-json abi-version &parakeet-error parakeet-error?

    decode-options decoder word-filter vad-options vad-mode asr-stream-options
    sound-options speaker-options scene-options diarization-latency

    transcribe-file transcribe-pcm transcribe-file-json transcribe-file-vad-json
    transcribe-pcm-batch transcribe-pcm-batch-json
    transcribe-file-nbest-json transcribe-pcm-nbest-json
    transcribe-pcm-logits ctc-logits? ctc-logits-frames ctc-logits-vocabulary-size
    ctc-logits-values

    compile-grammar grammar? close-grammar
    transcribe-pcm-grammar transcribe-file-grammar
    transcribe-pcm-grammar-json transcribe-file-grammar-json
    open-grammar-stream grammar-stream? close-grammar-stream
    grammar-stream-feed-json grammar-stream-finish-json grammar-stream-reset!

    model-stream-kind model-stream-eou-events? model-stream-presets-json
    open-asr-stream asr-stream? close-asr-stream
    asr-stream-feed asr-stream-feed-json asr-stream-finish asr-stream-finish-json
    asr-stream-drain-events asr-stream-reset! asr-stream-info-json
    stream-event? stream-event-kind stream-event-token stream-event-frame stream-event-time

    vad-pcm-json vad-file-json
    open-vad-stream vad-stream? close-vad-stream
    vad-stream-feed-json vad-stream-finish-json vad-stream-reset!

    model-speaker-dimension model-speaker-family model-speaker-identity model-speaker-warning
    embed-speaker make-speaker-embedding speaker-embedding? speaker-embedding-vector
    speaker-embedding-family speaker-embedding-weights
    make-speaker-registry load-speaker-registry speaker-registry? close-speaker-registry
    save-speaker-registry speaker-registry-size speaker-registry-family
    speaker-registry-weights speaker-registry-strict-set!
    enroll-speaker! speaker-registry-add! identify-speaker-json

    diarize-file-json diarize-pcm-json diarize-named-pcm-json diarize-profiles-pcm-json
    transcribe-and-diarize transcribe-and-diarize-json transcribe-and-diarize-named-json
    attributed-utterance? attributed-utterance-speaker attributed-utterance-text
    attributed-utterance-start attributed-utterance-end attributed-utterance-confidence
    open-diarization-stream diarization-stream? close-diarization-stream
    diarization-stream-feed diarization-stream-finish diarization-stream-active
    diarization-stream-time diarization-stream-chunk-samples
    diarization-segment? diarization-segment-speaker diarization-segment-start
    diarization-segment-end
    open-attribution-stream attribution-stream? close-attribution-stream
    attribution-stream-feed attribution-stream-finish

    model-sound-labels open-sound-stream sound-stream? close-sound-stream
    sound-stream-feed sound-stream-finish sound-stream-active sound-stream-drain-scores-json
    sound-segment? sound-segment-class sound-segment-label sound-segment-start
    sound-segment-end sound-segment-peak

    open-scene-stream scene-stream? close-scene-stream
    scene-stream-feed-json scene-stream-finish-json scene-stream-drain-scores-json)
  (import (chezscheme) (parakeet native))

  (include "parakeet/options.ss")
  (include "parakeet/models.ss")
  (include "parakeet/grammar.ss")
  (include "parakeet/streams.ss")
  (include "parakeet/speakers.ss")
  (include "parakeet/analysis.ss"))
