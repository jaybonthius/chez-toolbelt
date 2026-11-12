#include <stddef.h>
#include <stdio.h>
#include "parakeet_capi.h"

/* R6RS needs the export list written out, so the definitions are walked
   twice: once for the names, once for the values. */
static int defining;

static void integer(const char *name, long long value)
{
    if (defining)
        printf("\n  (define %s %lld)", name, value);
    else
        printf(" %s", name);
}

static void string(const char *name, const char *value)
{
    if (defining)
        printf("\n  (define %s \"%s\")", name, value);
    else
        printf(" %s", name);
}

#define SIZE(name, type) integer(name, (long long)sizeof(type))
#define CONSTANT(name, value) integer(name, (long long)(value))
/* The binding reads and writes each field with a fixed C type; the assertion
   stops the build when parakeet.cpp changes a field's width. */
#define FIELD(name, type, member, width)                                       \
    do {                                                                       \
        _Static_assert(sizeof(((type *)0)->member) == (width), name);          \
        integer(name, (long long)offsetof(type, member));                      \
    } while (0)

static void definitions(void)
{
    string("libparakeet-path", LIBPARAKEET_PATH);
    string("shim-path", SHIM_PATH);

    CONSTANT("event-eou", PARAKEET_EVENT_EOU);
    CONSTANT("event-eob", PARAKEET_EVENT_EOB);
    CONSTANT("stream-kind-none", PARAKEET_STREAM_KIND_NONE);
    CONSTANT("stream-kind-cache-aware", PARAKEET_STREAM_KIND_CACHE_AWARE);
    CONSTANT("stream-kind-buffered", PARAKEET_STREAM_KIND_BUFFERED);
    CONSTANT("stream-cap-eou-events", PARAKEET_STREAM_CAP_EOU_EVENTS);
    CONSTANT("stream-preset-default", PARAKEET_STREAM_PRESET_DEFAULT);
    CONSTANT("stream-preset-custom", PARAKEET_STREAM_PRESET_CUSTOM);
    CONSTANT("diar-latency-model", PARAKEET_DIAR_LATENCY_MODEL);
    CONSTANT("diar-latency-low", PARAKEET_DIAR_LATENCY_LOW);
    CONSTANT("diar-latency-very-low", PARAKEET_DIAR_LATENCY_VERY_LOW);
    CONSTANT("diar-latency-ultra-low", PARAKEET_DIAR_LATENCY_ULTRA_LOW);
    CONSTANT("model-kind-none", PARAKEET_MODEL_KIND_NONE);
    CONSTANT("model-kind-asr", PARAKEET_MODEL_KIND_ASR);
    CONSTANT("model-kind-diarization", PARAKEET_MODEL_KIND_DIARIZATION);
    CONSTANT("model-kind-sound", PARAKEET_MODEL_KIND_SOUND);
    CONSTANT("model-kind-speaker", PARAKEET_MODEL_KIND_SPEAKER);
    CONSTANT("model-kind-vad", PARAKEET_MODEL_KIND_VAD);
    CONSTANT("grammar-ok", PARAKEET_GRAMMAR_OK);
    CONSTANT("grammar-no-match", PARAKEET_GRAMMAR_NO_MATCH);
    CONSTANT("grammar-error", PARAKEET_GRAMMAR_ERROR);

    SIZE("stream-event-size", parakeet_stream_event);
    FIELD("stream-event-token-offset", parakeet_stream_event, token, 4);
    FIELD("stream-event-is-eob-offset", parakeet_stream_event, is_eob, 4);
    FIELD("stream-event-encoder-frame-offset", parakeet_stream_event, encoder_frame, 4);
    FIELD("stream-event-time-sec-offset", parakeet_stream_event, time_sec, 4);

    SIZE("stream-opts-size", parakeet_stream_opts);
    FIELD("stream-opts-size-field-offset", parakeet_stream_opts, size, 4);
    FIELD("stream-opts-target-lang-offset", parakeet_stream_opts, target_lang, sizeof(void *));
    FIELD("stream-opts-preset-offset", parakeet_stream_opts, preset, 4);
    FIELD("stream-opts-left-frames-offset", parakeet_stream_opts, left_frames, 4);
    FIELD("stream-opts-chunk-frames-offset", parakeet_stream_opts, chunk_frames, 4);
    FIELD("stream-opts-right-frames-offset", parakeet_stream_opts, right_frames, 4);
    FIELD("stream-opts-flags-offset", parakeet_stream_opts, flags, 4);

    SIZE("sas-result-size", parakeet_sas_result);
    FIELD("sas-result-speaker-offset", parakeet_sas_result, speaker, 4);
    FIELD("sas-result-text-offset", parakeet_sas_result, text, sizeof(void *));
    FIELD("sas-result-start-offset", parakeet_sas_result, start, 4);
    FIELD("sas-result-end-offset", parakeet_sas_result, end, 4);
    FIELD("sas-result-conf-offset", parakeet_sas_result, conf, 4);

    SIZE("diar-segment-size", parakeet_diar_segment);
    FIELD("diar-segment-speaker-offset", parakeet_diar_segment, speaker, 4);
    FIELD("diar-segment-start-offset", parakeet_diar_segment, start, 4);
    FIELD("diar-segment-end-offset", parakeet_diar_segment, end, 4);

    SIZE("sound-opts-size", parakeet_sound_opts);
    FIELD("sound-opts-size-field-offset", parakeet_sound_opts, size, 4);
    FIELD("sound-opts-window-sec-offset", parakeet_sound_opts, window_sec, 4);
    FIELD("sound-opts-hop-sec-offset", parakeet_sound_opts, hop_sec, 4);
    FIELD("sound-opts-on-threshold-offset", parakeet_sound_opts, on_threshold, 4);
    FIELD("sound-opts-off-threshold-offset", parakeet_sound_opts, off_threshold, 4);
    FIELD("sound-opts-min-duration-sec-offset", parakeet_sound_opts, min_duration_sec, 4);
    FIELD("sound-opts-top-k-offset", parakeet_sound_opts, top_k, 4);

    SIZE("sound-segment-size", parakeet_sound_segment);
    FIELD("sound-segment-class-index-offset", parakeet_sound_segment, class_index, 4);
    FIELD("sound-segment-label-offset", parakeet_sound_segment, label, sizeof(void *));
    FIELD("sound-segment-start-offset", parakeet_sound_segment, start, 4);
    FIELD("sound-segment-end-offset", parakeet_sound_segment, end, 4);
    FIELD("sound-segment-peak-offset", parakeet_sound_segment, peak, 4);

    SIZE("scene-opts-size", parakeet_scene_opts);
    FIELD("scene-opts-size-field-offset", parakeet_scene_opts, size, 4);
    FIELD("scene-opts-diar-latency-offset", parakeet_scene_opts, diar_latency, 4);
    FIELD("scene-opts-sound-offset", parakeet_scene_opts, sound, sizeof(parakeet_sound_opts));
    FIELD("scene-opts-flags-offset", parakeet_scene_opts, flags, 4);
    FIELD("scene-opts-speaker-accept-threshold-offset", parakeet_scene_opts, speaker_accept_threshold, 4);
    FIELD("scene-opts-speaker-margin-offset", parakeet_scene_opts, speaker_margin, 4);
    FIELD("scene-opts-speaker-min-voice-sec-offset", parakeet_scene_opts, speaker_min_voice_sec, 4);
    FIELD("scene-opts-speaker-refresh-sec-offset", parakeet_scene_opts, speaker_refresh_sec, 4);
    FIELD("scene-opts-speaker-max-voice-sec-offset", parakeet_scene_opts, speaker_max_voice_sec, 4);
}

int main(void)
{
    printf("(library (parakeet layout)\n  (export");
    definitions();
    printf(")\n  (import (chezscheme))");
    defining = 1;
    definitions();
    printf(")\n");
    return 0;
}
