#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "parakeet_capi.h"

static void fail(const char *what, const char *detail)
{
    fprintf(stderr, "reference: %s: %s\n", what, detail ? detail : "");
    exit(1);
}

static uint32_t le32(const unsigned char *p) { return p[0] | p[1] << 8 | p[2] << 16 | (uint32_t)p[3] << 24; }

/* 16-bit mono WAV only: the fixtures' format. */
static float *read_wav(const char *path, int *count)
{
    FILE *f = fopen(path, "rb");
    if (!f) fail("open", path);
    fseek(f, 0, SEEK_END);
    long size = ftell(f);
    fseek(f, 0, SEEK_SET);
    unsigned char *bytes = malloc(size);
    if (!bytes || fread(bytes, 1, size, f) != (size_t)size) fail("read", path);
    fclose(f);
    long at = 12;
    while (at + 8 <= size && memcmp(bytes + at, "data", 4) != 0) at += 8 + le32(bytes + at + 4);
    if (at + 8 > size) fail("no data chunk", path);
    int n = (int)(le32(bytes + at + 4) / 2);
    float *samples = malloc(sizeof(float) * (n ? n : 1));
    for (int i = 0; i < n; i++) {
        int16_t v = (int16_t)(bytes[at + 8 + 2 * i] | bytes[at + 9 + 2 * i] << 8);
        samples[i] = v / 32768.0f;
    }
    free(bytes);
    *count = n;
    return samples;
}

static char *dir;

static FILE *output(const char *name)
{
    char path[4096];
    snprintf(path, sizeof path, "%s/%s", dir, name);
    FILE *f = fopen(path, "wb");
    if (!f) fail("create", path);
    return f;
}

static void write_text(const char *name, const char *text)
{
    FILE *f = output(name);
    fputs(text, f);
    fclose(f);
}

static void write_owned(const char *name, char *text, parakeet_ctx *ctx)
{
    if (!text) fail(name, parakeet_capi_last_error(ctx));
    write_text(name, text);
    parakeet_capi_free_string(text);
}

static parakeet_ctx *load(const char *path, const char *component)
{
    parakeet_ctx *ctx = component ? parakeet_capi_load_component(path, component) : parakeet_capi_load(path);
    if (!ctx) fail("load", parakeet_capi_load_error());
    return ctx;
}

static void asr(const char *model, const char *wav)
{
    parakeet_ctx *ctx = load(model, NULL);
    int n;
    float *pcm = read_wav(wav, &n);
    write_owned("transcript-default.txt", parakeet_capi_transcribe_path_lang(ctx, wav, 0, NULL), ctx);
    write_owned("transcript-ctc.txt", parakeet_capi_transcribe_path_lang(ctx, wav, 1, NULL), ctx);
    write_owned("pcm-default.txt", parakeet_capi_transcribe_pcm_lang(ctx, pcm, n, 16000, 0, NULL), ctx);
    write_owned("json.txt", parakeet_capi_transcribe_path_json(ctx, wav, 0), ctx);
    write_owned("nbest.txt", parakeet_capi_transcribe_pcm_nbest_json(ctx, pcm, n, 16000, 4, 2, 1, NULL), ctx);

    const float *clips[2] = {pcm, pcm + n / 2};
    int counts[2] = {n / 2, n - n / 2};
    char *texts[2];
    if (parakeet_capi_transcribe_pcm_batch_lang(ctx, clips, counts, 2, 16000, 1, NULL, texts) != 0)
        fail("batch", parakeet_capi_last_error(ctx));
    FILE *f = output("batch.txt");
    fprintf(f, "%s\n%s", texts[0], texts[1]);
    fclose(f);
    parakeet_capi_free_string(texts[0]);
    parakeet_capi_free_string(texts[1]);
    write_owned("batch-json.txt", parakeet_capi_transcribe_pcm_batch_json_lang(ctx, pcm, counts, 2, 16000, 0, NULL), ctx);

    float *logits;
    int frames, vocabulary;
    if (parakeet_capi_transcribe_pcm_logits(ctx, pcm, n, 16000, &logits, &frames, &vocabulary) != 0)
        fail("logits", parakeet_capi_last_error(ctx));
    f = output("logits.bin");
    fwrite(logits, sizeof(float), (size_t)frames * vocabulary, f);
    fclose(f);
    parakeet_capi_free_logits(logits);
    f = output("logits-shape.txt");
    fprintf(f, "%d %d", frames, vocabulary);
    fclose(f);

    parakeet_grammar *grammar = parakeet_capi_grammar_create(ctx, "root ::= \"well\" | \"Well\"\n");
    if (!grammar) fail("grammar", parakeet_capi_last_error(ctx));
    char *json;
    int status = parakeet_capi_transcribe_pcm_grammar_json(ctx, grammar, pcm, n, 16000, 1, 4, NULL, &json);
    if (status != PARAKEET_GRAMMAR_OK) fail("grammar json", parakeet_capi_last_error(ctx));
    write_owned("grammar.txt", json, ctx);
    parakeet_capi_grammar_free(grammar);
    free(pcm);
    parakeet_capi_free(ctx);
}

/* Feeds exactly as the streaming tests do, so their results compare: the
   speech fixture, three seconds of silence, 1600 samples at a time. */
static void stream(const char *model, const char *wav)
{
    parakeet_ctx *ctx = load(model, NULL);
    int n;
    float *speech = read_wav(wav, &n);
    int total = n + 48000;
    float *pcm = calloc(total, sizeof(float));
    memcpy(pcm, speech, sizeof(float) * n);
    free(speech);

    parakeet_stream *s = parakeet_capi_stream_begin(ctx);
    if (!s) fail("stream", parakeet_capi_last_error(ctx));
    FILE *text = output("stream-text.txt");
    FILE *events = output("stream-events.bin");
    for (int at = 0; at < total + 1600; at += 1600) {
        char *delta = at < total ? parakeet_capi_stream_feed(s, pcm + at, total - at < 1600 ? total - at : 1600, NULL)
                                 : parakeet_capi_stream_finalize(s);
        if (!delta) fail("feed", parakeet_capi_last_error(ctx));
        fputs(delta, text);
        parakeet_capi_free_string(delta);
        parakeet_stream_event *drained;
        int count = parakeet_capi_stream_drain_events(s, &drained);
        if (count < 0) fail("drain", parakeet_capi_last_error(ctx));
        fwrite(drained, sizeof *drained, count, events);
        parakeet_capi_free_events(drained);
    }
    fclose(text);
    fclose(events);
    parakeet_capi_stream_free(s);

    s = parakeet_capi_stream_begin(ctx);
    FILE *json = output("stream-json.txt");
    for (int at = 0; at < total + 1600; at += 1600) {
        char *document = at < total ? parakeet_capi_stream_feed_json(s, pcm + at, total - at < 1600 ? total - at : 1600)
                                    : parakeet_capi_stream_finalize_json(s);
        if (!document) fail("feed json", parakeet_capi_last_error(ctx));
        fprintf(json, "%s\n", document);
        parakeet_capi_free_string(document);
    }
    fclose(json);
    parakeet_capi_stream_free(s);
    free(pcm);
    parakeet_capi_free(ctx);
}

static void speaker(const char *model, const char *wav)
{
    parakeet_ctx *ctx = load(model, NULL);
    int n, dimension;
    float *pcm = read_wav(wav, &n);
    float *embedding;
    if (parakeet_capi_speaker_embed_pcm(ctx, pcm, n, 16000, &embedding, &dimension) != 0)
        fail("embed", parakeet_capi_last_error(ctx));
    FILE *f = output("embedding.bin");
    fwrite(embedding, sizeof(float), dimension, f);
    fclose(f);
    parakeet_capi_free_floats(embedding);
    free(pcm);
    parakeet_capi_free(ctx);
}

static void bundle(const char *path, const char *speech, const char *two_speakers)
{
    int n;
    parakeet_ctx *vad = load(path, "vad");
    float *pcm = read_wav(speech, &n);
    write_owned("vad.txt", parakeet_capi_vad_pcm_json(vad, pcm, n, 16000, "{\"probabilities\":true}"), vad);
    free(pcm);
    parakeet_capi_free(vad);

    parakeet_ctx *diar = load(path, "diar");
    pcm = read_wav(two_speakers, &n);
    write_owned("diarize.txt", parakeet_capi_diarize_pcm(diar, pcm, n, 16000), diar);
    free(pcm);
    parakeet_capi_free(diar);
}

int main(int argc, char **argv)
{
    if (argc < 4) fail("usage", "reference asr|stream|speaker|bundle OUTDIR MODEL WAV [WAV]");
    dir = argv[2];
    if (strcmp(argv[1], "asr") == 0) asr(argv[3], argv[4]);
    else if (strcmp(argv[1], "stream") == 0) stream(argv[3], argv[4]);
    else if (strcmp(argv[1], "speaker") == 0) speaker(argv[3], argv[4]);
    else if (strcmp(argv[1], "bundle") == 0 && argc == 6) bundle(argv[3], argv[4], argv[5]);
    else fail("unknown command", argv[1]);
    return 0;
}
