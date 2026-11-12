#define _POSIX_C_SOURCE 199309L
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include "parakeet_capi.h"

static double now_ms(void)
{
    struct timespec t;
    clock_gettime(CLOCK_MONOTONIC, &t);
    return t.tv_sec * 1e3 + t.tv_nsec / 1e6;
}

static void fail(const char *what, const char *detail)
{
    fprintf(stderr, "direct: %s: %s\n", what, detail ? detail : "");
    exit(1);
}

static uint32_t le32(const unsigned char *p) { return p[0] | p[1] << 8 | p[2] << 16 | (uint32_t)p[3] << 24; }

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
    float *samples = malloc(sizeof(float) * n);
    for (int i = 0; i < n; i++) samples[i] = (int16_t)(bytes[at + 8 + 2 * i] | bytes[at + 9 + 2 * i] << 8) / 32768.0f;
    free(bytes);
    *count = n;
    return samples;
}

/* The same measurements as the Scheme benchmark, called from C, so the two
   differ only by the binding. */
int main(int argc, char **argv)
{
    if (argc != 4) fail("usage", "direct OFFLINE-MODEL STREAMING-MODEL WAV");
    int n;
    float *pcm = read_wav(argv[3], &n);

    parakeet_ctx *ctx = parakeet_capi_load(argv[1]);
    if (!ctx) fail("load", parakeet_capi_load_error());
    parakeet_capi_free_string(parakeet_capi_transcribe_pcm(ctx, pcm, n, 16000, 0));
    double best = 1e30, total = 0;
    for (int i = 0; i < 10; i++) {
        double start = now_ms();
        char *text = parakeet_capi_transcribe_pcm(ctx, pcm, n, 16000, 0);
        double elapsed = now_ms() - start;
        if (!text) fail("transcribe", parakeet_capi_last_error(ctx));
        parakeet_capi_free_string(text);
        total += elapsed;
        if (elapsed < best) best = elapsed;
    }
    printf("C       transcribe %.2f s of speech: best %.1f ms, mean %.1f ms\n", n / 16000.0, best, total / 10);
    parakeet_capi_free(ctx);

    ctx = parakeet_capi_load(argv[2]);
    if (!ctx) fail("load", parakeet_capi_load_error());
    best = 1e30;
    for (int round = 0; round < 3; round++) {
        parakeet_stream *s = parakeet_capi_stream_begin(ctx);
        double start = now_ms();
        int calls = 0;
        for (int at = 0; at < n; at += 1600, calls++) {
            char *delta = parakeet_capi_stream_feed(s, pcm + at, n - at < 1600 ? n - at : 1600, NULL);
            if (!delta) fail("feed", parakeet_capi_last_error(ctx));
            parakeet_capi_free_string(delta);
        }
        parakeet_capi_free_string(parakeet_capi_stream_finalize(s));
        double per_call = (now_ms() - start) / (calls + 1);
        if (per_call < best) best = per_call;
        parakeet_capi_stream_free(s);
    }
    printf("C       stream 100 ms pieces: best %.2f ms per piece\n", best);

    parakeet_stream *s = parakeet_capi_stream_begin(ctx);
    double start = now_ms();
    for (int i = 0; i < 100000; i++) {
        char *delta = parakeet_capi_stream_feed(s, pcm, 0, NULL);
        parakeet_capi_free_string(delta);
    }
    printf("C       empty stream feed: %.2f us per call\n", (now_ms() - start) / 100);
    parakeet_capi_stream_free(s);
    parakeet_capi_free(ctx);
    free(pcm);
    return 0;
}
