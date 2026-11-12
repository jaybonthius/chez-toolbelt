#include <cstdlib>
#include <cstring>
#include <mutex>
#include "parakeet_capi.h"

// parakeet_capi_last_error copies the message into a buffer of the context
// and returns that buffer, which its next call replaces: two threads reading
// one context's error at once would copy from a freed buffer, so reads are
// serialised and copied here. The copy can throw (allocation, locking), and
// an exception must not unwind into Scheme's frames; NULL then stands for an
// unavailable message.
extern "C" char *chez_parakeet_last_error(parakeet_ctx *ctx)
{
    static std::mutex mutex;
    try {
        std::lock_guard<std::mutex> lock(mutex);
        return strdup(parakeet_capi_last_error(ctx));
    } catch (...) {
        return nullptr;
    }
}
