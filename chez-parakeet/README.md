# chez-parakeet

Speech transcription, streaming recognition, grammars, voice activity detection, speaker identification, diarization and sound events for threaded Chez Scheme 10.4.1, on parakeet.cpp.

```sh
git submodule update --init --recursive
make SCHEME=/path/to/chez-10.4.1/scheme build     # needs cc, c++, cmake, git
make models                                       # about 2.5 GB, into ~/.cache/chez-parakeet/models; needs curl, sha256sum
make SCHEME=/path/to/chez-10.4.1/scheme test
make SCHEME=/path/to/chez-10.4.1/scheme test-asan
make SCHEME=/path/to/chez-10.4.1/scheme bench
```

Upstream: `vendor/parakeet.cpp` is github.com/jaybonthius/parakeet.cpp at `adefc51` (branch `chez`): mudler/parakeet.cpp `2eac2e7` with grammar-constrained decoding, grammar scores, a sound-stream leak fix, and voice-detect.cpp pinned to a fork that frees packed convolution weights.
