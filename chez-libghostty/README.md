# chez-libghostty

A Chez Scheme library, `(ghostty)`, binding libghostty-vt, the terminal emulator core of Ghostty.

## Building and testing

Needs threaded Chez Scheme 10.4.1 and Zig 0.16.0; Ghostty is a git submodule, so clone with `--recursive`.

```sh
make SCHEME=/path/to/chez-10.4.1/scheme build
make SCHEME=/path/to/chez-10.4.1/scheme test
make SCHEME=/path/to/chez-10.4.1/scheme bench
make clean
```

## Upstream

`vendor/ghostty` is Ghostty at `b58ef1d678be2747c830bc1b1e6c0a086afecf9a`, unmodified.
