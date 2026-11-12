# chez-json

A JSON reader and writer for threaded 64-bit Chez Scheme 10.4.1, to the RFC 8259 grammar and strict by default. It reads from strings, bytevectors and ports, and writes to strings, bytevectors and ports; objects are association lists with string keys and arrays are vectors. Floats are read correctly rounded and written as the shortest text that reads back exactly.

```scheme
(import (chezscheme) (json))

(define signals (string->json "{\"count\": 3, \"tags\": [\"a\", \"b\"], \"ratio\": 0.25}"))
;; (("count" . 3) ("tags" . #("a" "b")) ("ratio" . 0.25))

(cdr (assoc "count" signals))                ; 3
(json->string (cons '(seen . #t) signals))
;; "{\"seen\":true,\"count\":3,\"tags\":[\"a\",\"b\"],\"ratio\":0.25}"

(guard (c [(json-error? c) (json-error-position c)])
  (string->json "{\"a\": 1, \"a\": 2}"))     ; 9, where the second key starts

(let ([port (open-string-input-port "1 [2] {\"three\": 3}")])
  (list (json-read port) (json-read port) (json-read port) (json-read port)))
;; (1 #(2) (("three" . 3)) #!eof)
```

## Building

The build needs threaded 64-bit Chez Scheme 10.4.1. JSONTestSuite is a git submodule, so clone with `--recursive`.

```sh
git clone --recursive <this repository>
make SCHEME=/path/to/chez-10.4.1/scheme build
make SCHEME=/path/to/chez-10.4.1/scheme test
make SCHEME=/path/to/chez-10.4.1/scheme bench
```

`CAP` prefixes every build, test and benchmark process, for example `CAP="systemd-run --user --scope -q -p MemoryMax=8G"`. `make bench` fetches nativejson-benchmark's canada.json, citm_catalog.json and twitter.json into `build/data` with `curl`, checking them against `bench/data.sha256`, and times reading and writing them and `bench/signals.json`.

Programs import `(json)` with `--libdirs /path/to/chez-json/src::/path/to/chez-json/build/obj/src`.

## API

| Form | What it does |
|---|---|
| `(json-read [port])` | reads one value from a textual or binary port (default: the current input port) and leaves the port just after it; the eof object when only whitespace is left. A binary port at position 0 may start with a byte order mark; a port without a position, such as a pipe, never skips one. After an error the port is left after the text the call took to be the value: its brackets and strings matched, or at the end of the input, or just after the bracket that went past `json-max-depth`. Reading a file through a binary port is faster than through a textual one, which decodes the UTF-8 first. |
| `(string->json string)` | reads exactly one value; whitespace around it is allowed |
| `(bytevector->json bytevector)` | the same for UTF-8 bytes; one leading byte order mark is skipped |
| `(json-write value [port])` | writes to a textual or binary port (default: the current output port); UTF-8 on binary ports |
| `(json->string value)`, `(json->bytevector value)` | the text, as a string or as UTF-8 bytes |
| `json-error?`, `json-error-position` | the `&json` condition, with who, a message that gives the position in words, and the offending values as irritants; the position is a character index for strings and textual ports, a byte index for bytevectors and binary ports, and `#f` for writing |
| `json-max-depth` | thread parameter, default 1000: deeper nesting is refused, by the reader and the writer |
| `json-max-number-digits` | thread parameter, default 4300: a number with more digits in its integer and fraction parts is refused |
| `json-allow-duplicate-keys?` | thread parameter, default `#f`: when true, an object with a repeated key keeps every pair, in document order |
| `json-escape-html?` | thread parameter, default `#f`: when true, the writer escapes `<`, `>`, `&`, U+2028 and U+2029 as `\uXXXX` |

Settings are read once per call. `json-read` counts positions from where the call began reading.

## Data mapping

| JSON | Scheme |
|---|---|
| object | association list with string keys, in document order: `(("name" . "Ada") ("age" . 36))`; `{}` is `()` |
| array | vector |
| string | string |
| number without fraction or exponent | exact integer |
| number with fraction or exponent | flonum, correctly rounded; `-0.0` stays `-0.0`, and `-0` is exact 0 |
| `true`, `false`, `null` | `#t`, `#f`, the symbol `null` |

Keys are immutable strings. Equal keys read in one call share one string while a small per-call cache holds it, except keys written with escapes, so repeated keys in large documents allocate nothing; keys are never interned as symbols, whose table untrusted input could flood.

The writer takes string or symbol keys, writes keys in the order given, and does not look for duplicates. It refuses, with a `&json` condition, infinities and NaNs, exact non-integers, lists that are not association lists with string or symbol keys, symbols other than `null`, nesting deeper than `json-max-depth` (so a vector that contains itself), an association list whose spine is circular, and every other object. A value that is refused leaves the port untouched. The writer does not apply `json-max-number-digits`: an integer with more digits is written, and reading it back needs the limit raised.

## Strictness

The reader refuses, with a `&json` condition and the position: anything outside RFC 8259's grammar (no comments, trailing commas, single quotes, `NaN`, leading zeros or other whitespace); invalid UTF-8 in bytes (overlong forms, encoded surrogates, truncated sequences, values above U+10FFFF); a `\u` escape that is half of a surrogate pair without the other half; unescaped control characters in strings; a key repeated in one object, unless `json-allow-duplicate-keys?` is true; a number that overflows to infinity or has more than `json-max-number-digits` digits (one that underflows reads as zero); nesting deeper than `json-max-depth`; and anything but whitespace after the value in `string->json` and `bytevector->json`.

Two JSONTestSuite files that RFC 8259 allows have a repeated key, so they are refused by default and read with `json-allow-duplicate-keys?`; every other `y_` file is read and written back, and every `n_` file is refused.
