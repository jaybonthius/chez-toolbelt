# chez-md4c

Markdown for threaded Chez Scheme 10.4.1: md4c parses CommonMark with GitHub's tables, task lists, strikethrough and autolinks into records with source offsets, safely for untrusted input.

```sh
git submodule update --init vendor/md4c
make SCHEME=/path/to/chez-10.4.1/scheme build
make SCHEME=/path/to/chez-10.4.1/scheme test
make SCHEME=/path/to/chez-10.4.1/scheme sanitize
make SCHEME=/path/to/chez-10.4.1/scheme bench
```

md4c (MIT) is the submodule `vendor/md4c`, unmodified, pinned to `f9e5eb232b95b737373ae706648cb75bd259ed1a`.
