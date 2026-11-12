SCHEME ?= scheme
ZIG ?= zig
CAP ?=
OBJ := build/obj
LIBDIRS := src::$(OBJ)/src:tests/lib::$(OBJ)/tests/lib
RUN := $(SCHEME) --libdirs $(LIBDIRS) --script tests/run.ss
BUILD_TIMEOUT ?= 1800
TEST_TIMEOUT ?= 300
TESTS := $(sort $(wildcard tests/*-test.sps))

GHOSTTY_SOURCE := vendor/ghostty
GHOSTTY_PREFIX := build/ghostty
GHOSTTY_LIBRARY := $(GHOSTTY_PREFIX)/lib/libghostty-vt.so.0
# A baseline CPU keeps the library runnable on any x86-64 machine; the
# caches live in build/ so the submodule's working tree stays clean.
ZIG_OPTIONS := -Demit-lib-vt=true -Doptimize=ReleaseFast -Dcpu=baseline

.PHONY: build ghostty test bench clean $(TESTS:%=run/%)

# Phony so that Chez, which tracks what each object was built against,
# decides what is stale.
build: ghostty build/config.ss
	$(CAP) timeout $(BUILD_TIMEOUT) $(SCHEME) -q --script build.ss

# Zig decides what is stale in libghostty-vt.
ghostty:
	@test "$$($(ZIG) version)" = 0.16.0 || { echo "need Zig 0.16.0" >&2; exit 1; }
	@mkdir -p build
	cd $(GHOSTTY_SOURCE) && env ZIG_GLOBAL_CACHE_DIR=$(abspath build/zig-global-cache) \
	  ZIG_LOCAL_CACHE_DIR=$(abspath build/zig-cache) \
	  $(CAP) timeout $(BUILD_TIMEOUT) $(ZIG) build $(ZIG_OPTIONS) --prefix $(abspath $(GHOSTTY_PREFIX))

build/config.ss:
	@mkdir -p build
	printf '(define libghostty-vt-path "%s")\n' "$(abspath $(GHOSTTY_LIBRARY))" > $@

# One process per test file, so a crash or leaked global state stays
# inside its file.
test: $(TESTS:%=run/%)

$(TESTS:%=run/%): run/%: build
	@$(CAP) timeout $(TEST_TIMEOUT) $(RUN) ./$(OBJ)/$(*:.sps=.so)

bench: build
	$(CAP) timeout $(TEST_TIMEOUT) $(RUN) ./$(OBJ)/bench/bench.so

clean:
	rm -rf build
