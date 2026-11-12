SCHEME ?= scheme
CC ?= cc
CAP ?=
OBJ ?= build/obj
LIBDIRS := src::$(OBJ)/src:tests/lib::$(OBJ)/tests/lib
RUN := $(SCHEME) --libdirs $(LIBDIRS) --program
TEST_ENV ?=
BUILD_TIMEOUT ?= 1800
TEST_TIMEOUT ?= 600
BENCH_TIMEOUT ?= 1800
TESTS := $(sort $(wildcard tests/*-test.sps))

MD4C := vendor/md4c/src
SHARED_OBJECT := $(OBJ)/src/md4c/libchez-md4c.so
C_SOURCES := src/md4c/events.c $(MD4C)/md4c.c $(MD4C)/entity.c
CFLAGS ?= -O2
# Hidden symbols keep md4c's own md_parse bound inside the shared object,
# whatever other md4c the process loads.
SHARED_CFLAGS := -std=gnu99 -fPIC -shared -fvisibility=hidden -Wall -I$(MD4C)
SANITIZE_CFLAGS := -O1 -g -fno-omit-frame-pointer -fsanitize=address,undefined \
  -fno-sanitize-recover=all
# Chez itself is not built with the sanitizers, so their runtimes are
# preloaded; Chez frees its heap only at exit, so leaks are not reported.
SANITIZE_ENV := ASAN_OPTIONS=detect_leaks=0 \
  LD_PRELOAD=$(shell $(CC) -print-file-name=libasan.so):$(shell $(CC) -print-file-name=libubsan.so)

.PHONY: build test sanitize bench clean $(TESTS:%=run/%)

# Phony so that Chez, which tracks what each object was built against,
# decides what is stale.
build: $(SHARED_OBJECT)
	$(CAP) timeout $(BUILD_TIMEOUT) $(SCHEME) -q --script build.ss $(OBJ)

$(SHARED_OBJECT): $(C_SOURCES) $(MD4C)/md4c.h $(MD4C)/entity.h
	@mkdir -p $(@D)
	$(CAP) timeout $(BUILD_TIMEOUT) $(CC) $(CFLAGS) $(SHARED_CFLAGS) -o $@ $(C_SOURCES)

# One process per test file, so a crash or leaked global state stays inside
# its file.
test: $(TESTS:%=run/%)

$(TESTS:%=run/%): run/%: build
	@$(CAP) timeout $(TEST_TIMEOUT) env $(TEST_ENV) $(RUN) ./$(OBJ)/$(*:.sps=.so)

# A build of its own, so that the ordinary one is never left sanitized.
sanitize:
	@$(MAKE) --no-print-directory test OBJ=build/sanitize CFLAGS="$(SANITIZE_CFLAGS)" \
	  TEST_ENV="$(SANITIZE_ENV)"

bench: build
	$(CAP) timeout $(BENCH_TIMEOUT) $(RUN) ./$(OBJ)/bench/bench.so

clean:
	rm -rf build
