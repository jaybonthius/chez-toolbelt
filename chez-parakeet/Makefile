SCHEME ?= scheme
CAP ?=
CACHE_HOME ?= $(or $(XDG_CACHE_HOME),$(HOME)/.cache)
PARAKEET_MODELS ?= $(CACHE_HOME)/chez-parakeet/models
OBJ := build/obj
GEN := build/gen
LIBDIRS := src::$(OBJ)/src:$(GEN)::$(OBJ)/$(GEN):tests/lib::$(OBJ)/tests/lib
RUN := $(SCHEME) --libdirs $(LIBDIRS) --script tests/run.ss
BUILD_TIMEOUT ?= 3600
TEST_TIMEOUT ?= 900
TESTS := $(sort $(wildcard tests/*-test.sps))

SOURCE := build/source
FIXTURES := vendor/parakeet.cpp/tests/fixtures
NATIVE := build/native
LIB := build/lib
LIBPARAKEET := $(LIB)/libparakeet.so
SHIM := $(LIB)/libchez-parakeet.so
# Upstream's defaults otherwise, including GGML_NATIVE: the library is built
# for the machine that builds it.
CMAKE_OPTIONS := -DCMAKE_BUILD_TYPE=Release -DPARAKEET_SHARED=ON \
  -DPARAKEET_BUILD_CLI=OFF -DPARAKEET_BUILD_SERVER=OFF -DPARAKEET_BUILD_TESTS=OFF \
  -DPARAKEET_WITH_CED=ON -DPARAKEET_WITH_VOICEDETECT=ON \
  -DCMAKE_BUILD_WITH_INSTALL_RPATH=ON '-DCMAKE_INSTALL_RPATH=$$ORIGIN'

export PARAKEET_MODELS

.PHONY: build native models check-models test test-asan bench clean $(TESTS:%=run/%) run/consumer

# Phony so that Chez, which tracks what each object was built against,
# decides what is stale.
build: native
	$(CAP) timeout $(BUILD_TIMEOUT) $(SCHEME) -q --script build.ss

# cmake decides what is stale in parakeet.cpp. Its make would otherwise join
# this make's job server and warn.
native:
	@$(CAP) timeout $(BUILD_TIMEOUT) scripts/export-source vendor/parakeet.cpp $(SOURCE)
	@mkdir -p $(NATIVE) $(LIB) $(GEN)/parakeet
	@test -f $(NATIVE)/CMakeCache.txt || \
	  $(CAP) timeout $(BUILD_TIMEOUT) cmake -S $(SOURCE) -B $(NATIVE) $(CMAKE_OPTIONS) \
	    > build/configure.log 2>&1 || { cat build/configure.log; exit 1; }
	@env -u MAKEFLAGS -u MAKELEVEL -u MFLAGS $(CAP) timeout $(BUILD_TIMEOUT) \
	  cmake --build $(NATIVE) --target parakeet --parallel $(shell nproc) > build/native.log 2>&1 || \
	  { tail -50 build/native.log; exit 1; }
	@find $(NATIVE) -name '*.so*' -exec cp -P -u {} $(LIB)/ \;
	@$(MAKE) -s --no-print-directory $(SHIM) $(GEN)/parakeet/layout.sls build/tests/reference build/bench/direct

$(SHIM): src/parakeet/shim.cpp $(LIBPARAKEET)
	c++ -std=c++17 -O2 -Wall -Wextra -Werror -shared -fPIC -I$(SOURCE)/include -o $@ $< \
	  -L$(LIB) -lparakeet '-Wl,-rpath,$$ORIGIN'

$(GEN)/parakeet/layout.sls: src/parakeet/layout.c $(SOURCE)/include/parakeet_capi.h $(SHIM)
	cc -std=c11 -Wall -Wextra -Werror -I$(SOURCE)/include \
	  -DLIBPARAKEET_PATH='"$(abspath $(LIBPARAKEET))"' -DSHIM_PATH='"$(abspath $(SHIM))"' \
	  -o build/layout $<
	build/layout > $@.new && mv $@.new $@

build/tests/reference: tests/reference.c $(LIBPARAKEET)
	@mkdir -p build/tests
	cc -std=c11 -O2 -Wall -Wextra -Werror -I$(SOURCE)/include -o $@ $< \
	  -L$(LIB) -lparakeet -Wl,-rpath,$(abspath $(LIB))

build/bench/direct: bench/direct.c $(LIBPARAKEET)
	@mkdir -p build/bench
	cc -std=c11 -O2 -Wall -Wextra -Werror -I$(SOURCE)/include -o $@ $< \
	  -L$(LIB) -lparakeet -Wl,-rpath,$(abspath $(LIB))

models:
	scripts/fetch-models $(PARAKEET_MODELS)

check-models:
	@scripts/fetch-models $(PARAKEET_MODELS) --check

REFERENCE := build/reference

$(REFERENCE)/done: build/tests/reference | check-models
	@mkdir -p $(REFERENCE)
	$(CAP) timeout $(TEST_TIMEOUT) build/tests/reference asr $(REFERENCE) \
	  $(PARAKEET_MODELS)/tdt_ctc-110m-q8_0.gguf $(FIXTURES)/speech.wav
	$(CAP) timeout $(TEST_TIMEOUT) build/tests/reference stream $(REFERENCE) \
	  $(PARAKEET_MODELS)/realtime_eou_120m-v1-q8_0.gguf $(FIXTURES)/speech.wav
	$(CAP) timeout $(TEST_TIMEOUT) build/tests/reference speaker $(REFERENCE) \
	  $(PARAKEET_MODELS)/wespeaker-resnet34-voxceleb.gguf $(FIXTURES)/speech.wav
	$(CAP) timeout $(TEST_TIMEOUT) build/tests/reference bundle $(REFERENCE) \
	  $(PARAKEET_MODELS)/parakeet-bundle-small.gguf $(FIXTURES)/speech.wav $(FIXTURES)/two_speakers.wav
	@touch $@

# One process per test file, so a crash or leaked global state stays inside
# its file.
test: $(TESTS:%=run/%) run/consumer

$(TESTS:%=run/%): run/%: build check-models $(REFERENCE)/done
	@$(CAP) timeout $(TEST_TIMEOUT) $(RUN) ./$(OBJ)/$(*:.sps=.so)

# Run from / with only library directories, as a program using the library
# would be, so that nothing resolves against the checkout.
run/consumer: build
	@cd / && $(CAP) timeout $(TEST_TIMEOUT) $(SCHEME) \
	  --libdirs $(abspath src)::$(abspath $(OBJ)/src):$(abspath $(GEN))::$(abspath $(OBJ)/$(GEN)) \
	  --program $(abspath $(OBJ)/tests/consumer.so)

# The tests again with AddressSanitizer's allocator and its checked memcpy
# and strlen, which the binding copies native results with: a free of the
# wrong pointer, a double free or a copy past a native buffer stops the run.
# libstdc++ is preloaded with it, as the sanitizer needs it to catch C++
# exceptions in a program that is not C++.
ASAN_PRELOAD = $(shell cc -print-file-name=libasan.so) $(shell c++ -print-file-name=libstdc++.so.6)

test-asan: build check-models $(REFERENCE)/done
	@for test in $(TESTS:.sps=.so); do \
	  LD_PRELOAD="$(ASAN_PRELOAD)" ASAN_OPTIONS=detect_leaks=0:halt_on_error=1 \
	    $(CAP) timeout $(TEST_TIMEOUT) $(RUN) ./$(OBJ)/$$test || exit 1; \
	done

bench: build check-models
	@$(CAP) timeout $(TEST_TIMEOUT) build/bench/direct $(PARAKEET_MODELS)/tdt_ctc-110m-q8_0.gguf \
	  $(PARAKEET_MODELS)/realtime_eou_120m-v1-q8_0.gguf $(FIXTURES)/speech.wav 2> /dev/null
	@$(CAP) timeout $(TEST_TIMEOUT) $(RUN) ./$(OBJ)/bench/bench.so 2> /dev/null

clean:
	rm -rf build
