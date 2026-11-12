SCHEME ?= scheme
CAP ?=
OBJ := build/obj
LIBDIRS := src::$(OBJ)/src:tests/lib::$(OBJ)/tests/lib
RUN := $(SCHEME) --libdirs $(LIBDIRS) --script tests/run.ss
BUILD_TIMEOUT ?= 1800
TEST_TIMEOUT ?= 600
BENCH_TIMEOUT ?= 1800
TESTS := $(sort $(wildcard tests/*-test.sps))

DATA := build/data
DATA_FILES := canada.json citm_catalog.json twitter.json
DATA_URL := https://raw.githubusercontent.com/miloyip/nativejson-benchmark/478d5727c2a4048e835a29c65adecc7d795360d5/data

.PHONY: build test bench data clean $(TESTS:%=run/%)

# Phony so that Chez, which tracks what each object was built against,
# decides what is stale.
build:
	$(CAP) timeout $(BUILD_TIMEOUT) $(SCHEME) -q --script build.ss

# One process per test file, so a crash or leaked global state stays inside
# its file. A test that starts its own Scheme reads SCHEME.
test: $(TESTS:%=run/%)

$(TESTS:%=run/%): run/%: build
	@SCHEME=$(SCHEME) $(CAP) timeout $(TEST_TIMEOUT) $(RUN) ./$(OBJ)/$(*:.sps=.so)

data: $(DATA_FILES:%=$(DATA)/%)

$(DATA)/%.json: bench/data.sha256
	@mkdir -p $(DATA)
	curl -sSfL -o $@.part $(DATA_URL)/$*.json
	@echo "$$(grep ' $*.json$$' bench/data.sha256 | cut -d' ' -f1)  $@.part" | sha256sum -c --quiet
	@mv $@.part $@

bench: build data
	$(CAP) timeout $(BENCH_TIMEOUT) $(RUN) ./$(OBJ)/bench/bench.so $(DATA) bench/signals.json

clean:
	rm -rf build
