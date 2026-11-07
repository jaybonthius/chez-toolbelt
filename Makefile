SCHEME ?= scheme
CAP ?=
H2O_DIR ?= ../chez-h2o
JSON_DIR ?= ../chez-json
HAML_DIR ?= ../chez-haml
OBJ := build/obj
DEPENDENCIES := $(H2O_DIR)/build/obj/src:$(JSON_DIR)/build/obj/src:$(HAML_DIR)/build/obj/src
LIBDIRS := src::$(OBJ)/src:tests/lib::$(OBJ)/tests/lib:$(DEPENDENCIES)
RUN := $(SCHEME) --libdirs $(LIBDIRS) --script tests/run.ss
BUILD_TIMEOUT ?= 1800
TEST_TIMEOUT ?= 600
TEST_PORT ?= 18600
TESTS := $(sort $(wildcard tests/*-test.sps))

.PHONY: deps build test conformance clean $(TESTS:%=run/%)

deps:
	$(MAKE) -C $(H2O_DIR) SCHEME=$(SCHEME) CAP='$(CAP)' build
	$(MAKE) -C $(JSON_DIR) SCHEME=$(SCHEME) CAP='$(CAP)' build
	$(MAKE) -C $(HAML_DIR) SCHEME=$(SCHEME) CAP='$(CAP)' build

# Phony so that Chez, which tracks what each object was built against,
# decides what is stale.
build:
	$(CAP) timeout $(BUILD_TIMEOUT) $(SCHEME) -q --libdirs $(DEPENDENCIES) --script build.ss

# One process per test file, so a crash or leaked global state stays inside
# its file.
test: $(TESTS:%=run/%)

$(TESTS:%=run/%): run/%: build
	@TEST_PORT=$(TEST_PORT) $(CAP) timeout $(TEST_TIMEOUT) $(RUN) ./$(OBJ)/$(*:.sps=.so)

conformance: build
	@TEST_PORT=$(TEST_PORT) $(CAP) timeout $(TEST_TIMEOUT) $(RUN) ./$(OBJ)/tests/conformance-test.so
	@TEST_PORT=$(TEST_PORT) $(CAP) timeout $(TEST_TIMEOUT) $(RUN) ./$(OBJ)/tests/go-conformance.so

clean:
	rm -rf build
