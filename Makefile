
ALL_TEST_SRC = $(filter-out %~ %\#, $(wildcard test/*))
XFAIL_TEST_SRC = $(shell grep -l xfail $(ALL_TEST_SRC))

RUN_TEST_SRC = $(filter-out $(XFAIL_TEST_SRC), $(ALL_TEST_SRC))

CXXFLAGS := -g -O2 gc/lib/libgc.a -Igc/include -Irt

# Set up the flags to handle OpenCL
ifeq ($(shell uname), Darwin)
CXXFLAGS := $(CXXFLAGS) -framework OpenCL
CXX := g++-4.2
else
ifeq ($(shell uname), Linux)
# This should work on 64-bit Gentoo with NVIDIA GPUs at least. YMMV.
CXXFLAGS := $(CXXFLAGS) -I/opt/cuda/include -lOpenCL -lrt
CXX := g++
else
$(error Your operating system is not yet supported.)
endif
endif

HARLAN_SRC = harlanc \
	harlan/generate-kernel-calls.scm \
	harlan/parser.scm \
	annotate-free-vars.scm \
	harlan/convert-types.scm \
	harlan/compiler.scm \
	harlan/move-gpu-data.scm \
	harlan/remove-nested-kernels.scm \
	kernels.scm \
	lift-vectors.scm \
	lower-vectors.scm \
	print-c.scm \
	returnify-kernels.scm \
	returnify.scm \
	typecheck.scm \
	uglify-vectors.scm \
	verify-compile-module.scm \
	util/verify-grammar.ss \

# Invokes the harlan compiler. The first argument is the name of the
# source file, the second is the name of the output file.
HC = ./harlanc $(1) | $(CXX) -x c++ - -x none rt/libharlanrt.a $(CXXFLAGS) \
     -o $(2)

TEST_EXE_NAME = $(patsubst test/%, test.bin/%.bin, $(1))

TEST_OUT_NAME = $(patsubst test/%, test.bin/%.out, $(1))

COMPILE_TEST = $(call HC, $(1), $(call TEST_EXE_NAME, $(1)))

RUN_TEST = $(1)

.phony: check
check : test.bin gc/lib/libgc.a rt/libharlanrt.a \
	$(call TEST_OUT_NAME, $(RUN_TEST_SRC))
	@echo All tests succeeded.

.phony: force-check
force-check :
	rm -rf test.bin
	make -C . check

test.bin:
	mkdir -p test.bin

.phony: clean
clean:
	rm -rf test.bin *.dSYM gc
	Make -C rt clean

test.bin/%.out : test.bin/%.bin
	@echo Running $<
	@$(call RUN_TEST, $(call TEST_EXE_NAME, $<)) > $@

.precious : $(call TEST_EXE_NAME, $(RUN_TEST_SRC))
test.bin/%.bin : test/% rt/libharlanrt.a $(HARLAN_SRC)
	@echo Compiling $<
	$(call COMPILE_TEST, $<)

gc/lib/libgc.a :
	cd gc-7.2alpha6 && \
	./configure --prefix=`pwd`/../gc && \
	make -j4 && \
	make install

rt/libharlanrt.a : rt/*.h rt/*.cpp
	make -C rt
