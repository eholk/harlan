Harlan [![Build Status](https://travis-ci.org/eholk/harlan.svg?branch=master)](https://travis-ci.org/eholk/harlan)
==========

Harlan is a domain specific language for programming GPUs. This
project serves primarily as a testbed for implementation and
optimization techniques. The language is intentionally small, in order
to simplify the process of exploring new analyses and optimizations.

Getting Started
----------

Harlan is known to build and run on the following operating systems.
* Mac OS X 10.6 (Snow Leopard)
* Mac OS X 10.7 (Lion)
* Mac OS X 10.8 (Mountain Lion)
* Mac OS X 10.9 (Mavericks)
* Various flavors of Linux

Others will probably work as well. The OpenCL included with Mac OS X
has several bugs that lead to failures in some of the Harlan test
cases. For the most part, these failures can be ignored.

Harlan requires an OpenCL implementation as well as a compatible
Scheme. Below are several OpenCL implementations that should work.

* [Intel OpenCL SDK](http://software.intel.com/en-us/vcsource/tools/opencl-sdk)
* [NVIDIA CUDA Toolkit](http://developer.nvidia.com/cuda-toolkit)
* [AMD Accelerated Parallel Processing (APP) SDK](http://developer.amd.com/tools-and-sdks/opencl-zone/opencl-tools-sdks/amd-accelerated-parallel-processing-app-sdk/)

Harlan is known to work with (Petite) Chez Scheme and [Vicare]. Petite
Chez Scheme can be downloaded from http://www.scheme.com/download.

[Vicare]: https://github.com/marcomaggi/vicare

Once all the prerequisites are installed, you can compile and run the
test suite as follows.

    make check

If the tests are successful, you will see the following at the end of
all the output:

    All tests succeeded.

The test programs are available in the `test` directory. End-to-end
test programs have the `.kfc` extension. Other extensions represent
code that is valid at various intermediate passes in the compiler.

Make puts test binaries in the `test.bin` directory, and also saves
output from test programs here. Programs may be run directly from this
directory for easier debugging.

Harlan programs can be compiled manually as follows.

    ./harlanc hello.kfc

For debugging purposes, the `-v` flag can be used.

    ./harlanc -v hello.kfc

This causes the compiler to write out the intermediate results from
each compiler pass.

Assuming the Harlan compiler is successful, the compiler will produce
an executable based on the program's filename. For example, the
previous example will produce the file `hello`, which can be executed
directly.

Next Steps
----------

The official Harlan documentation is currently held in the [Harlan Wiki].

The Harlan mailing list is [harlan-dev]. Feel free to join this group
and ask questions of the Harlan developers and other Harlan users.

[Harlan Wiki]: https://github.com/eholk/harlan/wiki
[harlan-dev]: https://groups.google.com/forum/#!forum/harlan-dev

Directory Structure
----------

Here is a quick overview of the various directories included in this
repository.

- `HSBencher` - a utility to automatically run and report the results of
Harlan benchmarks.
- `etc` - miscellaneous tools, including an Emacs mode for Harlan files.
- `external` - the location of several git submodules, including the
[Nanopass Framework] and [Elegant Weapons].
- `harlan` - the source code for the Harlan compiler.
- `lib` - the standard library for Harlan.
- `rt` - the runtime library for Harlan.
- `test` - Harlan test cases and benchmarks.
- `test.bin` - Harlan's test runner (`run-tests.scm`) stores the
compiled binaries and program output for the test cases here.
- `travis` - support files for running Harlan's test suite under
  [Travis CI].
- `util` - miscellaneous Scheme files, including compatability shims for
  different Scheme implementations.

[Nanopass Framework]: https://github.com/akeep/nanopass-framework
[Elegant Weapons]: https://github.com/eholk/elegant-weapons
[Travis CI]: https://travis-ci.org/
