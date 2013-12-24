Harlan
==========

Harlan is a declarative, domain specific language for programming
GPUs. This project serves primarily as a testbed for implementation
and optimization techniques. The language is intentionally small, in
order to simplify the process of exploring new analyses and
optimizations.

Getting Started
----------

Harlan is known to build and run on the following operating systems.
* Mac OS X 10.6 (Snow Leopard)
* Mac OS X 10.7 (Lion)
* Mac OS X 10.8 (Mountain Lion)
* Mac OS X 10.9 (Mavericks)
* Various flavors of Linux

Others will probably work as well.

Harlan requires an OpenCL implementation as well as Petite Chez
Scheme. Below are several OpenCL implementations that should work.

* [Intel OpenCL SDK](http://software.intel.com/en-us/vcsource/tools/opencl-sdk)
* [NVIDIA CUDA Toolkit](http://developer.nvidia.com/cuda-toolkit)
* [AMD Accelerated Parallel Processing (APP) SDK](http://developer.amd.com/SDKS/AMDAPPSDK/Pages/default.aspx)

Petite Cheze Scheme can be downloaded from
[http://www.scheme.com/download](http://www.scheme.com/download).

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
