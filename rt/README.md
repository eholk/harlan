Harlan programs require a runtime library that is written in C++. This
includes code for managing OpenCL, data transfers, etc.

The lazy data transfer feature is implemented here, by the
`get_region_ptr` function in `harlan.hpp`. It checks the region header
to see if the region is on the CPU, and if not it uses `map_region`,
defined in `harlan.cpp` to transfer the region over.
