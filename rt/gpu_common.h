/* -*- c++ -*-
  Code that gets included in both the CPU and GPU code. This is mostly
  basic data structures.
 */

#pragma once

#ifndef cl_uint
#define cl_uint  uint
#define cl_ulong ulong
#endif

// This is mostly opaque to the GPU.
struct alloc_header {
    cl_uint magic;

    // Size of this header + the stuff
    cl_uint size;

    // This is actually a cl_mem
    cl_ulong cl_buffer;
};

#define adjust_header(p) ((char *)p + sizeof(struct alloc_header))

// TODO: Make sure the CL version and the CPU version have the same
// sizes.
//
// Actually, we'll probably be okay as long as the CPU version is
// bigger.
