/* -*- c++ -*-
  Code that gets included in both the CPU and GPU code. This is mostly
  basic data structures.
 */

#pragma once

// This is mostly opaque to the GPU.
struct alloc_header {
    // This is actually a cl_mem
    void *cl_buffer;

    // Size of this header + the stuff
    unsigned int size;
};

#define adjust_header(p) ((char *)p + sizeof(struct alloc_header))

// TODO: Make sure the CL version and the CPU version have the same
// sizes.
//
// Actually, we'll probably be okay as long as the CPU version is
// bigger.
