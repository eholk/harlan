/* -*- c++ -*-
  Code that gets included in both the CPU and GPU code. This is mostly
  basic data structures.
 */

#pragma once

// This is mostly opaque to the GPU.
struct alloc_header {
    unsigned int magic;

    // Size of this header + the stuff
    unsigned int size;

    // This is actually a cl_mem
    void *cl_buffer;
};

#define adjust_header(p) ((char *)p + sizeof(struct alloc_header))
