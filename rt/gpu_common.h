/* -*- c++ -*-
  Code that gets included in both the CPU and GPU code. This is mostly
  basic data structures.
 */

#pragma once

typedef unsigned int region_ptr;

// This is mostly opaque to the GPU.
struct region {
    unsigned int magic;

    // Size of this header + the stuff
    unsigned int size;

    // This is the next thing to allocate
    region_ptr alloc_ptr;

    // This is actually a cl_mem
    void *cl_buffer;
};

// This gives us a pointer to something in a region.
#define get_region_ptr(r, i) (((char *)r) + i)
