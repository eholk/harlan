/**
   Runtime library for Harlan.
*/

#pragma once

#include <iostream>
#include <string>
#include <algorithm>
#include <assert.h>
#include <string.h>
#include <cmath>

#ifdef __APPLE__
#include <OpenCL/opencl.h>
#else
#include <CL/opencl.h>
#endif

#include "gpu_common.h"

#include "cl++.h"

cl_device_type get_device_type();

#ifndef NO_GLOBALS
cl::device_list g_devices(get_device_type());
cl::context g_ctx(g_devices);
cl::command_queue g_queue(g_ctx.createCommandQueue(g_devices[0]));
#else
extern cl::device_list g_devices;
extern cl::context g_ctx;
extern cl::command_queue g_queue;
#endif

template<typename T>
void print(T n, std::ostream *f) {
	(*f).precision(10);
	*f << n;
}

void print(bool b, std::ostream *f);

template<typename T>
void print(T n) {
    print(n, &std::cout);
}

region *create_region(int size = -1);
void free_region(region *r);
void map_region(region *ptr);
void unmap_region(region *ptr);
region_ptr alloc_in_region(region **r, unsigned int size);
region_ptr alloc_vector(region **r, int item_size, int num_items);
cl_mem get_cl_buffer(region *r);
void harlan_error(const char *msg) __attribute__((noreturn));
bool hstrcmp(const char *lhs, const char *rhs);

#define __global

inline void *get_region_ptr(region *r, region_ptr i) {
    if(r->cl_buffer) {
        map_region(r);
    }

    return (((char __global *)r) + i);
}

const char *danger_name(int danger_type);

// FFI-related functions. These are pretty low level and could
// probably be open-coded by the compiler.

#define mk_refs(T) \
    inline T unsafe$dderef$d##T(T *p, int i) { return p[i]; } \
    inline void unsafe$dset$b$d##T(T *p, int i, T x) { p[i] = x; }

mk_refs(float)
mk_refs(int)
mk_refs(char)    
