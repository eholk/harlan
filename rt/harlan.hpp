/**
   Runtime library for Harlan.
*/

#pragma once

#include <iostream>
#include <string>
#include <algorithm>
#include <assert.h>
#include <string.h>

#ifdef __APPLE__
#include <OpenCL/opencl.h>
#else
#include <CL/opencl.h>
#endif

#include "gpu_common.h"

enum error {
    HARLAN_ERR_OUT_OF_BOUNDS,
    HARLAN_ERR_MISPLACED_VECTOR
};

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
  *f << n;
}

void print(bool b, std::ostream *f);

template<typename T>
void print(T n) {
    print(n, &std::cout);
}

region *create_region(unsigned int size);
void free_region(region *r);
void map_region(region *ptr);
void unmap_region(region *ptr);
region_ptr alloc_in_region(region *r, unsigned int size);
cl_mem get_cl_buffer(region *r);

void harlan_error(const char *msg);

#define __global
