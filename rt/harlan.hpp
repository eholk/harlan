/**
   Runtime library for Harlan.
*/

#pragma once

#include <iostream>
#include <string>
#include <algorithm>
#include <assert.h>
#include <string.h>

#include "gc.h"

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
void print(T n) {
    std::cout << n << std::endl;
}

void *alloc_buffer(unsigned int size);
void map_buffer(void *ptr);
void unmap_buffer(void *ptr);
cl_mem get_mem_object(void *ptr);

#define __global
