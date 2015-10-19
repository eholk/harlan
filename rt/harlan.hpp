/**
   Runtime library for Harlan.
*/

#pragma once

#include <iostream>
#include <string>
#include <algorithm>
#include <assert.h>
#include <string.h>
#include <stdlib.h>
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

#define validate_region(r) { \
		if(DEAD_REGION == (r)->magic) { \
			std::cerr << "Attempting to access freed region " \
			          << r << " in " << __FUNCTION__ << std::endl; \
			abort(); \
		} \
		if(ALLOC_MAGIC != (r)->magic) { \
			std::cerr << "Invalid magic on region " \
			          << r << " in " << __FUNCTION__ << std::endl; \
			abort(); \
		} \
	}

inline void *get_region_ptr(region *r, region_ptr i) {
	validate_region(r);
	
    if(r->cl_buffer) {
        map_region(r);
    }

    return (((char __global *)r) + i);
}

inline region_ptr get_alloc_ptr(region *r) {
	validate_region(r);
	
    if(r->cl_buffer) {
        map_region(r);
    }

    return r->alloc_ptr;
}

inline void set_alloc_ptr(region *r, region_ptr p) {
	validate_region(r);
	
    if(r->cl_buffer) {
        map_region(r);
    }

    r->alloc_ptr = p;
}

void reserve_at_least(region **r, int size);

const char *danger_name(int danger_type);

// FFI-related functions. These are pretty low level and could
// probably be open-coded by the compiler.

#define mk_refs(T) \
    inline T unsafe$dderef$d##T(T *p, int i) { return p[i]; } \
    inline void unsafe$dset$b$d##T(T *p, int i, T x) { p[i] = x; }

mk_refs(float)
mk_refs(int)
mk_refs(char)    
