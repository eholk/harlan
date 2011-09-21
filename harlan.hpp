/**
   Runtime library for Harlan.
*/

#pragma once

#include <iostream>
#include <string>
#include <algorithm>
#include <assert.h>

#include "vectors.h"

enum error {
    HARLAN_ERR_OUT_OF_BOUNDS,
    HARLAN_ERR_MISPLACED_VECTOR
};

#include "cl++.h"

// FIXME: These global variables will cause link errors if we ever
// combine multiple files into a Harlan program.
cl::device_list g_devices(//CL_DEVICE_TYPE_GPU |
						  CL_DEVICE_TYPE_CPU |
						  //CL_DEVICE_TYPE_ACCELERATOR |
						  0);
cl::context g_ctx(g_devices);
cl::command_queue g_queue(g_ctx.createCommandQueue(g_devices[0]));

template<typename T>
class vector {
    size_t len;
    T *data;

    bool on_gpu;
    cl::buffer<T> gpu_buffer;

public:
    vector() 
        : len(0), data(NULL), on_gpu(false),
          gpu_buffer(g_ctx.createBuffer<T>(sizeof(T) * 1, CL_MEM_READ_WRITE))
    {}

    vector(size_t len) 
        : len(len), data(new T[len]),
          on_gpu(false),
          gpu_buffer(g_ctx.createBuffer<T>(sizeof(T) * len, CL_MEM_READ_WRITE))
    {}

    vector(const vector<T> &v) 
        : len(v.len), on_gpu(false),
          gpu_buffer(g_ctx.createBuffer<T>(sizeof(T) * len, CL_MEM_READ_WRITE))
    {
        assert(!v.on_gpu);

        data = new T[len];
        std::copy(v.data, v.data + len, data);
    }

    ~vector() {
        if(data)
            delete [] data;
    }

    vector<T> &operator=(const vector<T> &rhs) {
        assert(!rhs.on_gpu);
        len = rhs.len;
        if(data)
            delete [] data;
        data = new T[len];
        std::copy(rhs.data, rhs.data + len, data);
        on_gpu = false;
        gpu_buffer = g_ctx.createBuffer<T>(sizeof(T) * len, CL_MEM_READ_WRITE);
        return *this;
    }

    T &operator[](size_t index) const {
        assert(!on_gpu);
        assert(index < len);
        return data[index];
    }

    size_t length() const { return len; }

    cl::buffer<T> &get_buffer() {
        assert(on_gpu);
        return gpu_buffer; 
    }

    void to_gpu() {
        assert(!on_gpu);
        on_gpu = true;
        g_queue.write_buffer(get_buffer(), data);
    }

    void from_gpu() {
        assert(on_gpu);
        g_queue.read_buffer(get_buffer(), data);
        on_gpu = false;
    }
};

template<typename T>
void print(T n) {
    std::cout << n << std::endl;
}

template<typename T1, typename T2>
void print(T1 n1, T2 n2) {
  std::cout << n1 << "\t" << n2 << std::endl;
}


void print(void *) {
  // We no longer have enough information to actually print the
  // contents of vectors.
  std::cout << "[vector]" << std::endl;
}

template<typename T>
void print(const vector<T> &v) {
    std::cout << "[" << std::endl;
    for(int i = 0; i < v.length(); ++i) {
        print(v[i]);
    }
    std::cout << "]" << std::endl;
}

template<typename T>
void print(const vector<T> &v1, const vector<T> &v2) {
    std::cout << "[" << std::endl;
    for(int i = 0; i < v1.length(); ++i) {
        print(v1[i]);
    }
    std::cout << "\t";
    for(int i = 0; i < v2.length(); ++i) {
        print(v2[i]);
    }    
    std::cout << "]" << std::endl;
}

#include <mach/mach_time.h>

uint64_t nanotime() {
  // TODO: Currently mac-only. See
  // https://github.com/graydon/rust/blob/master/src/rt/sync/timer.cpp
  // for an example of how to do this on other platforms.
  uint64_t time = mach_absolute_time();
  mach_timebase_info_data_t info = {0, 0};
  if (info.denom == 0) {
	mach_timebase_info(&info);
  }
  uint64_t time_nano = time * (info.numer / info.denom);
  return time_nano;  
}

struct free_this {
  void *p;
  free_this(void *p) : p(p) {}
  ~free_this() { free(p); }
};
