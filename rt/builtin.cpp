/**
   Builtin functions that are useful for doing extern with.
 */

#include <stdio.h>
#include <inttypes.h>
#include <time.h>

#ifdef __APPLE__
#include <mach/mach_time.h>
#endif

#include <iostream>
#include <fstream>

using namespace std;

#define NO_GLOBALS
#include "harlan.hpp"
#undef NO_GLOBALS

// () -> u64
uint64_t nanotime() {
#ifdef __APPLE__
    uint64_t time = mach_absolute_time();
    mach_timebase_info_data_t info = {0, 0};
    if (info.denom == 0) {
        mach_timebase_info(&info);
    }
    uint64_t time_nano = time * (info.numer / info.denom);
    return time_nano;  
#else
    uint64_t ns_per_s = 1000000000LL;
    timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    return (ts.tv_sec * ns_per_s + ts.tv_nsec);
#endif    
}

// () -> float
//
// Retuns the value of a timer in seconds.
float time$s() {
  return float(nanotime()) / 1e9;
}

// (str) -> (ptr ofstream)
std::ofstream* open_outfile(const char *filename) {
    // TODO: This is leaking files
    std::ofstream *f = new std::ofstream;
    f->open(filename);
    return f;
}

void close_outfile(std::ofstream *f) {
    f->close();
    delete f;
}

uint64_t get_kernel_time() {
    return g_queue.get_kernel_time();
}

void reset_kernel_time() {
    g_queue.reset_kernel_time();
}

float sqrt(float x) {
    return sqrtf(x);
}

// FFI-related functions. These are pretty low level and could
// probably be open-coded by the compiler.

#define mk_refs(T) \
    T unsafe$deref$##T(T *p, int i) { return p[i]; } \
    void unsafe$set$b$##T(T *p, int i, T x) { p[i] = x; }

mk_refs(float)
mk_refs(int)
mk_refs(char)    
