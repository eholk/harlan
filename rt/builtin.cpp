/**
   Builtin functions that are useful for doing extern with.
 */

#include <stdio.h>
#include <inttypes.h>
#include <time.h>
#include <gc/gc.h>

#ifdef __APPLE__
#include <mach/mach_time.h>
#endif

#include <iostream>
#include <fstream>

using namespace std;

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

// (str) -> (ptr ofstream)
std::ofstream* open_outfile(const char *filename) {
  std::ofstream f;
  f.open(filename);
  return &f;
}

