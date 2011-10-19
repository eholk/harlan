/**
   Builtin functions that are useful for doing extern with.
 */

#include <stdio.h>
#include <inttypes.h>
#include <time.h>

#ifdef __APPLE__
#include <mach/mach_time.h>
#endif

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

// ((vector (vector int 2) 2)) -> void
void print_2x2_int_vec(int *v) {
    printf("[ %d %d ]\n"
           "[ %d %d ]\n",
           v[0], v[1], v[2], v[3]);
}
