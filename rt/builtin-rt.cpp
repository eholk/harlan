// Builtins for the runtime system.

#include <inttypes.h>

#define NO_GLOBALS
#include "harlan.hpp"

extern uint64_t g_memtime;
extern cl::device_list g_devices;

// () -> float
//
// Returns the amount of time spend in memory copying.
float rt$dmem$dcopy$dtime() {
    return double(g_memtime) / 1e9;
}

// () -> bool
//
// Returns whether the current device is a CPU device
bool_t rt$dis$dcpu() {
	if(CL_DEVICE_TYPE_CPU & g_devices[0].type()) {
        return true;
    }
    else {
        return false;
    }
}
