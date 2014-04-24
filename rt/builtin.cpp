/**
   Builtin functions that are useful for doing extern with.
 */

#include <stdio.h>
#define __STDC_FORMAT_MACROS
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
    return ((uint64_t)ts.tv_sec * (uint64_t)ns_per_s + (uint64_t)ts.tv_nsec);
#endif    
}

// () -> float
//
// Retuns the value of a timer in seconds.
float time$ds() {
	return float(nanotime() / 1000) / 1e6;
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
template<typename T>
struct harlan_vector {
	int64_t length;
	T data[];
};

// in harlan.cpp
extern int ARGC;
extern char **ARGV;

#define VECTOR_LENGTH_OFFSET 8

// () -> (vec str)
region_ptr command$dline(region *&r) {
	region_ptr ptr = alloc_in_region(&r,
									 VECTOR_LENGTH_OFFSET 
									 + ARGC * sizeof(char *));
	harlan_vector<char *> *vec
		= (harlan_vector<char *> *)get_region_ptr(r, ptr);

	vec->length = ARGC;
	
	for(int i = 0; i < ARGC; ++i) {
		vec->data[i] = ARGV[i];
	}

	return ptr;
}

// (str) -> (vec char)
region_ptr str$d$vvec(const char *str, region *&r) {
	int length = strlen(str);

	region_ptr ptr = alloc_in_region(&r,
									 VECTOR_LENGTH_OFFSET
									 + length * sizeof(char));
	harlan_vector<char> *vec
		= (harlan_vector<char> *)get_region_ptr(r, ptr);
	
	vec->length = length;
	
	for(int i = 0; i < length; ++i) {
		vec->data[i] = str[i];
	}
	
	return ptr;
}

const char *EMPTY_ENV_VAR = "";

const char *get$denvironment$dvariable(const char *name) {
    char *v = getenv(name);
    if(v) {
        return v;
    }
    else {
        return EMPTY_ENV_VAR;
    }
}

int hscanf(FILE *f, const char *s, int *i) {
    return fscanf(f, s, i);
}

int hscanfu64(FILE *f, uint64_t *i) {
  return fscanf(f, "%" SCNu64, i);
}

void flush$dstdout() {
	cout.flush();
}
