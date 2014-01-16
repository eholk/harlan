#define NO_GLOBALS
#include "harlan.hpp"

#include <stdlib.h>
#include <limits.h>
#include <string>
#include <algorithm>

using namespace std;

#define ALLOC_MAGIC 0xa110ca7e

#define CHECK_MAGIC(hdr) assert((hdr)->magic == ALLOC_MAGIC)

void check_region(region *r) {
    CHECK_MAGIC(r);
    //assert(r->cl_buffer);
}

void print(bool b, std::ostream *f) {
    if(b)
        print("#t", f);
    else
        print("#f", f);
}

void harlan_error(const char *msg) {
    std::cerr << "Harlan Runtime Error: " << msg << std::endl;
    abort();
}

cl_device_type get_device_type()
{
  const char *cfg = getenv("HARLAN_DEVICE");

  if(cfg) {
    string s = cfg;
	transform(s.begin(), s.end(), s.begin(), ::tolower);
    if(s == "gpu") {
      return CL_DEVICE_TYPE_GPU | CL_DEVICE_TYPE_ACCELERATOR;
    }
    else if(s == "cpu") {
      return CL_DEVICE_TYPE_CPU;
    }
	else {
		cerr << "HARLAN_DEVICE must be either `cpu` or `gpu`." << endl;
		abort();
	}
  }
  return (CL_DEVICE_TYPE_GPU |
          CL_DEVICE_TYPE_CPU |
          CL_DEVICE_TYPE_ACCELERATOR |
          0);
}

int get_default_region_size()
{
  const char *cfg = getenv("HARLAN_MIN_REGION_SIZE");

  if(cfg)
      return atoi(cfg);
  //else return 8192;
  else return 8 << 20; // 8 megs
}

region *create_region(int size)
{
    if(size == -1) size = get_default_region_size();

    assert(size > sizeof(region));

    // void *ptr = GC_MALLOC(size);
    void *ptr = malloc(size);

    region *header = (region *)ptr;
    header->magic = ALLOC_MAGIC;
    header->size = size;
    header->alloc_ptr = sizeof(region);
    header->cl_buffer = NULL;

    check_region(header);

    return header;
}

void free_region(region *r)
{
    //fprintf(stderr, "freeing region %p. %d bytes allocated\n",
    //        r, r->alloc_ptr);
    if(r->cl_buffer) {
        clReleaseMemObject((cl_mem)r->cl_buffer);
    }
    free(r);
}

void map_region(region *header)
{
    cl_int status = 0;

    assert(header->cl_buffer);

    cl_mem buffer = (cl_mem)header->cl_buffer;

    //printf("map_region: old alloc_ptr = %d\n", header->alloc_ptr);

    // Read just the header
    status = clEnqueueReadBuffer(g_queue,
                                 buffer,
                                 CL_TRUE,
                                 0,
                                 sizeof(region),
                                 header,
                                 0,
                                 NULL,
                                 NULL);
    CL_CHECK(status);

    //printf("map_region: new alloc_ptr = %d\n", header->alloc_ptr);

    //printf("map_region: read %lu bytes, reading %lu more.\n",
    //       sizeof(region), header->alloc_ptr - sizeof(region));

    // Now read the contents
    status = clEnqueueReadBuffer(g_queue,
                                 buffer,
                                 CL_TRUE,
                                 sizeof(region),
                                 header->alloc_ptr - sizeof(region),
                                 ((char *)header) + sizeof(region),
                                 0,
                                 NULL,
                                 NULL);
    CL_CHECK(status);

    CL_CHECK(clReleaseMemObject(buffer));
    assert(!header->cl_buffer);
    check_region(header);
}

void unmap_region(region *header)
{
    check_region(header);
    
    // Don't unmap the region twice...
    // TODO: we might want to report a warning in this case.
    if(header->cl_buffer) return;
    //assert(!header->cl_buffer);

    //fprintf(stderr, "unmap_region %p, alloc_ptr = %d\n",
    //        header, header->alloc_ptr);

    cl_int status = 0;
    cl_mem buffer = clCreateBuffer(g_ctx,
                                   CL_MEM_READ_WRITE | CL_MEM_ALLOC_HOST_PTR,
                                   header->size,
                                   NULL,
                                   &status);
    CL_CHECK(status);

    status = clEnqueueWriteBuffer(g_queue,
                                  buffer,
                                  CL_TRUE,
                                  0,
                                  header->alloc_ptr,
                                  header,
                                  0,
                                  NULL,
                                  NULL);
    CL_CHECK(status);

    header->cl_buffer = buffer;
}

region_ptr alloc_in_region(region **r, unsigned int size)
{
    if((*r)->cl_buffer)
        map_region(*r);

    // printf("allocating %d bytes from region %p\n", size, *r);
    region_ptr p = (*r)->alloc_ptr;
    (*r)->alloc_ptr += size;
 
    // If this fails, we allocated too much memory and need to resize
    // the region.
    while((*r)->alloc_ptr > (*r)->size) {
        unsigned int new_size = (*r)->size * 2;
		// As long as we stick with power of two region sizes, this
		// will let us get up to 4GB regions. It's a big of a hacky
		// special case...
		if(new_size == 0) {
		  new_size = UINT_MAX;
		}
		assert(new_size > (*r)->size);
        region *old = *r;
        unsigned int old_size = (*r)->size;
		//printf("realloc(%p, %d)\n", *r, new_size);
        (*r) = (region *)realloc(*r, new_size);

		assert(*r != NULL);

        (*r)->size = new_size;
    }

    return p;
}

cl_mem get_cl_buffer(region *r) 
{
    assert(r->cl_buffer);
    return (cl_mem)r->cl_buffer;
}

int ARGC = 0;
char **ARGV = NULL;

extern int harlan_main();

int main(int argc, char **argv) {
	ARGC = argc;
	ARGV = argv;

	return harlan_main();
}
