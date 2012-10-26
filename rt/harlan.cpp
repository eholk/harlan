#define NO_GLOBALS
#include "harlan.hpp"

#include <stdlib.h>
#include <string>

using namespace std;

#define ALLOC_MAGIC 0xa110ca7e

#define CHECK_MAGIC(hdr) assert((hdr)->magic == ALLOC_MAGIC)

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
    if(s == "gpu") {
      return CL_DEVICE_TYPE_GPU | CL_DEVICE_TYPE_ACCELERATOR;
    }
    else if(s == "cpu") {
      return CL_DEVICE_TYPE_CPU;
    }
  }
  return (CL_DEVICE_TYPE_GPU |
          CL_DEVICE_TYPE_CPU |
          CL_DEVICE_TYPE_ACCELERATOR |
          0);
}

// void finalize_buffer(void *buffer, void *data)
void finalize_buffer(region *r)
{
    CHECK_MAGIC(r);
    CL_CHECK(clReleaseMemObject((cl_mem)r->cl_buffer));
}

region *create_region(unsigned int size)
{
    assert(size > sizeof(region));

    // void *ptr = GC_MALLOC(size);
    void *ptr = malloc(size);

    region *header = (region *)ptr;
    header->magic = ALLOC_MAGIC;
    header->size = size;
    header->alloc_ptr = sizeof(region);

    cl_int status = 0;
    header->cl_buffer = clCreateBuffer(g_ctx,
                                       CL_MEM_READ_WRITE | CL_MEM_USE_HOST_PTR,
                                       size,
                                       ptr,
                                       &status);
    CL_CHECK(status);

    // GC_register_finalizer(ptr, finalize_buffer, NULL, NULL, NULL);

    // Make the buffer accessible to the CPU
    clEnqueueMapBuffer(g_queue,
                       (cl_mem)header->cl_buffer,
                       CL_TRUE, // blocking
                       CL_MAP_READ | CL_MAP_WRITE,
                       0,
                       size,
                       0,
                       NULL,
                       NULL,
                       &status);
    CL_CHECK(status);

    return header;
}

void free_region(region *r)
{
  finalize_buffer(r);
  free(r);
}

void map_region(region *header)
{
    cl_int status = 0;
    CHECK_MAGIC(header);
    clEnqueueMapBuffer(g_queue,
                       (cl_mem)header->cl_buffer,
                       CL_TRUE, // blocking
                       CL_MAP_READ | CL_MAP_WRITE,
                       0,
                       header->size,
                       0,
                       NULL,
                       NULL,
                       &status);
    CL_CHECK(status);
}

void unmap_region(region *header)
{
    CHECK_MAGIC(header);
    clEnqueueUnmapMemObject(g_queue,
                            (cl_mem)header->cl_buffer,
                            header,
                            0,
                            NULL,
                            NULL);
}

region_ptr alloc_in_region(region **r, unsigned int size)
{
    region_ptr p = (*r)->alloc_ptr;
    (*r)->alloc_ptr += size;
 
    // If this fails, we allocated too much memory and need to resize
    // the region.
    assert((*r)->alloc_ptr < (*r)->size);

    return p;
}

cl_mem get_cl_buffer(region *r) 
{
    return (cl_mem)r->cl_buffer;
}
