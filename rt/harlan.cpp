#define NO_GLOBALS
#include "harlan.hpp"

#include <stdlib.h>
#include <string>

using namespace std;

#define ALLOC_MAGIC 0xa110ca7e

#define CHECK_MAGIC(hdr) assert((hdr)->magic == ALLOC_MAGIC)

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

void finalize_buffer(void *buffer, void *data)
{
    alloc_header *header = (alloc_header *)buffer;
    CHECK_MAGIC(header);
    CL_CHECK(clReleaseMemObject((cl_mem)header->cl_buffer));
}

void *alloc_buffer(unsigned int size)
{
    unsigned int new_size = size + sizeof(alloc_header);

    void *ptr = GC_MALLOC(new_size);

    alloc_header *header = (alloc_header *)ptr;
    header->magic = ALLOC_MAGIC;
    header->size = new_size;

    cl_int status = 0;
    header->cl_buffer = clCreateBuffer(g_ctx,
                                    CL_MEM_READ_WRITE | CL_MEM_USE_HOST_PTR,
                                    new_size,
                                    ptr,
                                    &status);
    CL_CHECK(status);

    GC_register_finalizer(ptr, finalize_buffer, NULL, NULL, NULL);

    // Make the buffer accessible to the CPU
    clEnqueueMapBuffer(g_queue,
                       (cl_mem)header->cl_buffer,
                       CL_TRUE, // blocking
                       CL_MAP_READ | CL_MAP_WRITE,
                       0,
                       new_size,
                       0,
                       NULL,
                       NULL,
                       &status);
    CL_CHECK(status);

    return ((char *)ptr) + sizeof(alloc_header);
}

void map_buffer(void *ptr)
{
    cl_int status = 0;
    alloc_header *header = (alloc_header *)((char *)ptr - sizeof(alloc_header));
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

void unmap_buffer(void *ptr)
{
    alloc_header *header = (alloc_header *)((char *)ptr - sizeof(alloc_header));
    CHECK_MAGIC(header);
    clEnqueueUnmapMemObject(g_queue,
                            (cl_mem)header->cl_buffer,
                            header,
                            0,
                            NULL,
                            NULL);
}

cl_mem get_mem_object(void *ptr)
{
    alloc_header *header = (alloc_header *)((char *)ptr - sizeof(alloc_header));
    CHECK_MAGIC(header);
    return (cl_mem)header->cl_buffer;
}
