#define NO_GLOBALS
#include "harlan.hpp"

#include <stdlib.h>
#include <string>

using namespace std;

#define ALLOC_MAGIC 0xa110ca7e

#define CHECK_MAGIC(hdr) assert((hdr)->magic == ALLOC_MAGIC)

void check_region(region *r) {
    CHECK_MAGIC(r);
    assert(r->cl_buffer);
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

int get_default_region_size()
{
  const char *cfg = getenv("HARLAN_MIN_REGION_SIZE");

  if(cfg)
      return atoi(cfg);
  //else return 8192;
  else return 16 << 20; // 16 megs
}

void finalize_buffer(region *r)
{
    check_region(r);
    CL_CHECK(clReleaseMemObject((cl_mem)r->cl_buffer));
}

region *create_region(int size)
{
    if(size == -1) size = get_default_region_size();

    assert(size > sizeof(region));

    // void *ptr = GC_MALLOC(size);
    void *ptr = malloc(size);

    cl_int status = 0;
    cl_mem buffer = clCreateBuffer(g_ctx,
                                   CL_MEM_READ_WRITE | CL_MEM_USE_HOST_PTR,
                                   size,
                                   ptr,
                                   &status);
    CL_CHECK(status);

    // Make the buffer accessible to the CPU
    clEnqueueMapBuffer(g_queue,
                       (cl_mem)buffer,
                       CL_TRUE, // blocking
                       CL_MAP_READ | CL_MAP_WRITE,
                       0,
                       size,
                       0,
                       NULL,
                       NULL,
                       &status);
    CL_CHECK(status);

    region *header = (region *)ptr;
    header->magic = ALLOC_MAGIC;
    header->size = size;
    header->alloc_ptr = sizeof(region);
    header->cl_buffer = buffer;
    assert(header->cl_buffer);

    check_region(header);

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
    check_region(header);
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
    check_region(header);
    clEnqueueUnmapMemObject(g_queue,
                            (cl_mem)header->cl_buffer,
                            header,
                            0,
                            NULL,
                            NULL);
}

region_ptr alloc_in_region(region **r, unsigned int size)
{
    // printf("allocating %d bytes from region %p\n", size, *r);
    region_ptr p = (*r)->alloc_ptr;
    (*r)->alloc_ptr += size;
 
    // If this fails, we allocated too much memory and need to resize
    // the region.
    while((*r)->alloc_ptr > (*r)->size) {
        // Free the OpenCL backing buffer
        finalize_buffer(*r);
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

        //printf("resized region %p with size %d to %p with size %d\n",
        //       old, old_size, *r, new_size);

        cl_int status = 0;
        (*r)->cl_buffer = clCreateBuffer(g_ctx,
                                         CL_MEM_READ_WRITE
                                         | CL_MEM_USE_HOST_PTR,
                                         (*r)->size,
                                         *r,
                                         &status);
        CL_CHECK(status);
        
        // Make the buffer accessible to the CPU
        clEnqueueMapBuffer(g_queue,
                           (cl_mem)(*r)->cl_buffer,
                           CL_TRUE, // blocking
                           CL_MAP_READ | CL_MAP_WRITE,
                           0,
                           (*r)->size,
                           0,
                           NULL,
                           NULL,
                           &status);
        CL_CHECK(status);    
    }

    return p;
}

cl_mem get_cl_buffer(region *r) 
{
    return (cl_mem)r->cl_buffer;
}
