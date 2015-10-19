#define NO_GLOBALS
#include "harlan.hpp"

#include <limits.h>
#include <string>
#include <algorithm>

using namespace std;

#define CHECK_MAGIC(hdr) assert((hdr)->magic == ALLOC_MAGIC)

extern cl::program g_prog;

extern uint64_t nanotime();
uint64_t g_memtime = 0;

void check_region(region *r) {
	validate_region(r);
    CHECK_MAGIC(r);
    //assert(r->cl_buffer);
}

void print(bool b, std::ostream *f) {
    if(b)
        print("#t", f);
    else
        print("#f", f);
}

bool hstrcmp(const char *lhs, const char *rhs) {
	return string(lhs) == rhs;
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
    static bool first_time = true;

    const char *cfg = getenv("HARLAN_MIN_REGION_SIZE");

    if(cfg) {
        int sz = atoi(cfg);
        if(first_time) {
            cerr << "Setting region size from HARLAN_MIN_REGION_SIZE to "
                 << sz << " bytes." << endl;
            first_time = false;
        }
        return sz;
    }
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

    // Start out with the region unmapped, to avoid needless copies.
    unmap_region(header);

    return header;
}

void free_region(region *r)
{
	fprintf(stderr, "freeing region %p. %d bytes of %d allocated\n",
	        r, r->alloc_ptr, r->size);
	validate_region(r);
    if(r->cl_buffer) {
	    cl_mem buffer = (cl_mem)r->cl_buffer;

	    // Get the reference count. It should just be one.
	    cl_uint count = 0;
	    cl_int status = clGetMemObjectInfo(buffer,
	                                       CL_MEM_REFERENCE_COUNT,
	                                       sizeof(count),
	                                       &count,
	                                       NULL);
	    fprintf(stderr, "releasing cl_mem associated with %p. rc=%d\n",
	            r, count);
        clReleaseMemObject(buffer);
    }
    r->magic = DEAD_REGION;
    free(r);
}

void map_region(region *header)
{
    uint64_t start = nanotime();
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

    if(header->alloc_ptr > header->size) {
	    fprintf(stderr,
	            "WARNING: reading over-allocated region %p"
	            " (%d out of %d bytes)\n",
	            header,
	            header->alloc_ptr,
	            header->size);
    }
    
    //printf("map_region: new alloc_ptr = %d\n", header->alloc_ptr);

    //printf("map_region: read %lu bytes, reading %lu more.\n",
    //       sizeof(region), header->alloc_ptr - sizeof(region));

    // Now read the contents, being careful not to read too much if
    // the kernel over-allocated.
    int remaining
	    = min((unsigned int)header->alloc_ptr, header->size) - sizeof(region);
	if(remaining > 0) {
		status = clEnqueueReadBuffer(g_queue,
									 buffer,
									 CL_TRUE,
									 sizeof(region),
									 remaining,
									 ((char *)header) + sizeof(region),
									 0,
									 NULL,
									 NULL);
		CL_CHECK(status);
	}

    CL_CHECK(clReleaseMemObject(buffer));
    assert(!header->cl_buffer);
    check_region(header);
    g_memtime += nanotime() - start;
}

void unmap_region(region *header)
{
    uint64_t start = nanotime();
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
    g_memtime += nanotime() - start;
}

region_ptr alloc_in_region(region **r, unsigned int size)
{
    if((*r)->cl_buffer) {
        map_region(*r);
    }

    // printf("allocating %d bytes from region %p\n", size, *r);
    region_ptr p = (*r)->alloc_ptr;
    (*r)->alloc_ptr += size;

    reserve_at_least(r, (*r)->alloc_ptr);
    
    return p;
}

void reserve_at_least(region **r, int size) {
    // If this fails, we allocated too much memory and need to resize
    // the region.
    while(size > (*r)->size) {
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
        fprintf(stderr, "realloc(%p, %d)\n", *r, new_size);
        (*r) = (region *)realloc(*r, new_size);

		assert(*r != NULL);

        (*r)->size = new_size;
    }
}

region_ptr alloc_vector(region **r, int item_size, int num_items)
{
    if((*r)->cl_buffer) {
        //cerr << "Attempting to allocate " << 8 + item_size * num_items <<  " byte vector on GPU." << endl;
        //cerr << "region size = " << (*r)->size << endl;
        // This region is on the GPU. Try to do the allocation there.
        cl::buffer<region_ptr> buf
            = g_ctx.createBuffer<region_ptr>(1, CL_MEM_READ_WRITE);
 
        cl::kernel k = g_prog.createKernel("harlan_rt_alloc_vector");
        k.setArg(0, (*r)->cl_buffer);
        k.setArg(1, item_size);
        k.setArg(2, num_items);
        k.setArg(3, buf);
        g_queue.execute(k, 1);

        cl::buffer_map<region_ptr> map = g_queue.mapBuffer<region_ptr>(buf);
        region_ptr p = map[0];
        if(p) return p;
    }
    //cerr << "Not enough space, allocating on CPU instead." << endl;
    // Well, that failed. I guess we'll do here instead.
    region_ptr p = alloc_in_region(r, 8 + item_size * num_items);
    *(int*)get_region_ptr(*r, p) = num_items;

    return p;
}

cl_mem get_cl_buffer(region *r) 
{
    assert(r->cl_buffer);
    return (cl_mem)r->cl_buffer;
}

const char *DANGER_TABLE[] = {
	"bounds check failure",
	"allocation failure"
};

const char *danger_name(int danger_type) {
	if(danger_type < sizeof(DANGER_TABLE) / sizeof(DANGER_TABLE[0])) {
		return DANGER_TABLE[danger_type];
	}
	else {
		return "Unknown danger";
	}		
}

int ARGC = 0;
char **ARGV = NULL;
