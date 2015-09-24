// the #if silences warnings on newer OpenCLs.
#if __OPENCL_VERSION__ < 120
#pragma OPENCL EXTENSION cl_khr_fp64: enable
#endif

// This gives us a pointer to something in a region.
#define get_region_ptr(r, i) (((char __global *)r) + i)

typedef unsigned long uint64_t;

region_ptr alloc_in_region(region __global *r, unsigned int size);
region_ptr alloc_in_region(region __global *r, unsigned int size)
{
    // region_ptr p = r->alloc_ptr;
    // r->alloc_ptr += size;
    region_ptr p = ((region_ptr) atomic_add(&(r->alloc_ptr), size));
    
    // If this fails, we allocated too much memory and need to resize
    // the region.
    if(p + size > r->size) {
        return 0;
    }

    return p;
}

region_ptr alloc_vector(region __global *r, int item_size, int num_items);
region_ptr alloc_vector(region __global *r, int item_size, int num_items)
{
    region_ptr old_alloc = r->alloc_ptr;
    region_ptr p = alloc_in_region(r, 8 + item_size * num_items);
    if(!p) {
        return 0;
    }

    int __global *length_field = (int __global *)get_region_ptr(r, p);
    *length_field = num_items;
    return p;
}

// 2 means allocation failure
// FIXME: with the new small danger vectors, this doesn't actually
// report allocation failures...
#define harlan_error(code) { /**danger = 2*/; /* return */; }

typedef int cl_int;

// This is the kernel that is used by the CPU to allocate vectors in
// regions already on the GPU... or it will be soon, anyway.
__kernel void harlan_rt_alloc_vector(void __global *r,
                                     int item_size,
                                     int num_items,
                                     region_ptr __global *result)
{
    *result = alloc_vector((region __global *)r, item_size, num_items);
}


/*
  FIXME: The next next bit is an awful hack for when kernel-local
  regions leak into the compiled program. This tends to happen when
  making lots of function calls from the kernel. Here we just use a
  NULL pointer as the region, which works as long as we never touch
  the region (for example, lambdas always have a region but it's often
  not used).

  Commented out below is an attempt I had to declare a tiny region on
  the stack. Unfortunately, OpenCL makes a distinction between local
  and global pointers, so we'd need versions of the function that work
  on different pointer types. This will take some pretty invasive
  changes to the type inferencer and the rest of the compiler, since
  we'll have a notion of local and global regions.

  Another option is to early on identify which functions are called by
  kernels and generate two versions. The kernel version would not
  allocate any regions, and instead rely on the callers to supply
  them. This again complicates the type system and will require help
  from the type inferencer.

  Finally, we could modify the compiler (such as in
  fix-kernel-local-regions) to just find a nearby region and use that
  instead. It's a hack, but it is a fairly local change and I don't
  think it introduces any unsoundness, unlike the way things are
  currently.

  I'll open an issue to keep track of this.

*/

#define KERNEL_LOCAL_REGION_SIZE 128

// Free region is a no-op on the GPU
#define free_region(r)
//#define declare_region(r) __global unsigned char r##_buf[KERNEL_LOCAL_REGION_SIZE]
#define declare_region(r) __global region *r = (__global region *)0
//#define init_region(r) init_region_real(r##_buf)
//#define init_region(r) (__global region *)0

//__global region *init_region_real(__global unsigned char *buf) {
//    __global region *r = (__global region *)buf;
//
//    r->magic = ALLOC_MAGIC;
//    r->size = KERNEL_LOCAL_REGION_SIZE;
//    r->alloc_ptr = sizeof(region);
//    r->cl_buffer = 0;
//
//    return r;
//}
