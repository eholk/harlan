#pragma OPENCL EXTENSION cl_khr_fp64: enable

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
    if(r->alloc_ptr > r->size) {
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
        r->alloc_ptr = old_alloc;
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
__kernel void harlan_rt_alloc_vector(region __global *r,
                                     int item_size,
                                     int num_items,
                                     region_ptr __global *result)
{
    *result = alloc_vector(r, item_size, num_items);
}

