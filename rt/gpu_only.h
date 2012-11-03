
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

// 2 means allocation failure
#define harlan_error(code) { *danger = 2; return; }
