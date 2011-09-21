#pragma once

#ifndef __OPENCL_VERSION__
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <string.h>
#define __global
#else
#define assert(e)
#endif

int round_alloc(int n);
int round_alloc(int n) {
    // round up to the nearest multiple of 32
    n--;
    if(n < 0) n = 0;
    n >>= 5;
    n++;
    n <<= 5;
    return n;
}

void vec_memcpy(__global void *dst, __global const void *src, int size);
void vec_memcpy(__global void *dst, __global const void *src, int size) {
    __global int *dst_i = (__global int *)dst;
    __global const int *src_i = (__global const int *)src;
    for(; size >= 0; size -= sizeof(int)) {
        *dst_i++ = *src_i++;
    }
}

typedef struct {
    int ref_count;
    int used;
    int alloc;
    char data[0];
} vbuf;

#ifndef __OPENCL_VERSION__
vbuf *vbuf_alloc(int size) {
    vbuf *b = (vbuf *)malloc(sizeof(vbuf) + size);

    b->ref_count = 1;
    b->used = 0;
    b->alloc = size;
    return b;
}

vbuf *vbuf_realloc(vbuf *b, int size) {
    b = (vbuf *)realloc(b, sizeof(vbuf) + size);
    b->alloc = size;
    return b;
}

void vbuf_drop(vbuf *b) {
    assert(b->ref_count > 0);
    b->ref_count--;
    if(!b->ref_count) {
        free(b);
    }
}
#endif

int vbuf_size(__global vbuf *b);
int vbuf_size(__global vbuf *b) {
    return sizeof(vbuf) + b->alloc;
}

typedef struct {
    __global vbuf *index;
    __global vbuf *data;

    int dim;
    int unit_sz;
    int base;
} vec;

#ifndef __OPENCL_VERSION__
void *hmk_vec(int rows, int cols, int unit_sz);
void *hmk_vec(int rows, int cols, int unit_sz) {
	int row_sz = 2 * sizeof(int) + cols * unit_sz;
	int total_sz = 2 * sizeof(int) + rows * row_sz;
	int *p = (int *)malloc(total_sz);
	p[0] = total_sz;
	p[1] = rows;

	char *data = (char *)(p+2);
	
	for(int i = 0; i < rows; i++) {
		int *l = (int *)data;
		l[0] = row_sz;
		l[1] = cols;

		data += row_sz;
	}

	return p;
}

void vec_destroy(vec v);
void vec_destroy(vec v) {
    vbuf_drop(v.index);
    vbuf_drop(v.data);
}
#endif

int hvec_byte_size(__global void *v);
int hvec_byte_size(__global void *v) {
  return ((int*)v)[0];
}

int hvec_length(__global void *v, int unit_sz);
int hvec_length(__global void *v, int unit_sz) {
  return ((int*)v)[1];
}

__global void *hvec_ref(__global void *v, int i, int unit_sz);
__global void *hvec_ref(__global void *v, int i, int unit_sz) {
  assert(i < hvec_length(v, unit_sz));
  return (__global void *)((__global char *)v + 2 * sizeof(int) 
						   + i * unit_sz);
}

__global int *vec_index(vec v);
__global int *vec_index(vec v) {
    return (__global int *)(v.index->data + v.base);
}

__global void *vec_ref_1d(vec v, int i);
__global void *vec_ref_1d(vec v, int i) {
    // make sure this is actually a 1d vector
    assert(v.dim == 1);
    __global int *index = vec_index(v);

    // make sure we are in-bounds
    assert(i < index[0]);
    return v.data->data + index[1] + v.unit_sz * i;
}

vec vec_ref_nd(vec v, int i);
vec vec_ref_nd(vec v, int i) {
    assert(v.dim > 1);

    vec r = v;
    r.dim--;

    __global int *index = vec_index(v);
    
    // make sure we're in bounds
    assert(i < index[0]);

    r.base = index[1 + i];

    return r;
}

#ifndef __OPENCL_VERSION__
void vec_serialize(vec v, char *dst);
void vec_serialize(vec v, char *dst) {
    vec *tv = (vec *)dst;
    dst += sizeof(vec);

    vbuf *index = (vbuf *)dst;
    int index_sz = vbuf_size(v.index);
    dst += index_sz;
    
    vbuf *data = (vbuf *)dst;
    int data_sz = vbuf_size(v.data);
    dst += data_sz;

    memcpy(tv, &v, sizeof(vec));
    memcpy(index, v.index, index_sz);
    memcpy(data, v.data, data_sz);
}
#endif

int vec_size(vec v);
int vec_size(vec v) {
    return sizeof(vec) + vbuf_size(v.index) + vbuf_size(v.data);
}

vec vec_deserialize(__global char *src);
vec vec_deserialize(__global char *src) {
    // initialize what fields we can.
    vec v = *(__global vec *)src;

    src += sizeof(vec);
    v.index = (__global vbuf *)src;
    
    src += vbuf_size(v.index);
    v.data = (__global vbuf *)src;
    
    return v;
}

#ifndef __OPENCL_VERSION__
void vec_deserialize_host(char *src, vec *dst) {
    // this assumes the dimensions of the vector don't change
    src += sizeof(vec);
    vbuf *index = (vbuf *)src;
    int index_sz = vbuf_size(index);
    src += index_sz;
    vbuf *data = (vbuf *)src;
    int data_sz = vbuf_size(data);

    memcpy(dst->index, index, index_sz);
    memcpy(dst->data, data, data_sz);
}
#endif
