// xfail
#include "vectors.h"

#include <stdio.h>

/**

   This file tests our representation of vectors.

*/

int buf_test() {
    printf("buf_test...");

    vbuf *b = vbuf_alloc(128);
    
    vbuf_drop(b);

    printf("succeeded.\n");
    return 0;
}

int vec_1d() {
    printf("vec_1d...");

    vec v = mk_vec(1, 4, sizeof(int));
    
    int *i;

    i = (int *)vec_ref_1d(v, 0);
    *i = 1;
    i = (int *)vec_ref_1d(v, 1);
    *i = 2;
    i = (int *)vec_ref_1d(v, 2);
    *i = 3;
    i = (int *)vec_ref_1d(v, 3);
    *i = 4;
    
    i = (int *)vec_ref_1d(v, 0);
    printf("%d\n", *i);
    assert(*i == 1);

    i = (int *)vec_ref_1d(v, 1);
    printf("%d\n", *i);
    assert(*i == 2);

    i = (int *)vec_ref_1d(v, 2);
    printf("%d\n", *i);
    assert(*i == 3);

    i = (int *)vec_ref_1d(v, 3);
    printf("%d\n", *i);
    assert(*i == 4);

    vec_destroy(v);

    printf("succeeded.\n");
    return 0;
}

vec iota(int n) {
    vec v = mk_vec(1, n, sizeof(int));

    for(int j = 0; j < n; ++j) {
        int *i = (int *)vec_ref_1d(v, j);
        *i = j;
    }

    return v;
}

int print_iota() {
    printf("print_iota...\n");

    for(int i = 1; i <= 10; ++i) {
        vec v = iota(i);
        print_int_vec(v);
        vec_destroy(v);
    }

    printf("succeeded.\n");
    return 0;
}

int vec_2d() {
    printf("vec_2d...\n");

    const int n = 4;

    vec v = mk_vec(2, n, sizeof(int));
    
    for(int i = 0; i < n; i++) {
        vec r = vec_ref_nd(v, i);
        assert(vec_length(r) == 0);
        vec t = iota(i + 1);
        vec_set_vec(&v, i, t);
        vec_destroy(t);
    }

    print_vec_vec_int(v);

    //vec_destroy(v);
    printf("succeeded.\n");
    return 0;
}

int vec_2d_init() {
    printf("vec_2d_init...\n");

    const int n = 4;

    vec v = mk_vec(2, n, sizeof(int));
    
    for(int i = 0; i < n; i++) {
        // Make sure we initialized everything to a zero length vector.
        assert(vec_length(vec_ref_nd(v, i)) == 0);
    }

    print_vec_vec_int(v);

    //vec_destroy(v);
    printf("succeeded.\n");
    return 0;
}

int main() {
    return buf_test()
        || vec_1d()
        || print_iota()
        || vec_2d_init()
        || vec_2d();
}
