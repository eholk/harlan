;; -*- scheme -*-

;; Uses the broken outdated vector representation...
; xfail

;; A hand-written OpenCL kernel, to make sure the backend is in good
;; enough shape.

((global std::string g_progtext
         "
#include <vectors.h>

__kernel void simple(__global void *X_0) {
    int i = get_global_id(0);

    vec X = vec_deserialize(X_0);

    *(__global int*)vec_ref_1d(X,i) = *(__global int*)vec_ref_1d(X,i) + 1;
}")

 (global cl::program g_prog ((field g_ctx createProgramFromSource)
                             g_progtext))
 
 (func int main ()
       (do ((field g_prog build)))

       (let X vec (mk_vec 1 4 (sizeof int)))
       (set! (deref (cast (ptr int) (vec_ref_1d X 0))) 1)
       (set! (deref (cast (ptr int) (vec_ref_1d X 1))) 2)
       (set! (deref (cast (ptr int) (vec_ref_1d X 2))) 3)
       (set! (deref (cast (ptr int) (vec_ref_1d X 3))) 4)

       (do (print_int_vec X))
       
       (let K cl::kernel ((field g_prog createKernel) "simple"))

       (let X_gpu (cl::buffer char)
         ;; TODO: the type instantiation here is kind of a hack.
         ((field g_ctx createBuffer<char>) (vec_size X) CL_MEM_READ_WRITE))
       (block
        (let X_ptr cl::buffer_map<char>
          ((field g_queue mapBuffer<char>) X_gpu))
        (do (vec_serialize X X_ptr)))
       
       (do ((field K setArg) 0 X_gpu)
           ((field g_queue execute) K (vec_length X) 1))

       (block
        (let X_ptr cl::buffer_map<char>
          ((field g_queue mapBuffer<char>) X_gpu))
        ;; This is going to leak all kinds of memory...
        (do (vec_deserialize_host X_ptr (addressof X))))

       (do (print_int_vec X)
           (assert (= (deref (cast (ptr int) (vec_ref_1d X 0))) 2))
         (assert (= (deref (cast (ptr int) (vec_ref_1d X 1))) 3))
         (assert (= (deref (cast (ptr int) (vec_ref_1d X 2))) 4))
         (assert (= (deref (cast (ptr int) (vec_ref_1d X 3))) 5)))
       
       (return 0)))
