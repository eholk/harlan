;; -*- scheme -*-

;xfail

;; A hand-written OpenCL kernel, to make sure the backend is in good
;; enough shape.

((global std::string g_progtext
         "__kernel void simple(__global float *X) {
    int i = get_global_id(0);
    
    X[i] = X[i] + 1;
}")

 (global cl::program g_prog ((field g_ctx createProgramFromSource)
                             g_progtext))
 
 (func int main ()
       (do ((field g_prog build)))

       (let X (vector float) 4)
       (vector-set! X 0 1)
       (vector-set! X 1 2)
       (vector-set! X 2 3)
       (vector-set! X 3 4)

       (let K cl::kernel ((field g_prog createKernel) "simple"))

       (do ((field X to_gpu))
           ((field K setArg) 0 ((field X get_buffer)))
         ((field g_queue execute) K ((field X length)) 1)
         ((field X from_gpu)))

       (do (assert (= (vector-ref X 0) 2))
           (assert (= (vector-ref X 1) 3))
         (assert (= (vector-ref X 2) 4))
         (assert (= (vector-ref X 3) 5)))
       
       (print X)
       
       (return 0)))
