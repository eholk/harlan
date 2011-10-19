(library
 (harlan generate-kernel-calls)
 (export
  generate-kernel-calls)
 (import
  (rnrs)
  (only (print-c) format-ident)
  (util helpers)
  (util match))

  (define-match (generate-kernel-calls)
   ((module ,[Decl -> decl*] ...)
    `(module . ,decl*)))

 (define-match (Decl)
   ((fn ,name ,args ,type ,[Stmt -> stmt*] ...)
    `(fn ,name ,args ,type . ,stmt*))
   (,else else))

 (define (get-arg-length a)
   (match (type-of a)
     ((vector ,t ,n) n)
     (,else (error 'get-arg-length "Invalid kernel argument" a))))
 
 (define Stmt
   (lambda (stmt)
     (match stmt
       ((apply-kernel ,k ,arg* ...)
        (let ((kernel (gensym k)))
          `(block
            (let ,kernel cl::kernel (call cl::kernel
                                          (field (var cl::program g_prog)
                                                 createKernel)
                                          (str ,(symbol->string k))))
            (do ,@(map (lambda (arg i)
                         `(call void
                                (field (var cl::kernel ,kernel) setArg)
                                (int ,i)
                                ,arg))
                       arg* (iota (length arg*)))
                (call void (field (var cl::queue g_queue) execute)
                      (var cl::kernel ,kernel)
                      (int ,(get-arg-length (car arg*))) ;; global size
                      (int 1)))))) ;; local size
       ((for (,i ,start ,end) ,[stmt*] ...)
        `(for (,i ,start ,end) ,stmt* ...))
       (,else else))))

 (define old
   '((apply-kernel ,k ,arg* ...)
        (let ((k-var (gensym 'kernel)))
          `(block
            (let ,k-var cl::kernel
                 (call cl::kernel (field (var cl::program g_prog) createKernel)
                       (str ,(format-ident k))))
            ,@(let ((_gpu* (map (lambda (arg) (gensym '_gpu)) arg*))
                    (_ptr* (map (lambda (arg) (gensym '_ptr)) arg*))
                    (i* (iota (length arg*))))
                `(
                  ;; This is the part where we allocate GPU buffers.
                  ,@(map
                     (lambda (arg _gpu _ptr i)
                       (match arg
                         ((var (ptr char) ,x)
                          (error 'compile-kernel-stmt "Don't do this. (a)")
                          `(let ,_gpu (cl::buffer char)
                                ((field (var cl::context g_ctx)
                                        createBuffer<char>)
                                 (hvec_byte_size ,(unpack-arg arg))
                                 CL_MEM_READ_WRITE)))
                         ((var (vector ,t ,n) ,x)
                          `(let ,_gpu (cl::buffer char)
                                (call
                                 cl::buffer
                                 (field (var cl::context g_ctx)
                                        createBuffer<char>)
                                 ,(vector-bytesize `(vector ,t ,n))
                                 (var int CL_MEM_READ_WRITE))))
                         ((var ,t ,s)
                          '(block)))) ;; no-op
                     arg* _gpu* _ptr* i*)
                  ;; Here's where we copy data to the GPU.
                  ,@(map
                     (lambda (arg _gpu _ptr i)
                       (match arg
                         ((var (ptr char) ,x)
                          (error 'compile-kernel-stmt "Don't do this (b)"))
                         ((var (vector ,t ,n) ,x)
                          `(block
                            (let ,_ptr cl::buffer_map<char>
                                 (call cl::buffer_map<char>
                                       (field (var cl::queue g_queue)
                                              mapBuffer<char>)
                                       (var buffer ,_gpu)))
                            (do (call void memcpy (var ptr ,_ptr)
                                      (var ptr ,(unpack-arg arg))
                                      ,(vector-bytesize `(vector ,t ,n))))))
                         ((var ,t ,x)
                          '(block))))
                     arg* _gpu* _ptr* i*)
                  ;; Now we assign the arguments.
                  ,@(map
                     (lambda (arg _gpu _ptr i)
                       (match arg
                         ((var (ptr char) ,x)
                          (error 'compile-kernel-stmt "Don't do this (c)")
                          `(do (call void (field (var cl::kernel ,k-var)
                                                 setArg)
                                     (int ,i)
                                     (var buffer ,_gpu))))
                         ((var (vector ,t ,n) ,x)
                          `(do (call void (field (var cl::kernel ,k-var)
                                                 setArg)
                                     (int ,i)
                                     (var buffer ,_gpu))))
                         ((var ,t ,x)
                          `(do (call void (field (var cl::kernel ,k-var)
                                                 setArg)
                                     (int ,i)
                                     (var ,t ,x))))))
                     arg* _gpu* _ptr* i*)
                  ;; This is where we execute the kernel.
                  (do (call void (field (var cl::queue g_queue) execute)
                            (var cl::kernel ,k-var)
                            (int ,(match (car arg*)
                                    ((var (vector ,t ,n) ,x)
                                     n)
                                    (,else (error 'generate-kernel-calls-Stmt
                                                  "Invalid kernel argument type"
                                                  else))))
                            (int 1)))
                  ;; And now we copy the results back.
                  ,@(map
                     (lambda (arg _gpu _ptr i)
                       (match arg
                         ((var (ptr char) ,x)
                          (error 'compile-kernel-stmt "don't do this. (d)"))
                         ((var (vector ,t ,n) ,x)
                          `(block
                            (let ,_ptr cl::buffer_map<char>
                                 ;; TODO: use real types instead of buffer
                                 (call cl::buffer_map<char>
                                       (field (var cl::queue g_queue)
                                              mapBuffer<char>)
                                       (var buffer ,_gpu)))
                            ;; TODO: use a real type instead of ptr
                            (do (call void memcpy
                                      (var ptr ,(unpack-arg arg))
                                      (var ptr ,_ptr)
                                      ,(vector-bytesize `(vector ,t ,n))))))
                         ((var ,t ,x)
                          '(block))))
                     arg* _gpu* _ptr* i*)))))))
 
 (define (unpack-arg x)
   (match x
     ((var ,t ,x^) x^)
     (,x^ (guard (symbol? x^)) x^)
     (,else (error 'unpack-arg "invalid kernel argument" else))))

;; end library
 )