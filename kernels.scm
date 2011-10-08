(library
 (kernels)
 (export compile-kernels compile-kernel hoist-kernels)
 (import (only (chezscheme) format)
         (rnrs)
         (match)
         (print-c)
	 (vectors))

 ;; This is copied from vectors.scm. It should go in its own library.
 (define gensym
   (let ((c 0))
     (lambda (x)
       (unless (symbol? x) (error 'gensym "invalid symbol" x))
       (set! c (+ 1 c))
       (string->symbol
        (string-append (symbol->string x) "_" (number->string c))))))

 (define iota
   (lambda (n)
     (let loop ([i 0])
       (cond
         [(= i n) '()]
         [else (cons i (loop (+ i 1)))]))))
 
 (define format-kernel-arg
   (lambda (arg)
     (format-arg arg)))
 
 (define compile-kernel
   (lambda (kernel)
     (match kernel
       ((kernel ,name (,[format-kernel-arg -> args*] ...)
               . ,stmts)
        (string-append
         "__kernel void " (format-ident name)
         "(" (join ", " args*) ") "
         (format-block `(block . ,stmts))))
       (,else
        (error 'compile-kernel (format "bad kernel expression: ~s" kernel))))))

 (define compile-kernel-stmt
   (lambda (stmt)
     (match stmt
       ;; This clause is really doing way too much, and should
       ;; probably be three passes just by itself. Instead, I'll
       ;; attempt to document what's going on with it.
       ;;
       ;; We have a form like (apply-kernel add_vector x y). To make
       ;; this work, we do several things. 
       ;;
       ;; First, we call g_prog.createKernel("add_vector") to create
       ;; an OpenCL kernel object.
       ;; 
       ;; Second, we allocate buffers on the GPU to hold x and y.
       ;;
       ;; Third, we copy x and y into the GPU buffers.
       ;; 
       ;; Fourth, we call setArg for each kernel argument so OpenCL
       ;; knows which parameter values to use.
       ;; 
       ;; Fifth, we actually execute the kernel.
       ;;
       ;; Sixth, we copy x and y back from the GPU.
       ;;
       ;; Each of these has more special cases, which makes the whole
       ;; process super ugly. More comments will follow below.
       ((apply-kernel ,k ,arg* ...)
        (let ((k-var (gensym 'kernel)))
          `(block
            (let ,k-var cl::kernel
                 ((field g_prog createKernel) ,(format-ident k)))            
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
                                ((field g_ctx createBuffer<char>)
                                 (hvec_byte_size ,(unpack-arg arg))
                                 CL_MEM_READ_WRITE)))
                         ((var (vector ,t ,n) ,x)
                          `(let ,_gpu (cl::buffer char)
                                ((field g_ctx createBuffer<char>)
                                 ,(vector-bytesize `(vector ,t ,n))
                                 CL_MEM_READ_WRITE)))
                         ((var ,t ,s)
                          '(block)))) ;; no-op
                     arg* _gpu* _ptr* i*)
		  ;; Here's where we copy data to the GPU.
                  ,@(map
                     (lambda (arg _gpu _ptr i)
                       (match arg
                         ((var (ptr char) ,x)
			  (error 'compile-kernel-stmt "Don't do this (b)")
                          `(block
                            (let ,_ptr cl::buffer_map<char>
                                 ((field g_queue mapBuffer<char>) ,_gpu))
                            (do (memcpy ,_ptr ,(unpack-arg arg)
                                        (hvec_byte_size ,(unpack-arg arg))))))
                         ((var (vector ,t ,n) ,x)
                          `(block
                            (let ,_ptr cl::buffer_map<char>
                                 ((field g_queue mapBuffer<char>) ,_gpu))
                            (do (memcpy ,_ptr ,(unpack-arg arg)
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
                          `(do ((field ,k-var setArg) ,i ,_gpu)))
                         ((var (vector ,t ,n) ,x)
                          `(do ((field ,k-var setArg) ,i ,_gpu)))
                         ((var ,t ,x)
                          `(do ((field ,k-var setArg) ,i ,x)))))
                     arg* _gpu* _ptr* i*)
		  ;; This is where we execute the kernel.
                  (do ((field g_queue execute) ,k-var
		       ,(match (car arg*)
			  ((var (vector ,t ,n) ,x)
			   n)
			  (,else (error 'compile-kernels
					"Invalid kernel argument type"
					else)))
		       1))
		  ;; And now we copy the results back.
                  ,@(map
                     (lambda (arg _gpu _ptr i)
                       (match arg
                         ((var (ptr char) ,x)
			  (error 'compile-kernel-stmt "don't do this. (d)")
                          `(block
                            (let ,_ptr cl::buffer_map<char>
                                 ((field g_queue mapBuffer<char>) ,_gpu))
                            (do (memcpy ,(unpack-arg arg) ,_ptr
                                        (hvec_byte_size ,(unpack-arg arg))))))
                         ((var (vector ,t ,n) ,x)
                          `(block
                            (let ,_ptr cl::buffer_map<char>
                                 ((field g_queue mapBuffer<char>) ,_gpu))
                            (do (memcpy ,(unpack-arg arg) ,_ptr
                                        ,(vector-bytesize `(vector ,t ,n))))))
                         ((var ,t ,x)
                          '(block))))
                     arg* _gpu* _ptr* i*))))))
       ((for (,i ,start ,end) ,[stmt*] ...)
        `(for (,i ,start ,end) ,stmt* ...))
       (,else else))))

 (define (unpack-arg x)
   (match x
     ((var ,t ,x^) x^)
     (,x^ (guard (symbol? x^)) x^)
     (,else (error 'unpack-arg "invalid kernel argument" else))))

 (define (unpack-type x)
   (match x
     ((var (vector ,t ,n) ,x^) t)
     (,else (error 'unpack-type "invalid kernel argument" else))))
 
 (define compile-kernels
   (lambda (mod)
     ;; (display "compiling kernels\n")
     (map (lambda (decl)
            (match decl
              ((gpu-module ,[compile-kernel -> kernel*] ...)
               `(global cl::program g_prog
                        ((field g_ctx createProgramFromSource)
                         ,(join "\n" kernel*))))
              ((func ,t ,x ,args
                     ,[compile-kernel-stmt -> stmt*] ...)
               `(func ,t ,x ,args ,stmt* ...))
              (,else else)))
          mod)))

 ;; This pass is probably too big. It finds all the kernel
 ;; expressions, hoists them into a GPU module, replaces the old
 ;; expression with an apply-kernel block, and rewrites all the
 ;; iterator variables in the body.
 (define hoist-kernels
   (lambda (mod)
     ;; (display "hoisting kernels\n")
     (match mod
       ((,[hoist-decl -> decl* kernel*] ...)
        `((gpu-module . ,(apply append kernel*))
          ,decl* ...)))))

 (define hoist-decl
   (lambda (decl)
     (match decl
       ((func ,type ,name ,args ,[hoist-stmt -> stmt* kernel*] ...)
        (values
         (if (and (eq? name 'main) (not (null? (apply append kernel*))))
             `(func ,type ,name ,args (do (GC_INIT) 
					  ((field g_prog build))) 
		    ,stmt* ...)
             `(func ,type ,name ,args (do (GC_INIT)) ,stmt* ...))
         (apply append kernel*)))
       ((extern ,t ,name ,arg-types)
        (values `(extern ,t ,name ,arg-types) '()))
       (,else (error 'hoist-decl "Invalid declaration" else)))))

 (define hoist-stmt
   (lambda (stmt)
     (match stmt
       ;; TODO: kernels are actually an expression form...
       ((kernel (((,x* ,t*) (,xs* ,ts*)) ...)
                ;; TODO: correctly handle free variables.
                (free-vars (,fv* ,ft*) ...)
                ;; TODO: What if this introduces free variables? What
                ;; about free variables in general?
                ,[hoist-stmt -> stmt* kernel*] ...)
        (let ((k-name (gensym 'kernel)))
          (values
           `(apply-kernel ,k-name ,xs* ...
                          ,@(map (lambda (x t)
                                   `(var ,t ,x))
                                 fv* ft*))
           (cons (generate-kernel k-name x* t*
                                  (map (lambda (xs) (gensym 'k_arg)) xs*)
                                  ts* fv* ft* stmt*)
                 (apply append kernel*)))))
       ((for (,i ,start ,end) ,[hoist-stmt -> stmt* kernel*] ...)
        ;;; WEB: have no idea if this is right.
        (values `(for (,i ,start ,end) ,stmt* ...) (apply append kernel*)))
       (,else (values else '())))))
 
 (define generate-kernel
   (lambda (name x* t* xs* ts* fv* ft* stmt*)
     ;; Plan of attack: replace all vectors with renamed char *'s,
     ;; then immediate use vec_deserialize. Also, for some reason the
     ;; vector refs don't seem to be being lowered like they should
     ;; be.
     ;;
     ;; We can also let-bind vars to the cell we care about, then
     ;; replace everything with a deref. That'll be cleaner.
     (let ((i (gensym 'i)))
       ;; TODO: Correctly handle free vars
       `(kernel ,name ,(append (map (lambda (x t)
                                      `(,x (ptr char)))
                                    xs* t*)
                               (map list fv* ft*))
                ;; TODO: allow this to work on n-dimensional vectors.
                (let ,i int (get_global_id 0))
                ,@(apply
                   append
                   (map
                    (lambda (x t xs)
		      `((let ,x (ptr ,t)
			     (cast (ptr ,t) (+ ,xs (* ,i (sizeof ,t)))))))
                    x* t* xs*))
                . ,(replace-vec-refs stmt* i x* xs* ts*)))))

 (define replace-vec-refs
   (lambda (stmt* i x* xs* ts*)
     (map (lambda (stmt)
            (fold-left 
             (lambda (stmt x xs ts)
               (let ((t (match ts ((vector ,t ,n) t))))
                 (match stmt
                   ((,[x] ...) `(,x ...))
                   (,y (guard (eq? x y)) `(deref ,x))
                   (,x x))))
             stmt x* xs* ts*))
          stmt*)))
 )