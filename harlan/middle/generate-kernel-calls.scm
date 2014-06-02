(library
  (harlan middle generate-kernel-calls)
  (export generate-kernel-calls)
  (import
   (rnrs)
   (nanopass)
   (except (elegant-weapons helpers) ident?)
   (elegant-weapons print-c)
   (harlan compile-opts)
   (harlan middle languages)
   (harlan helpers))

(define-pass generate-kernel-calls
  : M8 (m) -> M9 ()

  (Stmt
   : Stmt (stmt) -> Stmt ()

   ((apply-kernel ,x (,[e1*] ...) ,[e*] ...)
   (let ((kernel (gensym x))
         (region* (filter region? e*))
         (dim-names (map (lambda (_) (gensym 'dim)) e1*)))
     (let ((k-def (list `(let ,kernel cl::kernel
                              (call
                               (field (var cl::program g_prog)
                                      createKernel)
                               (str ,(run-format (mangle-ident (symbol->string x))))))))
           (arg-def (map (lambda (n d)
                           `(let ,n int ,d))
                         dim-names e1*))
           (unmaps (map (lambda (region)
                          `(do (call
                                (c-expr (fn ((ptr region)) -> void)
                                        unmap_region)
                                ,region)))
                        region*))
           (set-args
            (map (lambda (arg i)
                   `(do (call
                         (field (var cl::kernel ,kernel) setArg)
                         (int ,i)
                         ,(nanopass-case
                           (M9 Expr) arg
                           ((var (ptr ,x0) ,x) (guard (eq? x0 'region))
                            `(call
                              (c-expr (fn ((ptr region)) -> cl_mem)
                                      get_cl_buffer)
                              (var (ptr region) ,x)))
                           (else arg)))))
                 e* (iota (length e*))))
           (call-kernel
            (list (if (null? (cdr e1*))
                      `(do (call (field (var cl::queue g_queue) execute)
                                 (var cl::kernel ,kernel)
                                 (var int ,(car dim-names)) ;; global size
                                 ;;(int 1) ;; local size
                                 ))
                      (begin
                        (assert (= (length e1*) 2))
                        `(do (call (field (var cl::queue g_queue) execute2d)
                                   (var cl::kernel ,kernel)
                                   (var int ,(car dim-names)) ;; global size
                                   (var int ,(cadr dim-names))
                                   (int 1))))))))
       `(begin ,(append k-def arg-def unmaps set-args call-kernel) ...))))))

(define (region? arg)
  (nanopass-case
   (M9 Expr) arg
   ((var (ptr ,x0) ,x) (eq? x0 'region))
   (else #f))))
