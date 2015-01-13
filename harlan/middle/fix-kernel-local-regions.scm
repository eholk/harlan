(library
  (harlan middle fix-kernel-local-regions)
  (export fix-kernel-local-regions)
  (import
   (rnrs)
   (nanopass)
   (except (elegant-weapons helpers) ident?)
   (elegant-weapons print-c)
   (harlan compile-opts)
   (harlan middle languages)
   (harlan helpers)
   (util compat))

(define in-kernel? (make-parameter #f))
  
(define-pass fix-kernel-local-regions
  : M8 (m) -> M8 ()

  (Decl
   : Decl (decl) -> Decl ()
   ((gpu-module ,k* ...)
    (let ((k* (parameterize ((in-kernel? #t))
                (map Kernel k*))))
      `(gpu-module ,k* ...))))

  (Kernel : Kernel (k) -> Kernel ())
  
  (Stmt
   : Stmt (stmt) -> Stmt ()
   ((let ,x (ptr ,t0)
      (call
       (c-expr (fn (,t1) ,-> (ptr ,t2)) ,x0)
       (int ,i)))
    (if (and (in-kernel?)
             (eq? t0 'region)
             (eq? t1 'int)
             (eq? t2 'region)
             (eq? x0 'create_region))
        `(do (call (c-expr (fn ((ptr region)) -> void) declare_region)
                   (var (ptr region) ,x)))
        stmt)))))
