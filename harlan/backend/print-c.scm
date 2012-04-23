(library
  (harlan backend print-c)
  (export
    format-as-harlan
    define-with-harlan-fns
    harlan-print-c
    harlan-format-c
    harlan-format-decl
    harlan-format-stmt
    harlan-format-expr
    harlan-format-ident
    harlan-format-type
    harlan-format-arg
    harlan-format-binop
    harlan-format-relop)
  (import
    (rnrs)
    (elegant-weapons print-c)
    (elegant-weapons match)
    (elegant-weapons compat)
    (elegant-weapons helpers))

  (define (harlan-expr expr ft)
    (match expr
      ((alloc
         ,[format-expr -> region]
         ,[format-expr -> size])
       (string-append
         "alloc_in_region(" region ", " size ")"))
      ((region-ref
         ,[format-type -> type]
         ,[format-expr -> region]
         ,[format-expr -> ptr])
       (string-append "((" type ")(get_region_ptr(" region ", " ptr ")))"))
      (,else (ft else))))

  (define-match compile-kernel
    ((kernel
       ,[format-ident -> name]
       (,[format-arg -> arg*] ...)
       ,[format-stmt -> stmt])
     (string-append
       "__kernel void " name "(" (join ", " arg*) ") " stmt)))

  (define (build-kernel-programs kernel*)
    `(global cl::program g_prog
       (call
         (field g_ctx createAndBuildProgramFromSource)
         (str
           ,(string-append
              "#include \"rt/gpu_common.h\"\n\n"
              "#include \"rt/gpu_only.h\"\n\n"
              (join "\n" (map compile-kernel kernel*)))))))
  
  (define (harlan-decl decl ft)
    (match decl
      ((gpu-module ,kernel* ...)
       (if (null? kernel*) "" (ft (build-kernel-programs kernel*))))
      (,else (ft else))))
  
  (define-syntax format-as-harlan
    (syntax-rules ()
      ((_ expr ...)
       (call-with-fns ((expr-fns harlan-expr)
                       (decl-fns harlan-decl))
         (begin expr ...)))))

  (define-syntax define-with-harlan-fns
    (syntax-rules ()
      ((_ (name base-fn) ...)
       (begin
         (define (name x)
           (format-as-harlan
             (base-fn x)))
         ...))))

  (define-with-harlan-fns
    (harlan-print-c      print-c)
    (harlan-format-c     format-c)
    (harlan-format-decl  format-decl)
    (harlan-format-stmt  format-stmt)
    (harlan-format-expr  format-expr)
    (harlan-format-ident format-ident)
    (harlan-format-type  format-type)
    (harlan-format-arg   format-arg)
    (harlan-format-binop format-binop)
    (harlan-format-relop format-relop))

)

