(library
  (harlan middle compile-kernels)
  (export compile-kernels)
  (import
    (rnrs)
    (harlan helpers)
    (elegant-weapons helpers)
    (harlan backend print-c))

(define harlan-format-kernel-arg harlan-format-arg)
  
(define-match compile-kernel
  ((kernel
     ,[harlan-format-ident -> name]
     (,[harlan-format-kernel-arg -> arg*] ...)
     ,[harlan-format-stmt -> stmt])
   (string-append
     "__kernel void " name "(" (join ", " arg*) ") " stmt)))

(define-match compile-decl
  ((gpu-module ,kernel* ...)
   (if (null? kernel*)
       '()
       `((global cl::program g_prog
                 (call
                  (field g_ctx createAndBuildProgramFromSource)
                  (str
                   ,(string-append
                     "#include \"rt/gpu_common.h\"\n\n"
                     (join "\n" (map compile-kernel kernel*)))))))))
  ((func ,type main ,args ,stmt)
   `((func ,type main ,args
           (begin
             (do (call [c-expr (() -> void) GC_INIT]))
             ,stmt))))
  (,else (list else)))

(define-match compile-kernels
  ((,decl* ...)
   (format-as-harlan
     (apply append (map compile-decl decl*)))))

;; end library
)
