(library
  (harlan middle compile-kernels)
  (export compile-kernels)
  (import
    (rnrs)
    (elegant-weapons helpers)
    (elegant-weapons print-c))
  
(define format-kernel-arg
  (lambda (arg)
    (format-arg arg)))

(define-match compile-kernel
  ((kernel
     ,[format-ident -> name]
     (,[format-kernel-arg -> args*] ...)
     ,[format-stmt -> stmt])
   (string-append
     "__kernel void " name "(" (join ", " args*) ") " stmt)))

(define-match compile-decl
  ((gpu-module ,kernel* ...)
   (if (null? kernel*)
       '()
       `((global cl::program g_prog
                 (call
                  (field g_ctx createAndBuildProgramFromSource)
                  (str
                   ,(join "\n" (map compile-kernel kernel*))))))))
  ((func ,type main ,args ,stmt)
   `((func ,type main ,args
           (begin
             (do (call [c-expr (() -> void) GC_INIT]))
             ,stmt))))
  (,else (list else)))

(define-match compile-kernels
  ((,[compile-decl -> decl*] ...) (apply append decl*)))

;; end library
)
