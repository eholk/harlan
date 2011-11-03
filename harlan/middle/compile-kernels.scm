(library
  (harlan middle compile-kernels)
  (export
    compile-kernels)
  (import
    (only (chezscheme) format)
    (rnrs)
    (elegant-weapons match)
    (util verify-grammar)
    (elegant-weapons helpers)
    (elegant-weapons print-c))
  
(define format-kernel-arg
  (lambda (arg)
    (format-arg arg)))

(define-match compile-kernel
  ((kernel ,name (,[format-kernel-arg -> args*] ...)
     . ,stmts)
   (string-append
     "__kernel void " (format-ident name)
     "(" (join ", " args*) ") "
     (format-block `(block . ,stmts)))))

(define-match unpack-type
  ((var (vector ,t ,n) ,x^) t))

(define-match compile-decl
  ((gpu-module ,[compile-kernel -> kernel*] ...)
   `(global cl::program g_prog
      ((field g_ctx createProgramFromSource)
       ,(join "\n" kernel*))))
  (,else else)) 

(define-match compile-kernels
  ((,[compile-decl -> decl*] ...) decl*)))
